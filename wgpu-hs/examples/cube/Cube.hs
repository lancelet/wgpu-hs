{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Codec.Picture (Image, Pixel8)
import qualified Codec.Picture as Picture
import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception.Safe (MonadThrow)
import Control.Lens (Lens', lens, set, (^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Default (def)
import Data.Foldable (foldl')
import Data.Has (Has, getter, hasLens)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.String.QQ as SQQ
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word16, Word8)
import Foreign (sizeOf)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Linear (V4 (V4), (!*!), _x, _y)
import qualified Linear
import Linear.Matrix (M44)
import Linear.V2 (V2 (V2))
import Linear.V3 (V3 (V3))
import qualified SDL
import qualified WGPU
import qualified WGPU.BoneYard.SimpleSDL as SimpleSDL
import qualified WGPU.Classy as C
import Prelude hiding (putStrLn)

main :: IO ()
main = runResourceT $ do
  -- Parameters for the static resource initialization
  let params =
        SimpleSDL.Params
          { title = "Cube Example",
            mDeviceDescriptor =
              WGPU.SJust $
                def {WGPU.limits = def {WGPU.maxBindGroups = 1}}
          }

  -- Initialize the app static resources. This creates the SDL window, the
  -- WGPU Instance, etc.
  resources <- SimpleSDL.loadResources params

  -- Create other state holders
  buffers <- SimpleSDL.emptyBuffers
  textures <- SimpleSDL.emptyTextures
  bindGroups <- SimpleSDL.emptyBindGroups
  shaders <- SimpleSDL.emptyShaders
  renderPipelines <- SimpleSDL.emptyRenderPipelines
  swapChainState <- SimpleSDL.emptySwapChainState
  viewTransform <- zeroViewTransform

  -- Run the app
  runApp Env {..} app

app :: App ()
app = do
  putStrLn "== Cube Example =="
  putStrLn "- Use left-mouse-button + drag to rotate the cube."
  putStrLn "- Press 'q' or close the window to quit."

  -- Dump some debugging stuff; set the WGPU log, etc.
  C.getVersion >>= \v -> putStrLn $ "WGPU version: " <> WGPU.versionToText v
  C.getAdapterProperties >>= \p -> putStrLn $ WGPU.adapterPropertiesToText p
  C.setLogLevel WGPU.Warn
  C.connectLog

  -- Create application-specific dynamic resources
  initApp

  -- Application loop
  let appLoop :: App ()
      appLoop = do
        render
        events <- SDL.pollEvents
        mouseInteraction events
        let qPressed, windowClosed, shouldClose :: Bool
            qPressed = any eventIsQPress events
            windowClosed = any eventIsWindowClose events
            shouldClose = qPressed || windowClosed
        unless shouldClose appLoop
  appLoop

mouseInteraction :: [SDL.Event] -> App ()
mouseInteraction events = do
  -- sensitivity is a coefficient that maps screen mouse motion to radians
  let sensitivity = 0.005
  let motionEvents =
        filter (\mmed -> SDL.ButtonLeft `elem` SDL.mouseMotionEventState mmed) $
          catMaybes (mMouseMotionEvent <$> events)
  unless (null motionEvents) $ do
    let dPos =
          foldl' (+) (V2 0 0) $
            SDL.mouseMotionEventRelMotion <$> motionEvents
    let dx = sensitivity * fromIntegral (dPos ^. _x)
        dy = sensitivity * fromIntegral (dPos ^. _y)
    _ <-
      modifyViewTransform
        ( \t ->
            t
              { vtYRot = vtYRot t + dx,
                vtXRot = vtXRot t + dy
              }
        )
    updateViewTransformUniformBuffer

-- | Rendering action.
render :: App ()
render = SimpleSDL.withSwapChain $ do
  nextTexture <- C.getSwapChainCurrentTextureView
  renderPipeline <- SimpleSDL.getRenderPipeline "Render Pipeline"
  indexBuffer <- SimpleSDL.getBuffer "index"
  vertexBuffer <- SimpleSDL.getBuffer "vertex"
  bindGroup <- SimpleSDL.getBindGroup "Bind Group"
  commandBuffer <-
    C.buildCommandBuffer "Command Encoder" "Command Buffer" $ do
      let renderPassDescriptor =
            WGPU.RenderPassDescriptor
              { renderPassLabel = "Render Pass",
                colorAttachments =
                  [ WGPU.RenderPassColorAttachment
                      { colorView = nextTexture,
                        resolveTarget = WGPU.SNothing,
                        operations =
                          WGPU.Operations
                            { load =
                                WGPU.LoadOpClear
                                  (WGPU.Color 0.1 0.2 0.3 1.0),
                              store = WGPU.StoreOpStore
                            }
                      }
                  ],
                depthStencilAttachment = WGPU.SNothing
              }
      C.buildRenderPass renderPassDescriptor $ do
        C.renderPassSetPipeline renderPipeline
        C.renderPassSetBindGroup 0 bindGroup []
        C.renderPassSetIndexBuffer
          indexBuffer
          WGPU.IndexFormatUint16
          0
          (fromIntegral $ 2 * Vector.length cubeIndices)
        C.renderPassSetVertexBuffer
          0
          vertexBuffer
          0
          (fromIntegral $ sizeOf (undefined :: Vert) * Vector.length cubeVerts)
        C.renderPassDrawIndexed
          (WGPU.Range 0 (fromIntegral . Vector.length $ cubeIndices))
          0
          (WGPU.Range 0 1)
  C.queueSubmit' [commandBuffer]
  C.swapChainPresent

-- | Application initialization.
initApp :: App ()
initApp = do
  _vertexBuffer <-
    SimpleSDL.createBufferInit "vertex" (def {WGPU.bufVertex = True}) cubeVerts
  _indexBuffer <-
    SimpleSDL.createBufferInit "index" (def {WGPU.bufIndex = True}) cubeIndices

  let size = 256
      texels = createTexels (fromIntegral size)
  texture <-
    SimpleSDL.createTexture
      "texture"
      (WGPU.Extent3D size size 1)
      1
      1
      WGPU.TextureDimension2D
      WGPU.TextureFormatR8Uint
      (def {WGPU.texCopyDst = True, WGPU.texSampled = True})
  textureView <-
    WGPU.createView
      texture
      WGPU.TextureViewDescriptor
        { textureViewLabel = "Texture View",
          textureViewFormat = WGPU.TextureFormatR8Uint,
          textureViewDimension = WGPU.TextureViewDimension2D,
          textureViewBaseMipLevel = 0,
          textureViewMipLevelCount = 1,
          baseArrayLayer = 0,
          arrayLayerCount = 1,
          textureViewAspect = WGPU.TextureAspectAll
        }
  C.queueWriteTexture
    WGPU.ImageCopyTexture
      { texture = texture,
        mipLevel = 0,
        origin = WGPU.Origin3D 0 0 0,
        aspect = WGPU.TextureAspectAll
      }
    WGPU.TextureDataLayout
      { textureOffset = 0,
        bytesPerRow = size,
        rowsPerImage = 0
      }
    (WGPU.Extent3D size size 1)
    (Picture.imageData texels)

  shaderModule <- SimpleSDL.compileWGSL "shader" shaderSrc

  bindGroupLayout <-
    C.createBindGroupLayout
      WGPU.BindGroupLayoutDescriptor
        { bindGroupLayoutLabel = "Bind Group",
          layoutEntries =
            [ WGPU.BindGroupLayoutEntry
                { layoutBinding = 0,
                  visibility = def {WGPU.stageVertex = True},
                  bindGroupLayoutEntryType =
                    WGPU.BindingTypeBuffer
                      WGPU.BufferBindingLayout
                        { bindingBufferLayoutType = WGPU.Uniform,
                          hasDynamicOffset = False,
                          minBindingSize = WGPU.SJust (4 * 16)
                        }
                },
              WGPU.BindGroupLayoutEntry
                { layoutBinding = 1,
                  visibility = def {WGPU.stageFragment = True},
                  bindGroupLayoutEntryType =
                    WGPU.BindingTypeTexture
                      WGPU.TextureBindingLayout
                        { sampleType = WGPU.TextureSampleTypeUnsignedInt,
                          textureBindingViewDimension =
                            WGPU.TextureViewDimension2D,
                          multiSampled = False
                        }
                }
            ]
        }
  pipelineLayout <-
    C.createPipelineLayout
      (WGPU.PipelineLayoutDescriptor "Pipeline Layout" [bindGroupLayout])

  matrixBuf <-
    SimpleSDL.createBuffer
      "matrix"
      (fromIntegral (4 * 4 * sizeOf (undefined :: Float)))
      (def {WGPU.bufUniform = True, WGPU.bufCopyDst = True})
  updateViewTransformUniformBuffer

  _bindGroup <-
    SimpleSDL.createBindGroup
      "Bind Group"
      WGPU.BindGroupDescriptor
        { bindGroupLabel = "Bind Group",
          bindGroupLayout = bindGroupLayout,
          bindGroupEntries =
            [ -- tranformation matrix
              WGPU.BindGroupEntry
                { binding = 0,
                  resource =
                    WGPU.BindingResourceBuffer
                      (WGPU.BufferBinding matrixBuf 0 (4 * 16))
                },
              -- texture
              WGPU.BindGroupEntry
                { binding = 1,
                  resource = WGPU.BindingResourceTextureView textureView
                }
            ]
        }

  swapChainFormat <- C.getSwapChainPreferredFormat
  _renderPipeline <-
    SimpleSDL.createRenderPipeline
      "Render Pipeline"
      WGPU.RenderPipelineDescriptor
        { renderPipelineLabel = "Render Pipeline",
          layout = WGPU.SJust pipelineLayout,
          vertex =
            WGPU.VertexState
              shaderModule
              "vs_main"
              [ WGPU.VertexBufferLayout
                  (fromIntegral . sizeOf $ (undefined :: Vert))
                  WGPU.InputStepModeVertex
                  [ WGPU.VertexAttribute WGPU.VertexFormatFloat32x4 0 0,
                    WGPU.VertexAttribute WGPU.VertexFormatFloat32x2 (4 * 4) 1
                  ]
              ],
          fragment =
            WGPU.SJust $
              WGPU.FragmentState
                shaderModule
                "fs_main"
                [ WGPU.ColorTargetState
                    swapChainFormat
                    (WGPU.SJust (WGPU.BlendState def def))
                    WGPU.colorWriteMaskAll
                ],
          primitive = def {WGPU.cullMode = WGPU.CullModeBack},
          depthStencil = WGPU.SNothing,
          multisample = WGPU.MultisampleState 1 0xFFFFFFFF False -- TODO def
        }
  pure ()

-------------------------------------------------------------------------------
-- Shader source

shaderSrc :: WGPU.WGSL
shaderSrc =
  WGPU.WGSL
    [SQQ.s|
struct VertexOutput {
  [[location(0)]] tex_coord: vec2<f32>;
  [[builtin(position)]] position: vec4<f32>;
};

[[block]]
struct Locals {
  transform: mat4x4<f32>;
};
[[group(0), binding(0)]]
var r_locals: Locals;

[[stage(vertex)]]
fn vs_main(
  [[location(0)]] position: vec4<f32>,
  [[location(1)]] tex_coord: vec2<f32>
) -> VertexOutput {
  var out: VertexOutput;
  out.tex_coord = tex_coord;
  out.position = r_locals.transform * position;
  return out;
}

[[group(0), binding(1)]]
var r_color: texture_2d<u32>;

[[stage(fragment)]]
fn fs_main(in: VertexOutput) -> [[location(0)]] vec4<f32> {
  let tex = textureLoad(r_color, vec2<i32>(in.tex_coord * 256.0), 0);
  let v = f32(tex.x) / 255.0;
  return vec4<f32>(
    1.0 - (v * 5.0),
    1.0 - (v * 15.0),
    1.0 - (v * 50.0),
    1.0
  );
}

[[stage(fragment)]]
fn fs_wire() -> [[location(0)]] vec4<f32> {
  return vec4<f32>(0.0, 0.5, 0.0, 0.5);
}
|]

-------------------------------------------------------------------------------
-- Texture / Texels

-- | Quick Mandelbrot texture.
createTexels :: Int -> Image Pixel8
createTexels size = Picture.generateImage f size size
  where
    f :: Int -> Int -> Word8
    f px py =
      let cx, cy :: Float
          cx = 3.0 * fromIntegral px / fromIntegral (size - 1) - 2.0
          cy = 2.0 * fromIntegral py / fromIntegral (size - 1) - 1.0

          go :: Word8 -> Float -> Float -> Word8
          go count x y =
            let x2, y2, esc :: Float
                x2 = x * x
                y2 = y * y
                esc = x2 + y2
             in if (count < 0xFF) && (esc < 4.0)
                  then
                    let x', y' :: Float
                        x' = x2 - y2 + cx
                        y' = 2.0 * x * y + cy
                     in go (count + 1) x' y'
                  else count
       in go 0 cx cy

-------------------------------------------------------------------------------
-- Cube definition

cubeVerts :: Vector Vert
cubeVerts =
  let v :: Float -> Float -> Float -> Float -> Float -> Vert
      v x y z s t = Vert (V4 x y z 1.0) (V2 s t)
   in [ -- top (0, 0, 1)
        v -1 -1 1 0 0,
        v 1 -1 1 1 0,
        v 1 1 1 1 1,
        v -1 1 1 0 1,
        -- bottom (0, 0, -1)
        v -1 1 -1 1 0,
        v 1 1 -1 0 0,
        v 1 -1 -1 0 1,
        v -1 -1 -1 1 1,
        -- right (1, 0, 0)
        v 1 -1 -1 0 0,
        v 1 1 -1 1 0,
        v 1 1 1 1 1,
        v 1 -1 1 0 1,
        -- left (-1, 0, 0)
        v -1 -1 1 1 0,
        v -1 1 1 0 0,
        v -1 1 -1 0 1,
        v -1 -1 -1 1 1,
        -- front (0, 1, 0)
        v 1 1 -1 1 0,
        v -1 1 -1 0 0,
        v -1 1 1 0 1,
        v 1 1 1 1 1,
        -- back (0, -1, 0)
        v 1 -1 1 0 0,
        v -1 -1 1 1 0,
        v -1 -1 -1 1 1,
        v 1 -1 -1 0 1
      ]

cubeIndices :: Vector Word16
cubeIndices =
  mconcat
    [ [0, 1, 2, 2, 3, 0], -- top
      [4, 5, 6, 6, 7, 4], -- bottom
      [8, 9, 10, 10, 11, 8], -- right
      [12, 13, 14, 14, 15, 12], -- left
      [16, 17, 18, 18, 19, 16], -- front
      [20, 21, 22, 22, 23, 20] -- back
    ]

-------------------------------------------------------------------------------
-- Vertex type

data Vert = Vert
  { -- | Position of the vertex.
    vertPos :: V4 Float,
    -- | Texture coordinate of the vertex.
    vertTexCoord :: V2 Float
  }
  deriving (Eq, Show, Generic)

-- | Provides a 'Generic'-derived @Storable@ instance for 'Vert'.
instance GStorable Vert

-------------------------------------------------------------------------------
-- Application Monad

runApp ::
  Env ->
  App a ->
  ResourceT IO a
runApp env a = runReaderT (unApp a) env

-- | Application monad data type.
newtype App a = App {unApp :: ReaderT Env (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadIO,
      MonadResource,
      MonadReader Env
    )

data Env = Env
  { resources :: !SimpleSDL.Resources,
    buffers :: !SimpleSDL.Buffers,
    textures :: !SimpleSDL.Textures,
    shaders :: !SimpleSDL.Shaders,
    bindGroups :: !SimpleSDL.BindGroups,
    renderPipelines :: !SimpleSDL.RenderPipelines,
    swapChainState :: !SimpleSDL.SwapChainState,
    viewTransform :: !(MVar ViewTransform)
  }

instance Has SimpleSDL.Resources Env where
  hasLens = lens resources (\s x -> s {resources = x})

instance Has SimpleSDL.Buffers Env where
  hasLens = lens buffers (\s x -> s {buffers = x})

instance Has SimpleSDL.Textures Env where
  hasLens = lens textures (\s x -> s {textures = x})

instance Has SimpleSDL.Shaders Env where
  hasLens = lens shaders (\s x -> s {shaders = x})

instance Has SimpleSDL.BindGroups Env where
  hasLens = lens bindGroups (\s x -> s {bindGroups = x})

instance Has SimpleSDL.RenderPipelines Env where
  hasLens = lens renderPipelines (\s x -> s {renderPipelines = x})

instance Has SimpleSDL.SwapChainState Env where
  hasLens = lens swapChainState (\s x -> s {swapChainState = x})

instance Has (MVar ViewTransform) Env where
  hasLens = lens viewTransform (\s x -> s {viewTransform = x})

envResourcesL :: Lens' Env SimpleSDL.Resources
envResourcesL = hasLens

instance Has WGPU.Instance Env where hasLens = envResourcesL . hasLens

instance Has SDL.Window Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Surface Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Adapter Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Device Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Queue Env where hasLens = envResourcesL . hasLens

-------------------------------------------------------------------------------
-- Viewing Transformation Matrix

updateViewTransformUniformBuffer ::
  ( Has (MVar ViewTransform) r,
    Has SimpleSDL.Buffers r,
    Has SDL.Window r,
    Has WGPU.Queue r,
    MonadReader r m,
    MonadThrow m,
    MonadIO m
  ) =>
  m ()
updateViewTransformUniformBuffer = do
  (w, h) <- SimpleSDL.getDrawableSize
  let aspect :: Float
      aspect = fromIntegral w / fromIntegral h
  matrix <- flip generateMatrix aspect <$> getViewTransform
  matrixBuf <- SimpleSDL.getBuffer "matrix"
  C.queueWriteBuffer matrixBuf matrix

getViewTransform ::
  (Has (MVar ViewTransform) r, MonadReader r m, MonadIO m) =>
  m ViewTransform
getViewTransform = asks getter >>= liftIO . flip withMVar pure

modifyViewTransform ::
  (Has (MVar ViewTransform) r, MonadReader r m, MonadIO m) =>
  (ViewTransform -> ViewTransform) ->
  m ()
modifyViewTransform f = asks getter >>= liftIO . flip modifyMVar_ (pure . f)

data ViewTransform = ViewTransform
  { vtYRot :: !Float,
    vtXRot :: !Float,
    vtZTrans :: !Float
  }
  deriving (Eq, Show)

zeroViewTransform :: MonadIO m => m (MVar ViewTransform)
zeroViewTransform =
  liftIO . newMVar $
    ViewTransform
      { vtYRot = pi / 8,
        vtXRot = pi / 12,
        vtZTrans = 5.0
      }

generateMatrix :: ViewTransform -> Float -> M44 Float
generateMatrix ViewTransform {..} aspect =
  let rotAxisAngle :: V3 Float -> Float -> M44 Float
      rotAxisAngle axis angle = Linear.m33_to_m44 . Linear.fromQuaternion $ quat
        where
          quat = Linear.axisAngle axis angle

      proj = Linear.perspective (pi / 4.0) aspect 1.0 10.0
      rotY = rotAxisAngle (V3 0 1 0) vtYRot
      rotX = rotAxisAngle (V3 1 0 0) vtXRot
      transZ = set Linear.translation (V3 0 0 (-vtZTrans)) Linear.identity
      view = transZ !*! rotX !*! rotY
   in Linear.transpose $ openGLToWGPU !*! proj !*! view

openGLToWGPU :: M44 Float
openGLToWGPU =
  V4
    (V4 1.0 0.0 0.0 0.0)
    (V4 0.0 1.0 0.0 0.0)
    (V4 0.0 0.0 0.5 0.5)
    (V4 0.0 0.0 0.0 1.0)

-------------------------------------------------------------------------------
-- SDL Event Helpers

mKeyEvent :: SDL.Event -> Maybe SDL.KeyboardEventData
mKeyEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEventData -> Just keyboardEventData
    _ -> Nothing

mKeyPressed :: SDL.KeyboardEventData -> Maybe SDL.Keysym
mKeyPressed ed =
  case SDL.keyboardEventKeyMotion ed of
    SDL.Pressed -> Just (SDL.keyboardEventKeysym ed)
    _ -> Nothing

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  fromMaybe False $
    mKeyEvent event >>= mKeyPressed >>= \keySym -> do
      if SDL.keysymKeycode keySym == SDL.KeycodeQ
        then Just True
        else Nothing

eventIsWindowClose :: SDL.Event -> Bool
eventIsWindowClose event =
  case SDL.eventPayload event of
    SDL.WindowClosedEvent _ -> True
    _ -> False

mMouseMotionEvent :: SDL.Event -> Maybe SDL.MouseMotionEventData
mMouseMotionEvent event =
  case SDL.eventPayload event of
    SDL.MouseMotionEvent mouseMotionEventData -> Just mouseMotionEventData
    _ -> Nothing

-------------------------------------------------------------------------------
-- Miscellaneous

-- | Print text to the console.
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TextIO.putStrLn
