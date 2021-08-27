{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception.Safe (MonadThrow)
import Control.Lens (Lens', lens)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Default (def)
import Data.Has (Has, hasLens)
import Data.Maybe (fromMaybe)
import qualified Data.String.QQ as SQQ
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified SDL
import qualified WGPU
import qualified WGPU.BoneYard.SimpleSDL as SimpleSDL
import qualified WGPU.Classy as C
import Prelude hiding (putStrLn)

main :: IO ()
main = runResourceT $ do
  putStrLn "Start"

  -- Parameters for the static resource initialization
  let params =
        SimpleSDL.Params
          { title = "SDL Triangle Example",
            mDeviceDescriptor =
              WGPU.SJust $
                def {WGPU.limits = def {WGPU.maxBindGroups = 1}}
          }

  -- Initialize the app static resources. This creates the SDL window, the
  -- WGPU Instance, etc.
  resources <- SimpleSDL.loadResources params

  -- Create other state holders
  shaders <- SimpleSDL.emptyShaders
  renderPipelines <- SimpleSDL.emptyRenderPipelines
  swapChainState <- SimpleSDL.emptySwapChainState

  -- Run the app
  runApp Env {..} app

-- | The core application.
app :: App ()
app = do
  putStrLn "SDL Triangle Example"

  -- Dump some debugging stuff; set the WGPU log, etc.
  C.getVersion >>= \v -> putStrLn $ "WGPU version: " <> WGPU.versionToText v
  C.getAdapterProperties >>= \p -> putStrLn $ WGPU.adapterPropertiesToText p
  C.setLogLevel WGPU.Warn
  C.connectLog

  -- Create application-specific dynamic resources: shader and pipeline.
  initApp

  -- Application loop
  let appLoop :: App ()
      appLoop = do
        render
        events <- SDL.pollEvents
        let qPressed, windowClosed, shouldClose :: Bool
            qPressed = any eventIsQPress events
            windowClosed = any eventIsWindowClose events
            shouldClose = qPressed || windowClosed
        unless shouldClose appLoop
  appLoop

-- | The rendering action.
render :: App ()
render = do
  SimpleSDL.withSwapChain $ do
    nextTexture <- C.getSwapChainCurrentTextureView
    renderPipeline <- SimpleSDL.getRenderPipeline "renderPipeline"
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
                              { load = WGPU.LoadOpClear (WGPU.Color 0 0 0 1),
                                store = WGPU.StoreOpStore
                              }
                        }
                    ],
                  depthStencilAttachment = WGPU.SNothing
                }
        C.buildRenderPass renderPassDescriptor $ do
          C.renderPassSetPipeline renderPipeline
          C.renderPassDraw (WGPU.Range 0 3) (WGPU.Range 0 1)
    C.queueSubmit' [commandBuffer]
    C.swapChainPresent

-- | Application initialization
initApp :: App ()
initApp = do
  shaderModule <- SimpleSDL.compileWGSL "shader" shaderSrc

  let pipelineLayoutDescriptor = WGPU.PipelineLayoutDescriptor "Pipeline" []
  pipelineLayout <- C.createPipelineLayout pipelineLayoutDescriptor

  swapChainFormat <- C.getSwapChainPreferredFormat
  let renderPipelineDescriptor =
        WGPU.RenderPipelineDescriptor
          { renderPipelineLabel = "Render Pipeline",
            layout = WGPU.SJust pipelineLayout,
            vertex = WGPU.VertexState shaderModule "vs_main" [],
            primitive = def,
            depthStencil = WGPU.SNothing,
            multisample = WGPU.MultisampleState 1 0xFFFFFFFF False,
            fragment =
              WGPU.SJust $
                WGPU.FragmentState
                  shaderModule
                  "fs_main"
                  [ WGPU.ColorTargetState
                      swapChainFormat
                      (WGPU.SJust (WGPU.BlendState def def))
                      WGPU.colorWriteMaskAll
                  ]
          }
  _ <- SimpleSDL.createRenderPipeline "renderPipeline" renderPipelineDescriptor
  pure ()

shaderSrc :: WGPU.WGSL
shaderSrc =
  WGPU.WGSL
    [SQQ.s|
[[stage(vertex)]]
fn vs_main([[builtin(vertex_index)]] in_vertex_index: u32) -> [[builtin(position)]] vec4<f32> {
  let x = f32(i32(in_vertex_index) - 1);
  let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
  return vec4<f32>(x, y, 0.0, 1.0);
}

[[stage(fragment)]]
fn fs_main([[builtin(position)]] in: vec4<f32>) -> [[location(0)]] vec4<f32> {
  return vec4<f32>(in.x/640.0, in.y/480.0, 1.0, 1.0);
}
|]

-------------------------------------------------------------------------------
-- Application Monad

-- | Run the application.
runApp ::
  -- | Environment for the application. This contains both static resources and
  -- 'MVar's that contain dynamic resources.
  Env ->
  -- | Application to run.
  App a ->
  -- | Resolved 'ResourceT' action. 'ResourceT' is used here so that portions of
  -- the app can be cleaned up automatically.
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

-- | Environment for the application.
--
-- The environment stores both static and dynamic data that the application
-- uses. This type can be customized at-will to add additional state, such as
-- game state or vizualisation state.
--
-- The 'Has' type class instances below describe how to access the components
-- of the environment that are required for the various operations.
data Env = Env
  { -- | Static resources (Instance, Window, Surface, Adapter, Device, Queue).
    resources :: !SimpleSDL.Resources,
    -- | Map of shaders. Shaders can be added to this map by compiling them,
    -- and then accessed as required. (See the 'SimpleSDL' module.)
    shaders :: !SimpleSDL.Shaders,
    -- | Map of render pipelines. Different render pipelines can be added here
    -- and then accessed as required. (See the 'SimpleSDL' module.)
    renderPipelines :: !SimpleSDL.RenderPipelines,
    -- | Swap chain state. When the app window is re-sized, the swap chain must
    -- be re-created. This type contains the current swap chain state.
    swapChainState :: !SimpleSDL.SwapChainState
  }

instance Has SimpleSDL.Resources Env where
  hasLens = lens resources (\s x -> s {resources = x})

instance Has SimpleSDL.Shaders Env where
  hasLens = lens shaders (\s x -> s {shaders = x})

instance Has SimpleSDL.RenderPipelines Env where
  hasLens = lens renderPipelines (\s x -> s {renderPipelines = x})

instance Has SimpleSDL.SwapChainState Env where
  hasLens = lens swapChainState (\s x -> s {swapChainState = x})

envResourcesL :: Lens' Env SimpleSDL.Resources
envResourcesL = hasLens

instance Has WGPU.Instance Env where hasLens = envResourcesL . hasLens

instance Has SDL.Window Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Surface Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Adapter Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Device Env where hasLens = envResourcesL . hasLens

instance Has WGPU.Queue Env where hasLens = envResourcesL . hasLens

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

-------------------------------------------------------------------------------
-- Miscellaneous

-- | Print text to the console.
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TextIO.putStrLn
