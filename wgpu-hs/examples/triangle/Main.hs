{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Let's draw a triangle!
module Main (main) where

import Control.Concurrent
  ( MVar,
    newEmptyMVar,
    putMVar,
    tryReadMVar,
    tryTakeMVar,
  )
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Default (def)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitFailure)
import WGPU (SwapChain)
import qualified WGPU

main :: IO ()
main = do
  TextIO.putStrLn "Triangle Example"

  -- start GLFW
  initResult <- GLFW.init
  unless initResult $ do
    TextIO.putStrLn "Failed to initialize GLFW"
    exitFailure

  -- create the GLFW window without a "client API"
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  window <- do
    mWin <- GLFW.createWindow 640 480 "Triangle" Nothing Nothing
    case mWin of
      Just w -> pure w
      Nothing -> do
        TextIO.putStrLn "Failed to create GLFW window"
        exitFailure

  WGPU.withPlatformInstance bracket $ \inst -> do
    -- attach the logger and set the logging level
    WGPU.connectLog inst
    WGPU.setLogLevel inst WGPU.Warn

    -- print the version of the WGPU library
    version <- WGPU.getVersion inst
    TextIO.putStrLn $ "WGPU version: " <> WGPU.versionToText version

    -- fetch resources (surface, adpater, device)
    Resources {..} <- getResources inst window >>= getOrFail

    shader <- WGPU.createShaderModuleWGSL device "shader" shaderSrc
    swapChainFormat <- WGPU.getSwapChainPreferredFormat surface adapter
    swapChainMVar <- newEmptyMVar
    _ <- updateSwapChain device surface window swapChainFormat swapChainMVar

    pipelineLayout <-
      WGPU.createPipelineLayout
        device
        (WGPU.PipelineLayoutDescriptor "Pipeline" [])
    pipeline <-
      WGPU.createRenderPipeline
        device
        WGPU.RenderPipelineDescriptor
          { renderPipelineLabel = "Render Pipeline",
            layout = WGPU.SJust pipelineLayout,
            vertex = WGPU.VertexState shader "vs_main" [],
            primitive = def,
            depthStencil = WGPU.SNothing,
            multisample = WGPU.MultisampleState 1 0xFFFFFFFF False,
            fragment =
              WGPU.SJust $
                WGPU.FragmentState
                  shader
                  "fs_main"
                  [ WGPU.ColorTargetState
                      swapChainFormat
                      (WGPU.SJust (WGPU.BlendState def def))
                      WGPU.colorWriteMaskAll
                  ]
          }

    let doDraw =
          draw
            device
            surface
            window
            pipeline
            queue
            swapChainFormat
            swapChainMVar

    let loop = do
          doDraw

          -- handle GLFW quit event
          GLFW.pollEvents
          shouldClose <- GLFW.windowShouldClose window
          unless shouldClose loop

    loop

  -- close down GLFW
  GLFW.destroyWindow window
  GLFW.terminate

newtype Error = Error Text deriving (Eq, Show)

getOrFail :: Either Error a -> IO a
getOrFail ma =
  case ma of
    Right x -> pure x
    Left err -> failWith err

failWith :: Error -> IO a
failWith (Error err) = do
  TextIO.putStrLn err
  exitFailure

data Resources = Resources
  { surface :: WGPU.Surface,
    adapter :: WGPU.Adapter,
    device :: WGPU.Device,
    queue :: WGPU.Queue
  }

getResources :: WGPU.Instance -> GLFW.Window -> IO (Either Error Resources)
getResources inst window = runExceptT $ do
  -- fetch a surface for the window
  surface <- lift $ WGPU.createGLFWSurface inst window
  -- fetch an adapter for the surface
  adapter <-
    maybeToExceptT
      (Error "Failed to obtain WGPU Adapter")
      (MaybeT $ WGPU.requestAdapter surface)
  -- fetch a device for the adapter
  let deviceDescriptor :: WGPU.DeviceDescriptor
      deviceDescriptor = def {WGPU.limits = def {WGPU.maxBindGroups = 1}}
  device <-
    maybeToExceptT
      (Error "Failed to obtain WGPU Device")
      (MaybeT $ WGPU.requestDevice adapter deviceDescriptor)

  queue <- lift $ WGPU.getQueue device

  pure Resources {..}

updateSwapChain ::
  WGPU.Device ->
  WGPU.Surface ->
  GLFW.Window ->
  WGPU.TextureFormat ->
  MVar ((Int, Int), WGPU.SwapChain) ->
  IO WGPU.SwapChain
updateSwapChain device surface window textureFormat swapChainMVar = do
  mOldSwapChain <- fmap snd <$> tryReadMVar swapChainMVar
  oldSz <- maybe (0, 0) fst <$> tryReadMVar swapChainMVar
  curSz <- GLFW.getWindowSize window
  case (mOldSwapChain, curSz /= oldSz) of
    x | isNothing (fst x) || snd x -> do
      swapChain <-
        WGPU.createSwapChain
          device
          surface
          WGPU.SwapChainDescriptor
            { swapChainLabel = "SwapChain",
              usage = WGPU.TextureUsageRenderAttachment,
              swapChainFormat = textureFormat,
              width = fromIntegral . fst $ curSz,
              height = fromIntegral . snd $ curSz,
              presentMode = WGPU.PresentModeFifo
            }
      _ <- tryTakeMVar swapChainMVar
      putMVar swapChainMVar (curSz, swapChain)
      pure swapChain
    (Just swapChain, False) -> pure swapChain

draw ::
  WGPU.Device ->
  WGPU.Surface ->
  GLFW.Window ->
  WGPU.RenderPipeline ->
  WGPU.Queue ->
  WGPU.TextureFormat ->
  MVar ((Int, Int), WGPU.SwapChain) ->
  IO ()
draw device surface window pipeline queue swapChainFormat swapChainMVar = do
  -- update swapchain if the window size is different
  swapChain <-
    updateSwapChain device surface window swapChainFormat swapChainMVar
  -- render
  nextTexture <- WGPU.getSwapChainCurrentTextureView swapChain
  encoder <- WGPU.createCommandEncoder device "Command Encoder"
  renderPass <-
    WGPU.beginRenderPass
      encoder
      ( WGPU.RenderPassDescriptor
          { renderPassLabel = "Render Pass",
            colorAttachments =
              [ WGPU.RenderPassColorAttachment
                  nextTexture
                  WGPU.SNothing
                  ( WGPU.Operations
                      (WGPU.LoadOpClear (WGPU.Color 0 0 0 1))
                      WGPU.StoreOpStore
                  )
              ],
            depthStencilAttachment = WGPU.SNothing
          }
      )
  WGPU.renderPassSetPipeline renderPass pipeline
  WGPU.renderPassDraw renderPass (WGPU.Range 0 3) (WGPU.Range 0 1)
  WGPU.endRenderPass renderPass
  commandBuffer <- WGPU.commandEncoderFinish encoder "Command Buffer"
  WGPU.queueSubmit queue [commandBuffer]
  WGPU.swapChainPresent swapChain

shaderSrc :: WGPU.WGSL
shaderSrc =
  WGPU.WGSL $
    Text.intercalate
      "\n"
      [ "[[stage(vertex)]]",
        "fn vs_main([[builtin(vertex_index)]] in_vertex_index: u32) -> [[builtin(position)]] vec4<f32> {",
        "  let x = f32(i32(in_vertex_index) - 1);",
        "  let y = f32(i32(in_vertex_index & 1u) * 2 - 1);",
        "  return vec4<f32>(x, y, 0.0, 1.0);",
        "}",
        "",
        "[[stage(fragment)]]",
        "fn fs_main([[builtin(position)]] in: vec4<f32>) -> [[location(0)]] vec4<f32> {",
        "  return vec4<f32>(in.x/640.0, in.y/480.0, 1.0, 1.0);",
        "}"
      ]
