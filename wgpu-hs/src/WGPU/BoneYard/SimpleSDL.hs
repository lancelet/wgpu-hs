{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.BoneYard.SimpleSDL
-- Description : Template for a simple SDL app.
--
-- This is a kind of skeleton for a very simple SDL app. It is intended for
-- bootstrapping development. A common use case is when you want a window to
-- draw in with everything configured. This provides a version of that
-- functionality that can later be replaced or refined (easily) by the app
-- developer if necessary.
module WGPU.BoneYard.SimpleSDL
  ( -- * Swap Chain

    -- ** Types
    SwapChainState,

    -- ** Functions
    emptySwapChainState,
    withSwapChain,

    -- * Render Pipelines

    -- ** Types
    RenderPipelineName,
    RenderPipelines,

    -- ** Functions
    emptyRenderPipelines,
    createRenderPipeline,
    getRenderPipeline,

    -- * Shaders

    -- ** Types
    ShaderName,
    Shaders,

    -- ** Functions
    emptyShaders,
    compileWGSL,
    compileWGSL_,
    getShader,

    -- * Resources

    -- ** Types
    Params (..),
    Resources (..),

    -- ** Functions
    loadResources,

    -- * Exceptions
    AppException (..),
  )
where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Lens (lens)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Default (def)
import Data.Has (Has, getter, hasLens)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import SDL (Window)
import qualified SDL
import WGPU
  ( Adapter,
    Device,
    DeviceDescriptor,
    Instance,
    Queue,
    RenderPipeline,
    RenderPipelineDescriptor,
    SMaybe,
    ShaderModule,
    Surface,
    SwapChain,
    TextureFormat,
    WGSL,
  )
import qualified WGPU
import qualified WGPU.Classy as C
import qualified WGPU.SDL.Surface

-------------------------------------------------------------------------------
-- SwapChain Management

-- | Contains mutable state to manage the swap chain.
newtype SwapChainState = SwapChainState
  {unSwapChainState :: MVar (Maybe SwapChainDetails)}

data SwapChainDetails = SwapChainDetails
  { scdSize :: !(Word32, Word32),
    scdSwapChain :: !SwapChain
  }

-- | Initialize a new 'SwapChainState'.
emptySwapChainState :: MonadResource m => m SwapChainState
emptySwapChainState = SwapChainState <$> liftIO (newMVar Nothing)

-- | Provide a 'ReaderT' with a properly-configured 'SwapChain'.
withSwapChain ::
  forall r m a.
  ( C.HasDevice r m,
    C.HasSurface r m,
    C.HasAdapter r m,
    Has Window r,
    Has SwapChainState r
  ) =>
  ReaderT (SwapChain, r) m a ->
  m a
withSwapChain action = do
  env <- ask
  swapChain <- getSwapChain
  runReaderT action (swapChain, env)
  where
    windowSize :: m (Word32, Word32)
    windowSize = do
      SDL.V2 w h <- asks getter >>= SDL.glGetDrawableSize
      pure (fromIntegral w, fromIntegral h)

    getSwapChain :: m SwapChain
    getSwapChain = do
      device <- asks getter
      surface <- asks getter
      windowSz <- windowSize
      textureFormat <- C.getSwapChainPreferredFormat
      mVarMaybe <- asks getter
      liftIO $
        modifyMVar (unSwapChainState mVarMaybe) $ \case
          Nothing -> do
            newSwapChain device surface windowSz textureFormat
          Just scd@SwapChainDetails {..} -> do
            if scdSize == windowSz
              then pure (Just scd, scdSwapChain)
              else newSwapChain device surface windowSz textureFormat

    newSwapChain ::
      Device ->
      Surface ->
      (Word32, Word32) ->
      TextureFormat ->
      IO (Maybe SwapChainDetails, SwapChain)
    newSwapChain device surface (w, h) textureFormat = do
      swapChain <-
        WGPU.createSwapChain
          device
          surface
          WGPU.SwapChainDescriptor
            { swapChainLabel = "SwapChain",
              usage = WGPU.TextureUsageRenderAttachment,
              swapChainFormat = textureFormat,
              width = w,
              height = h,
              presentMode = WGPU.PresentModeFifo
            }
      pure (Just (SwapChainDetails (w, h) swapChain), swapChain)

-------------------------------------------------------------------------------
-- Render Pipeline Collection

-- | Name of a render pipeline.
newtype RenderPipelineName = RenderPipelineName {unRenderPipelineName :: Text}
  deriving (Eq, Ord, IsString, Show)

-- | Container for mutable state that contains a map of render pipelines.
newtype RenderPipelines = RenderPipelines
  {unRenderPipelines :: MVarMap RenderPipelineName RenderPipeline}

-- | Create an empty 'RenderPipelines'.
emptyRenderPipelines :: MonadResource m => m RenderPipelines
emptyRenderPipelines = RenderPipelines <$> emptyMVarMap

-- | Create a 'RenderPipeline', storing it in the 'RenderPipelines' map.
--
-- A 'RenderPipeline' created this way can be fetched using
-- 'getRenderPipeline'. This calls 'C.createRenderPipeline' under the hood.
createRenderPipeline ::
  ( MonadIO m,
    C.HasDevice r m,
    Has RenderPipelines r
  ) =>
  -- | Name of the render pipeline.
  RenderPipelineName ->
  -- | Descriptor of the render pipeline.
  RenderPipelineDescriptor ->
  -- | The created render pipeline.
  m RenderPipeline
createRenderPipeline name renderPipelineDescriptor = do
  renderPipeline <- C.createRenderPipeline renderPipelineDescriptor
  asks getter >>= insertMVarMap name renderPipeline . unRenderPipelines
  pure renderPipeline

-- | Fetch a render pipeline that was previously created using
-- 'createRenderPipeline'.
--
-- If the render pipeline is not available, this function throws an exception
-- of type 'AppException'.
getRenderPipeline ::
  (Has RenderPipelines r, MonadReader r m, MonadIO m, MonadThrow m) =>
  -- | Name of the render pipeline to fetch.
  RenderPipelineName ->
  -- | The render pipeline.
  m RenderPipeline
getRenderPipeline name = do
  mRenderPipeline <- asks getter >>= lookupMVarMap name . unRenderPipelines
  case mRenderPipeline of
    Just renderPipeline -> pure renderPipeline
    Nothing -> throwM (UnknownRenderPipelineName name)

-------------------------------------------------------------------------------
-- Shader Collection

-- | Name of a shader.
newtype ShaderName = ShaderName {unShaderName :: Text}
  deriving (Eq, Ord, IsString, Show)

-- | Container for mutable state that contains a map of shaders.
newtype Shaders = Shaders {unShaders :: MVarMap ShaderName ShaderModule}

-- | Create an empty 'Shaders'.
emptyShaders :: MonadResource m => m Shaders
emptyShaders = Shaders <$> emptyMVarMap

-- | Compile a WGSL shader, adding it to the 'Shaders' map.
compileWGSL_ ::
  (Has Device r, Has Shaders r, MonadReader r m, MonadResource m) =>
  -- | Name of the shader.
  ShaderName ->
  -- | Shader source code.
  WGSL ->
  -- | Action that compiles the shader and adds it to the 'Shaders' map.
  m ()
compileWGSL_ shaderName wgsl = compileWGSL shaderName wgsl >> pure ()

-- | Compile a WGSL shader, adding it to the 'Shaders' map, and returning the
-- compiled 'ShaderModule'.
compileWGSL ::
  (Has Device r, Has Shaders r, MonadReader r m, MonadResource m) =>
  -- | Name of the shader.
  ShaderName ->
  -- | Shader source code.
  WGSL ->
  -- | Action that returns the compiled shader module, after adding it to the
  -- 'Shaders' map.
  m ShaderModule
compileWGSL shaderName wgsl = do
  shaderModule <- C.createShaderModuleWGSL (unShaderName shaderName) wgsl
  asks getter >>= insertMVarMap shaderName shaderModule . unShaders
  pure shaderModule

-- | Fetch a shader that was previously compiled.
--
-- If the shader is not available, this function throws an exception of type
-- 'AppException'.
getShader ::
  (Has Shaders r, MonadReader r m, MonadIO m, MonadThrow m) =>
  -- | Name of the shader to fetch.
  ShaderName ->
  -- | The shader module.
  m ShaderModule
getShader shaderName = do
  mShaderModule <- asks getter >>= lookupMVarMap shaderName . unShaders
  case mShaderModule of
    Just shaderModule -> pure shaderModule
    Nothing -> throwM (UnknownShaderName shaderName)

-------------------------------------------------------------------------------
-- Application Static Resources

-- | Parameters for initialization.
data Params = Params
  { -- | Title of the window.
    title :: !Text,
    -- | Optional device descriptor.
    mDeviceDescriptor :: !(SMaybe DeviceDescriptor)
  }

-- | Load the resources for an application.
--
-- This creates:
--   - 'Instance',
--   - SDL 'Window' (which is shown)
--   - 'Surface' for the SDL window
--   - 'Adapter'
--   - 'Device'
--   - 'Queue'
loadResources ::
  forall m.
  (MonadResource m, MonadThrow m) =>
  -- | Initialization parameters.
  Params ->
  -- | Created application resources.
  m Resources
loadResources Params {..} = do
  inst <- createInstance
  window <- createWindow
  surface <- WGPU.SDL.Surface.createSurface inst window
  adapter <- requestAdapter surface
  device <- requestDevice adapter
  queue <- WGPU.getQueue device
  pure Resources {..}
  where
    createInstance :: m Instance
    createInstance = snd <$> WGPU.withPlatformInstance allocate

    createWindow :: m Window
    createWindow = do
      SDL.initializeAll
      let windowConfig = SDL.defaultWindow {SDL.windowResizable = True}
      snd
        <$> allocate
          (SDL.createWindow title windowConfig)
          SDL.destroyWindow

    requestAdapter :: Surface -> m Adapter
    requestAdapter surface =
      WGPU.requestAdapter surface >>= \case
        Nothing -> throwM AdapterRequestFailed
        Just adapter -> pure adapter

    requestDevice :: Adapter -> m Device
    requestDevice adapter = do
      let deviceDescriptor = WGPU.fromSMaybe def mDeviceDescriptor
      WGPU.requestDevice adapter deviceDescriptor >>= \case
        Nothing -> throwM DeviceRequestFailed
        Just device -> pure device

-- | Resources for the app.
data Resources = Resources
  { inst :: !Instance,
    window :: !Window,
    surface :: !Surface,
    adapter :: !Adapter,
    device :: !Device,
    queue :: !Queue
  }
  deriving (Generic)

instance Has Instance Resources where
  hasLens = lens inst (\s x -> s {inst = x})

instance Has Window Resources where
  hasLens = lens window (\s x -> s {window = x})

instance Has Surface Resources where
  hasLens = lens surface (\s x -> s {surface = x})

instance Has Adapter Resources where
  hasLens = lens adapter (\s x -> s {adapter = x})

instance Has Device Resources where
  hasLens = lens device (\s x -> s {device = x})

instance Has Queue Resources where
  hasLens = lens queue (\s x -> s {queue = x})

-------------------------------------------------------------------------------
-- Map inside an MVar

newtype MVarMap k v = MVarMap {unMVarMap :: MVar (Map k v)}

emptyMVarMap :: MonadIO m => m (MVarMap k v)
emptyMVarMap = MVarMap <$> liftIO (newMVar Map.empty)

insertMVarMap :: (Ord k, MonadIO m) => k -> v -> MVarMap k v -> m ()
insertMVarMap key value mVarMap =
  liftIO $ modifyMVar_ (unMVarMap mVarMap) (pure . Map.insert key value)

lookupMVarMap :: (Ord k, MonadIO m) => k -> MVarMap k v -> m (Maybe v)
lookupMVarMap key mVarMap =
  liftIO $ withMVar (unMVarMap mVarMap) (pure . Map.lookup key)

-------------------------------------------------------------------------------
-- Exceptions

-- | Exceptions from SimpleSDL.
data AppException
  = -- | Requesting an adapter failed.
    AdapterRequestFailed
  | -- | Requesting a device failed.
    DeviceRequestFailed
  | -- | Requesting a shader failed.
    UnknownShaderName ShaderName
  | -- | Requesting a render pipeline failed.
    UnknownRenderPipelineName RenderPipelineName
  deriving (Show)

instance Exception AppException
