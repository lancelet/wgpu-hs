{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Shader
-- Description : Shader modules.
module WGPU.Internal.Shader
  ( -- * Types
    ShaderModule,
    ShaderModuleDescriptor (..),
    ShaderSource (..),
    SPIRV (..),
    WGSL (..),
    ShaderEntryPoint (..),

    -- * Functions
    createShaderModule,
    createShaderModuleSPIRV,
    createShaderModuleWGSL,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Foreign (Ptr, castPtr, sizeOf)
import Foreign.C (CChar)
import WGPU.Internal.ChainedStruct (ChainedStruct (EmptyChain, PtrChain))
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, ToRawPtr, raw, rawPtr, showWithPtr)
import qualified WGPU.Raw.Generated.Enum.WGPUSType as WGPUSType
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUShaderModuleDescriptor (WGPUShaderModuleDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUShaderModuleDescriptor as WGPUShaderModuleDescriptor
import WGPU.Raw.Generated.Struct.WGPUShaderModuleSPIRVDescriptor (WGPUShaderModuleSPIRVDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUShaderModuleSPIRVDescriptor as WGPUShaderModuleSPIRVDescriptor
import WGPU.Raw.Generated.Struct.WGPUShaderModuleWGSLDescriptor (WGPUShaderModuleWGSLDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUShaderModuleWGSLDescriptor as WGPUShaderModuleWGSLDescriptor
import WGPU.Raw.Types (WGPUShaderModule (WGPUShaderModule))

-------------------------------------------------------------------------------

-- | Handle to a compiled shader module.
newtype ShaderModule = ShaderModule {wgpuShaderModule :: WGPUShaderModule}

instance Show ShaderModule where
  show m =
    let ShaderModule (WGPUShaderModule ptr) = m
     in showWithPtr "ShaderModule" ptr

instance Eq ShaderModule where
  (==) m1 m2 =
    let ShaderModule (WGPUShaderModule m1_ptr) = m1
        ShaderModule (WGPUShaderModule m2_ptr) = m2
     in m1_ptr == m2_ptr

instance ToRaw ShaderModule WGPUShaderModule where
  raw = pure . wgpuShaderModule

-------------------------------------------------------------------------------

-- | Create a shader module from either SPIR-V or WGSL source code.
createShaderModule ::
  -- | Device for the shader.
  Device ->
  -- | Descriptor of the shader module.
  ShaderModuleDescriptor ->
  -- | IO action producing the shader module.
  IO ShaderModule
createShaderModule device smd = evalContT $ do
  let inst :: Instance
      inst = deviceInst device
  shaderModuleDescriptor_ptr <- rawPtr smd
  rawShaderModule <-
    liftIO $
      RawFun.wgpuDeviceCreateShaderModule
        (wgpuHsInstance inst)
        (wgpuDevice device)
        shaderModuleDescriptor_ptr
  pure (ShaderModule rawShaderModule)

-- | Create a shader module from SPIR-V source code.
createShaderModuleSPIRV ::
  -- | Device for which the shader should be created.
  Device ->
  -- | Debugging label for the shader.
  Text ->
  -- | Shader source code (SPIR-V bytestring).
  SPIRV ->
  -- | IO action creating the shader module.
  IO ShaderModule
createShaderModuleSPIRV device label spirv =
  createShaderModule device smd
  where
    smd :: ShaderModuleDescriptor
    smd =
      ShaderModuleDescriptor
        { shaderLabel = label,
          source = ShaderSourceSPIRV spirv
        }

-- | Create a shader module from WGSL source code.
createShaderModuleWGSL ::
  -- | Device for which the shader should be created.
  Device ->
  -- | Debugging label for the shader.
  Text ->
  -- | Shader source code (WGSL source string).
  WGSL ->
  -- | IO action creating the shader module.
  IO ShaderModule
createShaderModuleWGSL device label wgsl =
  createShaderModule device smd
  where
    smd :: ShaderModuleDescriptor
    smd =
      ShaderModuleDescriptor
        { shaderLabel = label,
          source = ShaderSourceWGSL wgsl
        }

-------------------------------------------------------------------------------

-- | Descriptor for a shader module.
data ShaderModuleDescriptor = ShaderModuleDescriptor
  { -- | Debug label of the shader module.
    shaderLabel :: !Text,
    -- | Source code for the shader.
    source :: !ShaderSource
  }
  deriving (Eq, Show)

instance ToRaw ShaderModuleDescriptor WGPUShaderModuleDescriptor where
  raw ShaderModuleDescriptor {..} = do
    nextInChain_ptr <-
      case source of
        ShaderSourceSPIRV spirv -> do
          ptr <- rawPtr spirv
          rawPtr (PtrChain WGPUSType.ShaderModuleSPIRVDescriptor ptr)
        ShaderSourceWGSL wgsl -> do
          ptr <- rawPtr wgsl
          rawPtr (PtrChain WGPUSType.ShaderModuleWGSLDescriptor ptr)
    label_ptr <- rawPtr shaderLabel
    pure
      WGPUShaderModuleDescriptor.WGPUShaderModuleDescriptor
        { nextInChain = nextInChain_ptr,
          label = label_ptr
        }

-------------------------------------------------------------------------------

-- | Source for a shader module.
data ShaderSource
  = -- | Use shader source from a SPIRV module (pre-compiled).
    ShaderSourceSPIRV !SPIRV
  | -- | Use shader source from WGSL string.
    ShaderSourceWGSL !WGSL
  deriving (Eq, Show)

-- | Pre-compiled SPIRV module bytes.
newtype SPIRV = SPIRV ByteString deriving (Eq, Show)

instance ToRaw SPIRV WGPUShaderModuleSPIRVDescriptor where
  raw (SPIRV bs) =
    WGPUShaderModuleSPIRVDescriptor.WGPUShaderModuleSPIRVDescriptor
      <$> raw (EmptyChain WGPUSType.ShaderModuleSPIRVDescriptor)
      <*> pure
        ( fromIntegral $
            ByteString.length bs `div` sizeOf (undefined :: Word32)
        )
      <*> ((castPtr :: Ptr Word8 -> Ptr Word32) <$> rawPtr bs)

-- | WGSL shader source code.
newtype WGSL = WGSL Text deriving (Eq, Show)

instance ToRaw WGSL WGPUShaderModuleWGSLDescriptor where
  raw (WGSL txt) =
    WGPUShaderModuleWGSLDescriptor.WGPUShaderModuleWGSLDescriptor
      <$> raw (EmptyChain WGPUSType.ShaderModuleWGSLDescriptor)
      <*> rawPtr txt

-------------------------------------------------------------------------------

-- | Name of a shader entry point.
newtype ShaderEntryPoint = ShaderEntryPoint {unShaderEntryPoint :: Text}
  deriving (Eq, Show, IsString)

instance ToRawPtr ShaderEntryPoint CChar where
  rawPtr = rawPtr . unShaderEntryPoint
