{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module WGPU.Types.DeviceExtrasTest (tests) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Hedgehog (Gen, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified WGPU.CStruct as CStruct
import WGPU.Types.DeviceExtras (DeviceExtras)
import qualified WGPU.Types.DeviceExtras as DeviceExtras
import qualified WGPU.Types.NativeFeature as NativeFeature

tests :: H.Group
tests =
  H.Group
    "WGPU.Types.DeviceExtras"
    [ ("CStruct DeviceExtras round-trip", prop_CStruct_DeviceExtras_roundtrip)
    ]

prop_CStruct_DeviceExtras_roundtrip :: H.Property
prop_CStruct_DeviceExtras_roundtrip =
  H.property $ do
    deviceExtras <- H.forAll gen_DeviceExtras
    readDeviceExtras <-
      liftIO $ CStruct.withCStruct deviceExtras $ CStruct.peekCStruct
    deviceExtras === readDeviceExtras

gen_DeviceExtras :: Gen (DeviceExtras '[])
gen_DeviceExtras = do
  let chain = ()
  let gr x = Gen.integral (Range.linear 0 x)
  maxTextureDimension1D <- gr 1024
  maxTextureDimension2D <- gr 1024
  maxTextureDimension3D <- gr 1024
  maxBindGroups <- gr 10
  maxDynamicStorageBuffersPerPipelineLayout <- gr 256
  maxStorageBuffersPerShaderStage <- gr 512
  maxStorageBufferBindingSize <- gr 65535
  nativeFeatures <- NativeFeature.NativeFeature <$> gr 5
  label <- Gen.text (Range.linear 0 256) Gen.alpha
  tracePath <- Gen.text (Range.linear 0 256) Gen.alpha
  pure $ DeviceExtras.DeviceExtras {..}
