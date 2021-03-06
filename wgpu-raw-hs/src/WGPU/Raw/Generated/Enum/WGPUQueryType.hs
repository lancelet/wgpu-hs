{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUQueryType where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUQueryType = WGPUQueryType Word32
  deriving (Eq, Show, Num, Storable)

pattern Occlusion :: forall a. (Eq a, Num a) => a
pattern Occlusion = 0x00000000

pattern PipelineStatistics :: forall a. (Eq a, Num a) => a
pattern PipelineStatistics = 0x00000001

pattern Timestamp :: forall a. (Eq a, Num a) => a
pattern Timestamp = 0x00000002
