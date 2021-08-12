{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUPresentMode where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUPresentMode = WGPUPresentMode Word32
  deriving (Eq, Show, Num, Storable)

pattern Immediate :: forall a. (Eq a, Num a) => a
pattern Immediate = 0x00000000

pattern Mailbox :: forall a. (Eq a, Num a) => a
pattern Mailbox = 0x00000001

pattern Fifo :: forall a. (Eq a, Num a) => a
pattern Fifo = 0x00000002