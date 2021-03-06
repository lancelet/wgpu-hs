{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUTextureFormat where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUTextureFormat = WGPUTextureFormat Word32
  deriving (Eq, Show, Num, Storable)

pattern Undefined :: forall a. (Eq a, Num a) => a
pattern Undefined = 0x00000000

pattern R8Unorm :: forall a. (Eq a, Num a) => a
pattern R8Unorm = 0x00000001

pattern R8Snorm :: forall a. (Eq a, Num a) => a
pattern R8Snorm = 0x00000002

pattern R8Uint :: forall a. (Eq a, Num a) => a
pattern R8Uint = 0x00000003

pattern R8Sint :: forall a. (Eq a, Num a) => a
pattern R8Sint = 0x00000004

pattern R16Uint :: forall a. (Eq a, Num a) => a
pattern R16Uint = 0x00000005

pattern R16Sint :: forall a. (Eq a, Num a) => a
pattern R16Sint = 0x00000006

pattern R16Float :: forall a. (Eq a, Num a) => a
pattern R16Float = 0x00000007

pattern RG8Unorm :: forall a. (Eq a, Num a) => a
pattern RG8Unorm = 0x00000008

pattern RG8Snorm :: forall a. (Eq a, Num a) => a
pattern RG8Snorm = 0x00000009

pattern RG8Uint :: forall a. (Eq a, Num a) => a
pattern RG8Uint = 0x0000000A

pattern RG8Sint :: forall a. (Eq a, Num a) => a
pattern RG8Sint = 0x0000000B

pattern R32Float :: forall a. (Eq a, Num a) => a
pattern R32Float = 0x0000000C

pattern R32Uint :: forall a. (Eq a, Num a) => a
pattern R32Uint = 0x0000000D

pattern R32Sint :: forall a. (Eq a, Num a) => a
pattern R32Sint = 0x0000000E

pattern RG16Uint :: forall a. (Eq a, Num a) => a
pattern RG16Uint = 0x0000000F

pattern RG16Sint :: forall a. (Eq a, Num a) => a
pattern RG16Sint = 0x00000010

pattern RG16Float :: forall a. (Eq a, Num a) => a
pattern RG16Float = 0x00000011

pattern RGBA8Unorm :: forall a. (Eq a, Num a) => a
pattern RGBA8Unorm = 0x00000012

pattern RGBA8UnormSrgb :: forall a. (Eq a, Num a) => a
pattern RGBA8UnormSrgb = 0x00000013

pattern RGBA8Snorm :: forall a. (Eq a, Num a) => a
pattern RGBA8Snorm = 0x00000014

pattern RGBA8Uint :: forall a. (Eq a, Num a) => a
pattern RGBA8Uint = 0x00000015

pattern RGBA8Sint :: forall a. (Eq a, Num a) => a
pattern RGBA8Sint = 0x00000016

pattern BGRA8Unorm :: forall a. (Eq a, Num a) => a
pattern BGRA8Unorm = 0x00000017

pattern BGRA8UnormSrgb :: forall a. (Eq a, Num a) => a
pattern BGRA8UnormSrgb = 0x00000018

pattern RGB10A2Unorm :: forall a. (Eq a, Num a) => a
pattern RGB10A2Unorm = 0x00000019

pattern RG11B10Ufloat :: forall a. (Eq a, Num a) => a
pattern RG11B10Ufloat = 0x0000001A

pattern RGB9E5Ufloat :: forall a. (Eq a, Num a) => a
pattern RGB9E5Ufloat = 0x0000001B

pattern RG32Float :: forall a. (Eq a, Num a) => a
pattern RG32Float = 0x0000001C

pattern RG32Uint :: forall a. (Eq a, Num a) => a
pattern RG32Uint = 0x0000001D

pattern RG32Sint :: forall a. (Eq a, Num a) => a
pattern RG32Sint = 0x0000001E

pattern RGBA16Uint :: forall a. (Eq a, Num a) => a
pattern RGBA16Uint = 0x0000001F

pattern RGBA16Sint :: forall a. (Eq a, Num a) => a
pattern RGBA16Sint = 0x00000020

pattern RGBA16Float :: forall a. (Eq a, Num a) => a
pattern RGBA16Float = 0x00000021

pattern RGBA32Float :: forall a. (Eq a, Num a) => a
pattern RGBA32Float = 0x00000022

pattern RGBA32Uint :: forall a. (Eq a, Num a) => a
pattern RGBA32Uint = 0x00000023

pattern RGBA32Sint :: forall a. (Eq a, Num a) => a
pattern RGBA32Sint = 0x00000024

pattern Depth32Float :: forall a. (Eq a, Num a) => a
pattern Depth32Float = 0x00000025

pattern Depth24Plus :: forall a. (Eq a, Num a) => a
pattern Depth24Plus = 0x00000026

pattern Depth24PlusStencil8 :: forall a. (Eq a, Num a) => a
pattern Depth24PlusStencil8 = 0x00000027

pattern Stencil8 :: forall a. (Eq a, Num a) => a
pattern Stencil8 = 0x00000028

pattern BC1RGBAUnorm :: forall a. (Eq a, Num a) => a
pattern BC1RGBAUnorm = 0x00000029

pattern BC1RGBAUnormSrgb :: forall a. (Eq a, Num a) => a
pattern BC1RGBAUnormSrgb = 0x0000002A

pattern BC2RGBAUnorm :: forall a. (Eq a, Num a) => a
pattern BC2RGBAUnorm = 0x0000002B

pattern BC2RGBAUnormSrgb :: forall a. (Eq a, Num a) => a
pattern BC2RGBAUnormSrgb = 0x0000002C

pattern BC3RGBAUnorm :: forall a. (Eq a, Num a) => a
pattern BC3RGBAUnorm = 0x0000002D

pattern BC3RGBAUnormSrgb :: forall a. (Eq a, Num a) => a
pattern BC3RGBAUnormSrgb = 0x0000002E

pattern BC4RUnorm :: forall a. (Eq a, Num a) => a
pattern BC4RUnorm = 0x0000002F

pattern BC4RSnorm :: forall a. (Eq a, Num a) => a
pattern BC4RSnorm = 0x00000030

pattern BC5RGUnorm :: forall a. (Eq a, Num a) => a
pattern BC5RGUnorm = 0x00000031

pattern BC5RGSnorm :: forall a. (Eq a, Num a) => a
pattern BC5RGSnorm = 0x00000032

pattern BC6HRGBUfloat :: forall a. (Eq a, Num a) => a
pattern BC6HRGBUfloat = 0x00000033

pattern BC6HRGBFloat :: forall a. (Eq a, Num a) => a
pattern BC6HRGBFloat = 0x00000034

pattern BC7RGBAUnorm :: forall a. (Eq a, Num a) => a
pattern BC7RGBAUnorm = 0x00000035

pattern BC7RGBAUnormSrgb :: forall a. (Eq a, Num a) => a
pattern BC7RGBAUnormSrgb = 0x00000036
