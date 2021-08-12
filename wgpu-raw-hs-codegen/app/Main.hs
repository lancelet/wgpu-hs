{-# LANGUAGE OverloadedStrings #-}

module Main where

import WGPU.CodeGen.Haskell (haskellApi)
import WGPU.CodeGen.Parse (readApi)
import WGPU.CodeGen.Pretty (Module (Module), export)

main :: IO ()
main = do
  putStrLn "wgpu-raw-hs-codegen"

  let wgpuPath = "./wgpu-native/ffi/wgpu.h"
      rawProjectDir = "../wgpu-raw-hs"
      modl = Module ["WGPU", "Raw", "Generated"]

  cApi <- readApi wgpuPath
  let hsApi = haskellApi cApi

  -- pPrint hsApi
  export rawProjectDir modl hsApi
