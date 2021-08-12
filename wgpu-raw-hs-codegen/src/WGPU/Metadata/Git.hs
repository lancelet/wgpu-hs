{-# LANGUAGE OverloadedStrings #-}

-- | Obtain simple git submodule information to dump in the output.
module WGPU.Metadata.Git
  ( -- * Functions
    getWGPUSubmoduleString,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Process (readProcess)

-- | Fetch the string output for @wgpu-native@ from @git submodule status@.
getWGPUSubmoduleString :: IO Text
getWGPUSubmoduleString = do
  raw <- Text.pack <$> readProcess "git" ["submodule", "status"] ""
  let rawLines = Text.lines raw
      wlines = filter (Text.isInfixOf "wgpu-native") rawLines
  when (length wlines /= 1) $ do
    error $
      "No unique wgpu-line was found in git submodule status: "
        <> Text.unpack raw
  pure $ Text.strip (head wlines)
