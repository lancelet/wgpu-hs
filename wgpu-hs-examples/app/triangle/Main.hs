{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad (unless)
import qualified SDL
import SDL.Vect (V2 (V2))
import qualified WGPU

main :: IO ()
main = do
  putStrLn "Hello Triangle"

  SDL.initialize [SDL.InitVideo]
  window <-
    SDL.createWindow
      "Triangle"
      SDL.defaultWindow {SDL.windowInitialSize = V2 640 480}

  surface <- WGPU.createSurface window
  adapter <- WGPU.requestCompatibleAdapter surface

  SDL.showWindow window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        unless quit loop
  loop

  SDL.destroyWindow window
  SDL.quit
