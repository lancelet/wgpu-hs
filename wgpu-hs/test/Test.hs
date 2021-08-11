module Main (main) where

import qualified Hedgehog
import qualified WGPU.Types.DeviceExtrasTest

main :: IO ()
main = do
  runHedgehogTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ Hedgehog.checkParallel hedgehogTests
  putStrLn "---- Finished Hedgehog Tests ----"

hedgehogTests :: [Hedgehog.Group]
hedgehogTests =
  [ WGPU.Types.DeviceExtrasTest.tests
  ]
