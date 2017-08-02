module Main where

import           Protolude

import qualified Spec
import           System.Environment
import           Test.Hspec.Runner

main :: IO ()
main = do
  environment <- lookupEnv "JSON_ENVIRONMENT"
  let isLocal = fromMaybe "" environment == "local"
  hspecWith
    defaultConfig
    { configQuickCheckMaxSuccess =
        if isLocal
          then Just 10000
          else Nothing
    }
    Spec.spec
