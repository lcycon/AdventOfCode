module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Lib (runMain)

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  runMain
