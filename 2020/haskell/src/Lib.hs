module Lib (runMain, debugAdvent) where

import Advent
  ( AoC (AoCInput, AoCSubmit),
    AoCOpts (AoCOpts),
    Part (Part1, Part2),
    SubmitRes
      ( SubCorrect,
        SubIncorrect,
        SubInvalid,
        SubUnknown,
        SubWait
      ),
    mkDay,
    runAoC,
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import System.Console.Pretty
  ( Color (Green, Red, Yellow),
    Pretty (color),
  )
import System.Environment (getEnv)

runMain :: IO ()
runMain = do
  sessionKey <- getEnv "ADVENT_SESSION_KEY"
  let opts = AoCOpts sessionKey 2020 (Just "./cache") False 3000
  traverse_ (goRunAdvent opts) impls
  where
    goRunAdvent opts (day, part, func) = runAdvent opts day part func
    impls =
      [ (1, Part1, Day1.part1),
        (1, Part2, Day1.part2),
        (2, Part1, Day2.part1),
        (2, Part2, Day2.part2),
        (3, Part1, Day3.part1),
        (3, Part2, Day3.part2),
        (4, Part1, Day4.part1),
        (4, Part2, Day4.part2),
        (5, Part1, Day5.part1),
        (5, Part2, Day5.part2)
      ]

runAdvent :: AoCOpts -> Integer -> Part -> (Text -> String) -> IO ()
runAdvent opts rawDay part func = do
  input <- runAoC opts (AoCInput day)
  let result = func (fromRight (error "Oh no! Couldn't get the input") input)
  Right (_, response) <- runAoC opts (AoCSubmit day part result)
  putStr $ "Day " ++ show rawDay ++ ", " ++ show part ++ ": "
  putStrLn $ case response of
    SubCorrect _ -> color Green "CORRECT"
    SubIncorrect _ _ -> color Red "INCORRECT"
    SubWait n -> color Yellow $ mconcat ["Must wait ", show n, " seconds before submitting again"]
    SubInvalid -> "Submission to invalid question"
    SubUnknown s -> "Unknown error: " ++ s
  where
    day = fromJust $ mkDay rawDay

debugAdvent :: Integer -> Part -> (Text -> String) -> IO String
debugAdvent rawDay _ func = do
  _ <- loadFile defaultConfig
  sessionKey <- getEnv "ADVENT_SESSION_KEY"
  let opts = AoCOpts sessionKey 2020 (Just "./cache") False 3000
  input <- runAoC opts (AoCInput day)
  let result = func (fromRight (error "Oh no! Couldn't get the input") input)
  return result
  where
    day = fromJust $ mkDay rawDay
