{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment.XDG.BaseDir ( getUserCacheDir
                                                , getUserConfigFile
                                                )

import           Options.Generic

import           Advent                         ( AoC(AoCInput)
                                                , AoCOpts(_aCache)
                                                , Part(..)
                                                , dayInt
                                                , defaultAoCOpts
                                                , mkDay_
                                                , runAoC
                                                )

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

data Options = Options { day :: Int, problem :: Int }
    deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
    config    <- getRecord "AoC2019" :: IO Options
    tokenPath <- getUserConfigFile "aoc" "token"
    cachePath <- getUserCacheDir "aoc"
    token     <- readFile tokenPath
    let aocConfig   = (defaultAoCOpts 2019 token) { _aCache = Just cachePath }
        problemDay  = mkDay_ (toInteger $ day config)
        problemPart = case problem config of
            1 -> Part1
            2 -> Part2
            _ -> error "You picked a part that doesn't exist!"
        getProblemContents = AoCInput problemDay
    problemContentsEither <- runAoC aocConfig getProblemContents
    let problemContents = either
            (\e -> error $ "Error fetching problem input: " ++ show e)
            id
            problemContentsEither

    case (dayInt problemDay, problemPart) of
        (1 , Part1) -> Day1.problem1 problemContents
        (1 , Part2) -> Day1.problem2 problemContents

        (2 , Part1) -> Day2.problem1 problemContents
        (2 , Part2) -> Day2.problem2 problemContents

        (3 , Part1) -> Day3.problem1 problemContents
        (3 , Part2) -> Day3.problem2 problemContents

        (4 , Part1) -> Day4.problem1 problemContents
        (4 , Part2) -> Day4.problem2 problemContents

        (5 , Part1) -> Day5.problem1 problemContents
        (5 , Part2) -> Day5.problem2 problemContents

        (6 , Part1) -> Day6.problem1 problemContents
        (6 , Part2) -> Day6.problem2 problemContents

        (7 , Part1) -> Day7.problem1 problemContents
        (7 , Part2) -> Day7.problem2 problemContents

        (8 , Part1) -> Day8.problem1 problemContents
        (8 , Part2) -> Day8.problem2 problemContents

        (9 , Part1) -> Day9.problem1 problemContents
        (9 , Part2) -> Day9.problem2 problemContents

        (10, Part1) -> Day10.problem1 problemContents
        (10, Part2) -> Day10.problem2 problemContents

        (11, Part1) -> Day11.problem1 problemContents
        (11, Part2) -> Day11.problem2 problemContents

        s           -> error $ "Unknown problem/day combo: " ++ show s
