module Day1 where

import           Data.Text                      ( Text )
import           Lib                            ( readIntLines )

import           Data.List                      ( unfoldr )

problem1 :: Text -> IO ()
problem1 inputData = print totalFuelCost
  where
    input         = readIntLines inputData
    totalFuelCost = sum . map fuelCost $ input

problem2 :: Text -> IO ()
problem2 inputData = print totalFuelCost
  where
    input         = readIntLines inputData
    totalFuelCost = sum . map fuelCostWithFuel $ input

fuelCostWithFuel :: Int -> Int
fuelCostWithFuel = sum . unfoldr go . fuelCost
  where
    go v | v <= 0    = Nothing
         | otherwise = Just (v, fuelCost v)

fuelCost :: Int -> Int
fuelCost mass = floor ((fromIntegral mass :: Double) / 3.0) - 2
