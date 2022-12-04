{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Day3 (part1, part2) where

import Common (Impl, Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Field = Field
  { rows :: !Int,
    cols :: !Int,
    matrix :: Vector (Vector Bool)
  }
  deriving (Eq, Show)

mkField :: Text -> Field
mkField input = Field {..}
  where
    isTree = (== '#')
    matrix = Vec.fromList . fmap (Vec.fromList . map isTree . Text.unpack) . Text.lines $ input
    rows = Vec.length matrix
    cols = Vec.length . Vec.head $ matrix

part1 :: Impl
part1 input = show $ countCollisionsOnSlope 3 1 field
  where
    field = mkField input

part2 :: Impl
part2 input = show . product . fmap (\(r, d) -> countCollisionsOnSlope r d field) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    field = mkField input

countCollisionsOnSlope :: Int -> Int -> Field -> Int
countCollisionsOnSlope right down field =
  length . filter id $
    [ treeAtVirtualPoint row col field | n <- [0 .. (rows field)], let row = 0 + down * n, let col = 0 + right * n, row < rows field
    ]

treeAtVirtualPoint :: Int -> Int -> Field -> Bool
treeAtVirtualPoint row col (Field _ cols matrix) = (matrix Vec.! row) Vec.! fixedCol
  where
    fixedCol = col `mod` cols
