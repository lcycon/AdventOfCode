module Day.Day4 where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix {dat :: Vector a, width :: Int, height :: Int}

matrixCharAt :: Int -> Int -> Matrix a -> Maybe a
matrixCharAt row col m = if row >= m.height || col >= m.width || row < 0 || col < 0 then Nothing else m.dat V.!? (row * m.width + col)

buildMatrixFromInput :: Text -> Matrix Char
buildMatrixFromInput input = Matrix mdat mwidth mheight
 where
  mwidth = Text.length . head . Text.lines $ input
  mheight = length . Text.lines $ input
  mdat = V.reverse $ V.unfoldr go (mconcat . Text.lines $ input)
  go v = swap <$> Text.unsnoc v

walkOffsetLen :: Int -> Int -> Int -> Int -> Int -> Matrix a -> Maybe [a]
walkOffsetLen len rowStep colStep srow scol m =
  sequence
    [matrixCharAt row col m | (row, col) <- generateSequences]
 where
  generateSequence start step count = take count $ iterate (+ step) start
  generateSequences = generateSequence srow rowStep len `zip` generateSequence scol colStep len

part1 :: Text -> IO Int
part1 input = pure . sum $ [go row col | row <- [0 .. matrix.height], col <- [0 .. matrix.width - 1]]
 where
  matrix = buildMatrixFromInput input
  go row col =
    length . filter (== "XMAS") . catMaybes $
      [ walkOffsetLen 4 rowStep colStep row col matrix
      | rowStep <- [-1 .. 1]
      , colStep <- [-1 .. 1]
      , rowStep /= 0 || colStep /= 0
      ]

part2 :: Text -> IO Int
part2 input = pure . sum $ [1 | row <- [0 .. matrix.height], col <- [0 .. matrix.width - 1], matrixCharAt row col matrix == Just 'A', go row col == Just True]
 where
  matrix = buildMatrixFromInput input
  go row col = do
    ulbr <- sequence [matrixCharAt (row - 1) (col - 1) matrix, matrixCharAt (row + 1) (col + 1) matrix]
    urbl <- sequence [matrixCharAt (row - 1) (col + 1) matrix, matrixCharAt (row + 1) (col - 1) matrix]
    pure $ ulbr `elem` ["MS", "SM"] && urbl `elem` ["MS", "SM"]
