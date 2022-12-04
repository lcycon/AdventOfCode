module Day3 where

import           Prelude                 hiding ( Either(..) )

import qualified Data.Attoparsec.Text          as P
import           Data.Functor                   ( ($>) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Data.List                      ( foldl' )
import           Data.Text                      ( Text )

import           Lib                            ( readCustomLines )

data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

data Move = Move Direction Int
    deriving (Eq, Show)

newtype Line = Line [Move]
    deriving (Eq, Show)

type Crossings = HM.HashMap (Int, Int) Int

directionParser :: P.Parser Direction
directionParser = P.choice
    [ P.char 'R' $> Right
    , P.char 'L' $> Left
    , P.char 'U' $> Up
    , P.char 'D' $> Down
    ]

moveParser :: P.Parser Move
moveParser = Move <$> directionParser <*> P.decimal

lineParser :: P.Parser Line
lineParser = Line <$> moveParser `P.sepBy1'` P.char ','

problem1 :: Text -> IO ()
problem1 inputData = print (minimum intersectionDistances)
  where
    (firstLine : secondLine : _) = readCustomLines lineParser inputData
    intersects = HS.intersection (HM.keysSet $ lineToPathMap firstLine)
                                 (HM.keysSet $ lineToPathMap secondLine)
    intersectionDistances =
        map (manhattanDistance (0, 0)) (HS.toList intersects)

problem2 :: Text -> IO ()
problem2 inputData = print answer
  where
    (firstLine : secondLine : _) = readCustomLines lineParser inputData
    intersects = HM.intersectionWith (+)
                                     (lineToPathMap firstLine)
                                     (lineToPathMap secondLine)
    answer = minimum (HM.elems intersects)

lineToPathMap :: Line -> Crossings
lineToPathMap (Line moves) = tupRes $ foldl' go (HM.empty, (0, 0), 0) moves
  where
    tupRes (a, _, _) = HM.delete (0, 0) a
    go (hm, (x, y), distance) move =
        (newMap, (x', y'), distance + totalDistance)
      where
        totalDistance = manhattanDistance (x, y) (x', y')
        (x', y')      = applyMoveToCoord (x, y) move
        additions     = HM.fromList
            [ ((xi, yi), distance + manhattanDistance (x, y) (xi, yi))
            | xi <- range x x'
            , yi <- range y y'
            ]
        newMap = HM.unionWith min hm additions

applyMoveToCoord :: (Int, Int) -> Move -> (Int, Int)
applyMoveToCoord (x, y) (Move Down  offset) = (x, y - offset)
applyMoveToCoord (x, y) (Move Up    offset) = (x, y + offset)
applyMoveToCoord (x, y) (Move Left  offset) = (x - offset, y)
applyMoveToCoord (x, y) (Move Right offset) = (x + offset, y)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

range :: Int -> Int -> [Int]
range a b | a > b     = [b .. a]
          | otherwise = [a .. b]
