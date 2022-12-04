module Day10 where

import qualified Data.Attoparsec.Text          as P
import qualified Data.HashSet                  as HS
import           Data.List                      ( partition
                                                , sortOn
                                                )
import           Data.List.Extra                ( groupOn
                                                , maximumOn
                                                )
import           Data.Text                      ( Text )

import           Lib                            ( readCustomLines )

data InputCell = Blank | Asteroid
    deriving (Eq, Show)

cellP :: P.Parser InputCell
cellP = P.choice [P.char '.' >> return Blank, P.char '#' >> return Asteroid]

buildGrid :: [[InputCell]] -> HS.HashSet (Int, Int)
buildGrid cells = HS.fromList
    [ (fromIntegral x, fromIntegral y)
    | (y, xs) <- zip indicies cells
    , (x, e ) <- zip indicies xs
    , e == Asteroid
    ]
    where indicies = [0 ..] :: [Int]

bestAsteroidBase :: HS.HashSet (Int, Int) -> ((Int, Int), Int)
bestAsteroidBase grid =
    maximumOn snd
        . map (\x -> (x, HS.size . visibleFrom x $ grid))
        $ (HS.toList grid)

problem1 :: Text -> IO ()
problem1 inputData = print answer
  where
    input       = readCustomLines (P.many1' cellP) inputData
    grid        = buildGrid input
    (_, answer) = bestAsteroidBase grid

visibleFrom :: (Int, Int) -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
visibleFrom me =
    HS.fromList
        . map (getPoint . head . sortOn distanceOf)
        . groupOn angleOf
        . sortOn angleOf
        . map angleAndDistance
        . HS.toList
        . HS.delete me
  where
    getPoint (a, _, _) = a
    angleOf (_, b, _) = b
    distanceOf (_, _, c) = c
    angleAndDistance p = (p, angleTo me p, distance me p)

problem2 :: Text -> IO ()
problem2 inputData = print answer
  where
    input              = readCustomLines (P.many1' cellP) inputData
    grid               = buildGrid input
    (me, _)            = bestAsteroidBase grid
    deletes            = buildAsteroidDestructionList (HS.delete me grid) me
    (answerX, answerY) = deletes !! 199
    answer             = answerX * 100 + answerY

-- Grab all of the asteroids visible, sorted by radial from due-north. Once we have those, recurse
-- on all asteroids we haven't considered. Repeat until done.
buildAsteroidDestructionList
    :: HS.HashSet (Int, Int) -> (Int, Int) -> [(Int, Int)]
buildAsteroidDestructionList grid me
    | HS.null grid = []
    | otherwise    = buildSortedOutput visible ++ recur
  where
    recur   = buildAsteroidDestructionList (HS.difference grid visible) me
    visible = visibleFrom me grid
    buildSortedOutput =
        map fst
            . uncurry (flip (++)) -- Put upper left quadrant at the end of the list
            . partition (isUpperLeftQuadrant . snd) -- Isolate the upper left quadrant since it's in the wrong place
            . sortOn snd -- Sort on the angle
            . map (\x -> (x, angleTo me x)) -- Annotate with angle
            . HS.toList -- Make a list
    isUpperLeftQuadrant = (< (-pi / 2))

angleTo :: (Int, Int) -> (Int, Int) -> Double
angleTo (x1, y1) (x2, y2) = atan2 (fromIntegral y2 - fromIntegral y1)
                                  (fromIntegral x2 - fromIntegral x1)

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) = sqrt (x' * x' + y' * y')
  where
    x' = fromIntegral x1 - fromIntegral x2
    y' = fromIntegral y1 - fromIntegral y2
