module Day8 where

import qualified Data.Attoparsec.Text          as P
import           Data.List                      ( intercalate
                                                , transpose
                                                )
import           Data.List.Extra                ( minimumOn )
import           Data.List.Split                ( chunksOf )
import           Data.Text                      ( Text )

import           Lib                            ( readCustomLines )

width, height, pixelsPerLayer :: Int
width = 25
height = 6
pixelsPerLayer = width * height

inputP :: P.Parser [Int]
inputP = P.many1' (readInt <$> P.digit) where readInt = read . pure

problem1 :: Text -> IO ()
problem1 inputData = print (numberOfOnes * numberOfTwos)
  where
    countDigits d = count (== d)
    input                = head $ readCustomLines inputP inputData
    layers               = chunksOf pixelsPerLayer input
    layerWithFewestZeros = minimumOn (countDigits 0) layers
    numberOfOnes         = countDigits 1 layerWithFewestZeros
    numberOfTwos         = countDigits 2 layerWithFewestZeros

problem2 :: Text -> IO ()
problem2 inputData = putStrLn (render finalImage)
  where
    input      = head $ readCustomLines inputP inputData
    layers     = chunksOf pixelsPerLayer input
    transposed = transpose layers
    finalImage = chunksOf width . map finalColor $ transposed
    -- First pixel in a layer-wise stack (our input) that is 0 or 1 is the final color
    -- If everything is a 2, then I guess a 2?
    finalColor [c       ] = c
    finalColor (0 : _   ) = 0
    finalColor (1 : _   ) = 1
    finalColor (2 : rest) = finalColor rest
    finalColor _          = error "This should never happen"
    -- Render a pretty final answer
    render = intercalate "\n" . map (map renderCharacterSub)
    -- 0s and 1s are really hard to read, so lets sub 'em out
    renderCharacterSub 1 = '8'
    renderCharacterSub _ = ' '

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
