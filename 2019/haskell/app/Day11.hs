module Day11 where

import           Prelude                 hiding ( Either(..) )

import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )

import qualified IntCode
import           Lib                            ( readCommaSepSignedIntegers )

data Direction = Up | Down | Left | Right

type Point = (Int, Int)

newtype Field = Field (HM.HashMap Point Bool)

data PaintState = PaintState IntCode.Program Point Direction Field

emptyField :: Field
emptyField = Field (HM.empty)

paintedPoints :: Field -> Int
paintedPoints (Field hm) = HM.size hm

colorAt :: Field -> Point -> Bool
colorAt (Field hm) p = HM.lookupDefault False p hm

colorAtInt :: Field -> Point -> Integer
colorAtInt f p = go $ colorAt f p
  where
    go True  = 1
    go False = 0

setColor :: Field -> Point -> Bool -> Field
setColor (Field hm) p b = Field $ HM.insert p b hm

setColorInt :: Field -> Point -> Integer -> Field
setColorInt f p 0 = setColor f p False
setColorInt f p 1 = setColor f p True
setColorInt _ _ _ = error "Boom"

paintHull :: IntCode.Program -> Field -> Field
paintHull p f = go (PaintState p (0, 0) Up f)
  where
    go (PaintState prog point direction field) =
        case
                IntCode.runProgramC
                    True
                    prog { IntCode._output = []
                         , IntCode._input  = [colorAtInt field point]
                         }
            of
                IntCode.Program { IntCode._halted = True } -> field
                np@IntCode.Program { IntCode._output = [adjust, toPaint] } ->
                    go
                        (PaintState np
                                    (adjustPoint point direction adjust)
                                    (adjustDirection direction adjust)
                                    (setColorInt field point toPaint)
                        )
                _ -> error "BOOM"

adjustPoint :: Point -> Direction -> Integer -> Point
adjustPoint (x, y) d adjust = newPoint
  where
    newDirection = adjustDirection d adjust
    newPoint     = case newDirection of
        Up    -> (x, y + 1)
        Down  -> (x, y - 1)
        Left  -> (x - 1, y)
        Right -> (x + 1, y)

adjustDirection :: Direction -> Integer -> Direction
adjustDirection Up    0 = Left
adjustDirection Up    1 = Right
adjustDirection Down  0 = Right
adjustDirection Down  1 = Left
adjustDirection Left  0 = Down
adjustDirection Left  1 = Up
adjustDirection Right 0 = Up
adjustDirection Right 1 = Down
adjustDirection _     _ = error "BOOM"

displayField :: Field -> String
displayField f@(Field hm) = result
  where
    result = intercalate
        "\n"
        [ mconcat [ displayValue (colorAt f (x, y)) | x <- [minX .. maxX] ]
        | y <- reverse [minY .. maxY]
        ]
    minX = minimum . map fst . HM.keys $ hm
    maxX = maximum . map fst . HM.keys $ hm
    minY = minimum . map snd . HM.keys $ hm
    maxY = maximum . map snd . HM.keys $ hm
    displayValue False = " "
    displayValue True  = "#"

problem1 :: Text -> IO ()
problem1 inputData = print paintedTiles
  where
    input        = IntCode.initialize $ readCommaSepSignedIntegers inputData
    paintedTiles = paintedPoints $ paintHull input emptyField

problem2 :: Text -> IO ()
problem2 inputData = putStrLn paintJob
  where
    input    = IntCode.initialize $ readCommaSepSignedIntegers inputData
    paintJob = displayField $ paintHull input (setColor emptyField (0, 0) True)
