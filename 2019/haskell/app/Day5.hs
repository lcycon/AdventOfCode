module Day5 where

import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

import           Lib                            ( readCommaSepSignedInts )

data Program = Program {
    _i      :: Int,
    _dat    :: V.Vector Int,
    _input  :: [Int],
    _output :: [Int]
}

runProgram :: Program -> [Int]
runProgram p@(Program idx dat input output) = res
  where
    res = case opcode of
        -- Add
        1 -> runProgram
            (Program (idx + 4)
                     (update (valueAtOffset 3) (argAt 1 + argAt 2))
                     input
                     output
            )
        -- Mult
        2 -> runProgram
            (Program (idx + 4)
                     (update (valueAtOffset 3) (argAt 1 * argAt 2))
                     input
                     output
            )
        -- Input
        3 -> runProgram
            (Program (idx + 2)
                     (update (valueAtOffset 1) (head input))
                     (tail input)
                     output
            )
        -- Output
        4 -> runProgram (Program (idx + 2) dat input (argAt 1 : output))
        -- Jump if true
        5 ->
            let newIdx = if argAt 1 /= 0 then argAt 2 else idx + 3
            in  runProgram (Program newIdx dat input output)
        -- Jump if false
        6 ->
            let newIdx = if argAt 1 == 0 then argAt 2 else idx + 3
            in  runProgram (Program newIdx dat input output)
        -- LT
        7 ->
            let value = if argAt 1 < argAt 2 then 1 else 0
            in
                runProgram
                    (Program (idx + 4)
                             (update (valueAtOffset 3) value)
                             input
                             output
                    )
        -- EQ
        8 ->
            let value = if argAt 1 == argAt 2 then 1 else 0
            in
                runProgram
                    (Program (idx + 4)
                             (update (valueAtOffset 3) value)
                             input
                             output
                    )
        99 -> output
        n  -> error $ "Shouldn't happen, opcode: " ++ show n ++ ", " ++ show
            (V.take 5 . V.drop idx $ dat)

    valueAtOffset x = dat V.! (idx + x)
    update updatePoint newData = dat V.// [(updatePoint, newData)]
    argAt  = readArg p fullop
    fullop = dat V.! idx
    opcode = readOpCode fullop

problem1 :: Text -> IO ()
problem1 inputData = do
    print (runProgram program)
  where
    input   = V.fromList (readCommaSepSignedInts inputData)
    program = Program 0 input [1] []

problem2 :: Text -> IO ()
problem2 inputData = print (runProgram program)
  where
    input   = V.fromList (readCommaSepSignedInts inputData)
    program = Program 0 input [5] []

readOpCode :: Int -> Int
readOpCode x = x `mod` 100

readArg :: Program -> Int -> Int -> Int
readArg (Program idx dat _ _) opinst i =
    case opinst `div` (10 ^ (i + 1)) `mod` 10 of
        0 -> dat V.! (dat V.! (idx + i))
        1 -> dat V.! (idx + i)
        _ -> error "This shouldn't happen"
