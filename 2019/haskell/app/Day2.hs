module Day2 where

import           Data.Text                      ( Text )
import           Lib                            ( readCommaSepInts )

import qualified Data.Vector                   as V

problem1 :: Text -> IO ()
problem1 inputData = print (result V.! 0)
  where
    input         = V.fromList (readCommaSepInts inputData)
    modifiedInput = input V.// [(1, 12), (2, 2)]
    result        = runProgram modifiedInput

problem2 :: Text -> IO ()
problem2 inputData = print (100 * noun + verb)
  where
    input        = V.fromList (readCommaSepInts inputData)
    (noun, verb) = findSolution input 19690720

findSolution :: V.Vector Int -> Int -> (Int, Int)
findSolution program candidate = head
    -- This doesn't check all 10,000. Since `head` only requires 1 result, evaluation
    -- stops once the first result is found
    [ (noun, verb)
    | noun <- [0 .. 99]
    , verb <- [0 .. 99]
    , runCandidate noun verb == candidate
    ]
  where
    alterProgram noun verb = program V.// [(1, noun), (2, verb)]
    runCandidate noun verb = runProgram (alterProgram noun verb) V.! 0

runProgram :: V.Vector Int -> V.Vector Int
runProgram = go 0
  where
    go idx program = case program V.! idx of
      -- Addition
        1 -> go (idx + 4)
                (program V.// [(writeIdx, firstOperand + secondOperand)])
        -- Multiplication
        2 -> go (idx + 4)
                (program V.// [(writeIdx, firstOperand * secondOperand)])
        -- Halt
        99 -> program
        _  -> error "This should never happen"
      where
        dataAt i = program V.! i
        firstOperand  = dataAt (dataAt (idx + 1))
        secondOperand = dataAt (dataAt (idx + 2))
        writeIdx      = dataAt (idx + 3)
