module Day7 where

import           Data.List                      ( permutations )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

import           Lib                            ( readCommaSepSignedInts )

data Program = Program {
    _i      :: Int,
    _dat    :: V.Vector Int,
    _input  :: [Int],
    _output :: [Int],
    _halted :: Bool
}
    deriving (Eq, Show)

runProgram :: Program -> [Int]
runProgram = _output . runProgramRaw

runProgramRaw :: Program -> Program
runProgramRaw = runProgramC False

runProgramC :: Bool -> Program -> Program
runProgramC shouldSuspendOnMissingInput p@(Program idx dat input output halted)
    = res
  where
    res = case opcode of
        -- Add
        1 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 4)
                     (update (valueAtOffset 3) (argAt 1 + argAt 2))
                     input
                     output
                     halted
            )
        -- Mult
        2 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 4)
                     (update (valueAtOffset 3) (argAt 1 * argAt 2))
                     input
                     output
                     halted
            )
        -- Input
        3 -> case (shouldSuspendOnMissingInput, null input) of
            (True, True) -> p
            (False, True) ->
                error "You asked me not to suspend on missing input, so BOOM"
            _ -> runProgramC
                shouldSuspendOnMissingInput
                (Program (idx + 2)
                         (update (valueAtOffset 1) (head input))
                         (tail input)
                         output
                         halted
                )
        -- Output
        4 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 2) dat input (argAt 1 : output) halted)
        -- Jump if true
        5 ->
            let newIdx = if argAt 1 /= 0 then argAt 2 else idx + 3
            in  runProgramC shouldSuspendOnMissingInput
                            (Program newIdx dat input output halted)
        -- Jump if false
        6 ->
            let newIdx = if argAt 1 == 0 then argAt 2 else idx + 3
            in  runProgramC shouldSuspendOnMissingInput
                            (Program newIdx dat input output halted)
        -- LT
        7 ->
            let value = if argAt 1 < argAt 2 then 1 else 0
            in  runProgramC
                    shouldSuspendOnMissingInput
                    (Program (idx + 4)
                             (update (valueAtOffset 3) value)
                             input
                             output
                             halted
                    )
        -- EQ
        8 ->
            let value = if argAt 1 == argAt 2 then 1 else 0
            in  runProgramC
                    shouldSuspendOnMissingInput
                    (Program (idx + 4)
                             (update (valueAtOffset 3) value)
                             input
                             output
                             halted
                    )
        99 -> p { _halted = True }
        n  -> error $ "Shouldn't happen, opcode: " ++ show n ++ ", " ++ show
            (V.take 5 . V.drop idx $ dat)

    valueAtOffset x = dat V.! (idx + x)
    update updatePoint newData = dat V.// [(updatePoint, newData)]
    argAt  = readArg p fullop
    fullop = dat V.! idx
    opcode = readOpCode fullop

problem1Settings :: [(Int, Int, Int, Int, Int)]
problem1Settings = map arrToList . permutations $ [0, 1, 2, 3, 4]
  where
    arrToList [a, b, c, d, e] = (a, b, c, d, e)
    arrToList _               = error "This should never happen"

problem2Settings :: [(Int, Int, Int, Int, Int)]
problem2Settings = map arrToList . permutations $ [5, 6, 7, 8, 9]
  where
    arrToList [a, b, c, d, e] = (a, b, c, d, e)
    arrToList _               = error "This should never happen"

runAmpChain :: V.Vector Int -> (Int, Int, Int, Int, Int) -> Int
runAmpChain mem (a, b, c, d, e) = runE
  where
    programOf phase input = Program 0 mem [phase, input] [] False
    runA = head $ runProgram (programOf a 0)
    runB = head $ runProgram (programOf b runA)
    runC = head $ runProgram (programOf c runB)
    runD = head $ runProgram (programOf d runC)
    runE = head $ runProgram (programOf e runD)

runRecurringAmpChain :: V.Vector Int -> (Int, Int, Int, Int, Int) -> Int
runRecurringAmpChain mem (a, b, c, d, e) = go initial
  where
    initial =
        ( programOf a [0] -- A is seeded just once with 0
        , programOf b []
        , programOf c []
        , programOf d []
        , programOf e []
        )
    programOf phase input = Program 0 mem (phase : input) [] False
    go (progA, progB, progC, progD, progE)
        |
        -- If amplifier E has halted, our answer is its most recent output
          _halted progE = head $ _output progE
        |
        -- If we aren't halted, run another round...
          otherwise     = go (newA, newB, newC, newD, newE)
      where
        buildProgramToRun input toRun =
            -- Our input is taken from the concat of our existing input (which is likely none)
            -- plus the output of the amplifier that is the input to us.
            --
            -- We truncate our out to avoid re-adding old values.
            toRun { _input = _input toRun ++ _output input, _output = [] }
        newA = runProgramC True (buildProgramToRun progE progA)
        newB = runProgramC True (buildProgramToRun progA progB)
        newC = runProgramC True (buildProgramToRun progB progC)
        newD = runProgramC True (buildProgramToRun progC progD)
        newE = runProgramC True (buildProgramToRun progD progE)

problem1 :: Text -> IO ()
problem1 inputData = print answer
  where
    input      = V.fromList (readCommaSepSignedInts inputData)
    candidates = map (runAmpChain input) problem1Settings
    answer     = maximum candidates

problem2 :: Text -> IO ()
problem2 inputData = do
    print answer
  where
    input      = V.fromList (readCommaSepSignedInts inputData)
    candidates = map (runRecurringAmpChain input) problem2Settings
    answer     = maximum candidates

readOpCode :: Int -> Int
readOpCode x = x `mod` 100

readArg :: Program -> Int -> Int -> Int
readArg (Program idx dat _ _ _) opinst i =
    case opinst `div` (10 ^ (i + 1)) `mod` 10 of
        0 -> dat V.! (dat V.! (idx + i))
        1 -> dat V.! (idx + i)
        _ -> error "This shouldn't happen"
