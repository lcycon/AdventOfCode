module IntCode
    ( Program(..)
    , initialize
    , runProgram
    , runProgramRaw
    , runProgramC
    )
where

import qualified Data.HashMap.Strict           as HM

data Program = Program {
    _i      :: !Integer,
    _dat    :: !(HM.HashMap Integer Integer),
    _input  :: [Integer],
    _output :: [Integer],
    _rb     :: !Integer,
    _halted :: Bool
}
    deriving (Eq, Show)

initialize :: [Integer] -> Program
initialize dat = Program 0 (HM.fromList . zip [0 ..] $ dat) [] [] 0 False

runProgram :: Program -> [Integer]
runProgram = _output . runProgramRaw

runProgramRaw :: Program -> Program
runProgramRaw = runProgramC False

runProgramC :: Bool -> Program -> Program
runProgramC shouldSuspendOnMissingInput p@(Program idx dat input output rb halted)
    = res
  where
    res = case opcode of
        -- Add
        1 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 4)
                     (update (addressArgAt 3) (argAt 1 + argAt 2))
                     input
                     output
                     rb
                     halted
            )
        -- Mult
        2 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 4)
                     (update (addressArgAt 3) (argAt 1 * argAt 2))
                     input
                     output
                     rb
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
                         (update (addressArgAt 1) (head input))
                         (tail input)
                         output
                         rb
                         halted
                )
        -- Output
        4 -> runProgramC
            shouldSuspendOnMissingInput
            (Program (idx + 2) dat input (argAt 1 : output) rb halted)
        -- Jump if true
        5 ->
            let newIdx = if argAt 1 /= 0 then argAt 2 else idx + 3
            in  runProgramC shouldSuspendOnMissingInput
                            (Program newIdx dat input output rb halted)
        -- Jump if false
        6 ->
            let newIdx = if argAt 1 == 0 then argAt 2 else idx + 3
            in  runProgramC shouldSuspendOnMissingInput
                            (Program newIdx dat input output rb halted)
        -- LT
        7 ->
            let value = if argAt 1 < argAt 2 then 1 else 0
            in  runProgramC
                    shouldSuspendOnMissingInput
                    (Program (idx + 4)
                             (update (addressArgAt 3) value)
                             input
                             output
                             rb
                             halted
                    )
        -- EQ
        8 ->
            let value = if argAt 1 == argAt 2 then 1 else 0
            in  runProgramC
                    shouldSuspendOnMissingInput
                    (Program (idx + 4)
                             (update (addressArgAt 3) value)
                             input
                             output
                             rb
                             halted
                    )
        -- Set relative base
        9 -> runProgramC shouldSuspendOnMissingInput
                         (p { _i = _i p + 2, _rb = rb + argAt 1 })
        -- Halt
        99 -> p { _halted = True }
        n  -> error $ "Shouldn't happen, opcode: " ++ show n

    update updatePoint newData = HM.insert updatePoint newData dat
    argAt        = readArg p fullop
    addressArgAt = readAddressArg p fullop
    fullop       = dat HM.! idx
    opcode       = readOpCode fullop

readOpCode :: Integer -> Integer
readOpCode x = x `mod` 100

readAddressArg :: Program -> Integer -> Integer -> Integer
readAddressArg (Program idx dat _ _ rb _) opinst i =
    case opinst `div` (10 ^ (i + 1)) `mod` 10 of
        0 -> HM.lookupDefault defaultValue (idx + i) dat
        2 -> rb + (HM.lookupDefault defaultValue (idx + i) dat)
        _ -> error "This shouldn't happen"
    where defaultValue = 0

readArg :: Program -> Integer -> Integer -> Integer
readArg (Program idx dat _ _ rb _) opinst i =
    case opinst `div` (10 ^ (i + 1)) `mod` 10 of
        0 -> HM.lookupDefault defaultValue
                              (HM.lookupDefault defaultValue (idx + i) dat)
                              dat
        1 -> HM.lookupDefault defaultValue (idx + i) dat
        2 -> HM.lookupDefault
            defaultValue
            (rb + (HM.lookupDefault defaultValue (idx + i) dat))
            dat
        _ -> error "This shouldn't happen"
    where defaultValue = 0
