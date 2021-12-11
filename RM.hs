{-# LANGUAGE BangPatterns #-}

import Debug.Trace
import Data.Type.Bool (Not)
import Data.Maybe
import Data.Bits
import Data.List ( intercalate )

showSteps :: Bool
showSteps = True

type Register = Int
type State = [Int]
type Instruction = State -> State
type Program = [(String, Instruction)]

-- Formatting
-- https://unicode-table.com/en/sets/superscript-and-subscript-letters/
subScript :: String -> String
subScript = map s
  where
    s '0' = '\8320'
    s '1' = '\8321'
    s '2' = '\8322'
    s '3' = '\8323'
    s '4' = '\8324'
    s '5' = '\8325'
    s '6' = '\8326'
    s _ = undefined

superPlus, superMinus :: Char
superPlus = '\8314'
superMinus = '\8315'

formatHalt :: String
formatHalt = "HALT"

formatAdd :: Register -> Integer -> String
formatAdd i j = formatRegister i ++ superPlus : " → " ++ formatLabel j

formatSub :: Register -> Integer -> Integer -> String
formatSub i j k = formatRegister i ++ superMinus : " → " ++ formatLabel j ++ ", " ++ formatLabel k

formatRegister :: Register -> String
formatRegister r = 'R' : subScript (show r)

formatLabel :: Integer -> String
formatLabel l = 'L' : subScript (show l)

formatProgram :: Program -> String 
formatProgram p = intercalate "\n" $
    zipWith (++)
        (map ((++ ": ") . formatLabel) [0..])
        (map fst p)

halt = id

set :: Register -> Int -> Instruction
set 0 x (y : ys) = x : ys
set n x (y : ys) = y : set (n - 1) x ys
set  _ _ _ = undefined

sub :: Register -> Instruction -> Instruction -> Instruction
sub i f g !state
  | n == 0    = g s
  | otherwise = f $ set i (n - 1) s
    where
      n = s !! i

      s
        | showSteps = traceShowId state
        | otherwise = state

add :: Register -> Instruction -> Instruction
add i f !state
  = f $ set i (n + 1) s
    where
      n = s !! i

      s
        | showSteps = traceShowId state
        | otherwise = state

-- Have to make my own version because Data.Bits doesn't define it for Integer
countTrailingZeros' :: Integer -> Integer
-- PRE: n /= 0
countTrailingZeros' n
  | odd n    = 0
  | otherwise = 1 + countTrailingZeros' (n `div` 2)

-- Pairs.
encodePair :: (Integer, Integer) -> Integer
encodePair (x, y) = (2 ^ x) * (2 * y + 1) - 1
decodePair :: Integer -> (Integer, Integer)
decodePair 0 = undefined
decodePair p = (x, y)
  where
    x = countTrailingZeros' p
    y = shift p (-(fromInteger x) - 1)

-- Lists.
decodeList :: Integer -> [Integer]
decodeList 0 = []
decodeList l = x : decodeList y
  where (x, y) = decodePair l
encodeList :: [Integer] -> Integer
encodeList [] = 0
encodeList (x : xs) = 1 + encodePair (x, encodeList xs)

-- Instructions bodies.
decodeInstruction :: Program -> Integer -> (String, Instruction)
decodeInstruction _ 0 = (formatHalt, halt)
decodeInstruction program pair
  | even f   = (formatAdd i arg, add i (instr arg))
  | otherwise = (formatSub i j k, sub i (instr j) (instr k))
  where
    (f, arg) = decodePair pair
    i = fromIntegral f `div` 2 :: Int
    (j, k) = decodePair (arg + 1)
    
    instr n = snd $ program !! fromIntegral n

-- Programs.
decodeProgram :: Integer -> Program
decodeProgram = decodeProgramFromList . decodeList

decodeProgramFromList :: [Integer] -> Program
decodeProgramFromList l = program
  where
    program = map (decodeInstruction program) l

-- Execution
runProgram :: Program -> State -> State
runProgram = snd . head

-- Instructions.
encodeAdd :: Integer -> Integer -> Integer
encodeAdd i j = 1 + encodePair (2 * i, j)

encodeSub :: Integer -> Integer -> Integer -> Integer
encodeSub i j k = 1 + encodePair (2 * i + 1, encodePair (j, k))

encodeHalt :: Integer
encodeHalt = 0

-- Programs
zeroR0 :: Integer -- 3072
zeroR0 = encodeList [
  encodeSub 0 0 1, -- L₀: R₀⁻ → L₀, L₁
  encodeHalt]      -- L₁: HALT
  
addL0L1 :: Integer -- 6755674318962688
addL0L1 = encodeList [
  encodeSub 0 1 2, -- L₀: R₀⁻ → L₁, L₂
  encodeAdd 1 1,   -- L₁: R₁⁺ → L₁
  encodeHalt]      -- L₂: HALT

-- | R0 = R1 + R2.
adder :: Integer -- 28738678589325274522871087916445314772282785417214007261208231566195210827554169179139861628470962486606050497613789319537928452929252379720353942732879195765819929905481411192367913165424870405419359236629869563732570138212258934001178960300833861066045972985771779328041376477915028074641341742845112134412602756554506347740596379378771007129255295254260103608448723007616643684771274023685388049424268528133557469406988031448736465728547080432626258098667281737687908406236777938345714191323532562315600695931011610336637664954962548791057158591283473247357043382114632403895483000116092134518686862114381002254085074428592896626333315826705672990675973186146242892276056650679867491893573492116998932363973633639087766413428595732271832274448318088427381471021785644268820953877626134901789547297676215103641013574133336623969712045882654242431727896093194490955173751425648027501827221333805372761355399652661789738833048217909906861836304228703616513335888646946990427220815196181512059681627100470476690600390128221682196050367210088188662695436612748023048427226148698151404842535621340206218031127722370326842030151162040144267846930945521052830263468727196374740202910097556253633281843875156784405312440341158883729372819819334058400746019743922715399937828161817022670556978440727174928369172154273083243857404267686720588699632647990306415126457193269688351591566128567521220764583547861786591559018936249608044544
adder = encodeList [
  encodeSub 1 1 2, -- L₀: R₁⁻ → L₁, L₂
  encodeAdd 0 0,   -- L₁: R₀⁺ → L₀
  encodeSub 2 3 4, -- L₂: R₂⁻ → L₃, L₄
  encodeAdd 0 2,   -- L₃: R₀⁺ → L₂
  encodeHalt]      -- L₄: HALT

infiniteLoop :: Integer
infiniteLoop = encodeList [
  encodeAdd 0 0]

cwQ1 :: Integer
cwQ1 = encodeList [
  encodeSub 1 2 1, -- L₀: R₁⁻ → L₂, L₁
  encodeHalt,      -- L₁: HALT
  encodeSub 1 3 4, -- L₂: R₁⁻ → L₃, L₄
  encodeSub 1 5 4, -- L₃: R₁⁻ → L₅, L₄
  encodeHalt,      -- L₄: HALT
  encodeAdd 0 0]   -- L₅: R₀⁺ → L₀

cwQ2 :: Integer
cwQ2 = 2^46 * 20483

printProgram :: Integer -> IO ()
printProgram gn = do
  putStrLn $ formatProgram $ decodeProgram gn

executeProgram :: Integer -> State -> IO ()
executeProgram gn state = do
  putStrLn $ "\tEncoded Program: " ++ show gn
  putStrLn $ "\tDecoded Program: \n" ++ formatProgram decoded
  putStrLn $ "\tInitial State: " ++ show state
  putStrLn $ (\(!s) -> "\tFinal State: " ++ show s) state'
    where
      decoded = decodeProgram gn
      state' = runProgram decoded state

