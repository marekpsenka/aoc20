module Main where

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Array
import qualified Data.Map.Strict as Map

type CustomInteger = Array Integer Integer

toCustom :: Integer -> Integer -> CustomInteger
toCustom w i = listArray (0, w - 1) (reverse $ toCustom' (w - 1) i)
  where
    toCustom' :: Integer -> Integer -> [Integer]
    toCustom' 0 0 = [0]
    toCustom' 0 1 = [1]
    toCustom' e i
      | i >= 2^e = 1:toCustom' (e - 1) (i - 2^e)
      | otherwise = 0:toCustom' (e - 1) i

toCustomInt36 = toCustom 36

fromCustom :: CustomInteger -> Integer
fromCustom ci = sum (map (\(e, d) -> d * 2^e) (assocs ci))

customToString :: CustomInteger -> String
customToString ci = concatMap show (elems ci)

type Mask = String
type Assignment = (Integer, Integer)
type Instruction = Either Mask Assignment
type Memory = Map.Map Integer Integer

data ProgramState = ProgramState { mem :: Memory, mask :: Mask }

applyMask :: Mask -> CustomInteger -> CustomInteger
applyMask mask ci = ci // mapMaybe f (zip indices mask )
  where
    indices = [(0::Integer)..(genericLength mask - 1)]
    f (i, '1') = Just (i, 1 :: Integer)
    f (i, '0') = Just (i, 0 :: Integer)
    f _ = Nothing

applyMask2 :: Mask -> CustomInteger -> CustomInteger
applyMask2 mask ci = ci // mapMaybe f (zip indices mask )
  where
    indices = [(0::Integer)..(genericLength mask - 1)]
    f (i, '1') = Just (i, 1 :: Integer)
    f (i, 'X') = Just (i, 2 :: Integer)
    f _ = Nothing


expand :: CustomInteger -> [CustomInteger]
expand ci = map (listArray (bounds ci)) (expand' (elems ci))
  where
    expand' [] = [[]]
    expand' (2:xs) = map (0:) (expand' xs) ++ map (1:) (expand' xs)
    expand' (x:xs) = map (x:) (expand' xs) 

addressesToWrite :: Mask -> Integer -> [Integer]
addressesToWrite mask ci = map fromCustom (expand ci')
  where
    ci' = applyMask2 mask (toCustomInt36 ci)

readInstruction :: String -> Instruction
readInstruction s 
  | "mask" `isPrefixOf` s = Left (reverse b)
  | "mem" `isPrefixOf` s = Right (readAssignment a b)
  | otherwise = error "unexpected"
  where
    a:b:_ = splitOn " = " s
    readAssignment a b = 
      let _:c:_ = splitOn "[" a in (read (init c) :: Integer, read b :: Integer)

execute :: ProgramState -> Instruction -> ProgramState
execute st (Left m) = ProgramState {mem = mem st, mask = m}
execute st (Right (addr, val)) = ProgramState {mem = mem', mask = mask st}
  where
    newVal = fromCustom (applyMask (mask st) (toCustomInt36 val))
    mem' = Map.alter (\_ -> Just newVal) addr (mem st)

execute2 :: ProgramState -> Instruction -> ProgramState
execute2 st (Left m) = ProgramState {mem = mem st, mask = m}
execute2 st (Right (addr, val)) = ProgramState {mem = mem', mask = mask st}
  where
    setMem newVal m addr = Map.alter (\_ -> Just newVal) addr m 
    mem' = foldl (setMem val) (mem st) (addressesToWrite (mask st) addr)

main :: IO ()
main = do
  ins <- map readInstruction . lines <$> readFile "input.txt"
  let
    initState = ProgramState {mem = Map.empty, mask = replicate 36 'X'}
    endState = foldl execute initState ins
    endState2 = foldl execute2 initState ins

  print (sum $ Map.elems (mem endState))
  print (sum $ Map.elems (mem endState2))

