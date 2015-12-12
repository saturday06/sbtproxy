module Foo where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Function
import Data.DList

foo :: Int -> Int
foo x = x * 2

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
              ]
  guard (c' `elem` [1..9] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start target = elem target (in3 start)

in3x :: KnightPos -> KnightPos -> [[KnightPos]]
in3x start target = do
  first <- moveKnight start
  second <- moveKnight first
  third <- moveKnight second
  guard (third == target)
  return [first, second]

isBigGang :: Int -> (Bool, String)
isBigGang members = (members > 9, "Big!?")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Hoooo Number: " ++ show x])

gcd' :: Int -> Int -> Int
gcd' x y
  | y == 0 = x
  | otherwise = gcd' y (x `mod` y)

gcdx :: Int -> Int -> Writer [String] Int
gcdx a b
  | b == 0 = do
      tell ["Zero!!! with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdx b (a `mod` b)


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs (left - (right + n)) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

fibAndLog :: Int -> Writer [String] Int
fibAndLog 1 = writer (1, ["1st"])
fibAndLog 2 = writer (1, ["2nd"])
fibAndLog x = do
  l <- fibAndLog (x - 1)
  r <- fibAndLog (x - 3)
  let result = l + r
  writer (result, [show result])
{--
fibAndLog2 :: Int -> Writer (DiffList String) Int
fibAndLog2 1 = writer (1, ["1st"])
fibAndLog2 2 = writer (1, ["2nd"])
fibAndLog2 x = do
  l <- fibAndLog2 (x - 1)
  r <- fibAndLog2 (x - 2)
  let result = l + r
  writer (result, [show result])

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  mappend (DiffList l) (DiffList r) = DiffList (\x -> (l (r x)))
-}
{--
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

add10 :: Reader Int Int
add10 = do
  x <- ask
  return $ x + 10

testReader :: Reader Int (Int, Int, Int)
testReader = do
  x <- add10
  y <- local (+100) add10
  z <- ask
  return $ (x, y, z)
-}

banana :: Maybe a -> Maybe b
banana _ = Nothing

banana2 :: Maybe Int -> Maybe Int
banana2 (Just x) = (Just (x + 1))
banana2 Nothing = Nothing

fmonad :: Int -> Int
fmonad = do
  x <- (*3)
  y <- (+3)
  return (x + y)

fmonad2 :: Int -> Int
fmonad2 = (*3) >>= (\a -> (+5) >>= (\b -> (\_ -> a + b)))

hoge :: Int -> Int
hoge = do
  a <- (*2)
  return (a + 1)

type Stack = [Int]

pop :: State Stack Int
pop = state $ (\(x:xs) -> (x, xs))

push :: Int -> State Stack ()
push x = state $ (\xs -> ((), x:xs))

stackManip :: State Stack Int
stackManip = do
  push 1
  pop

type Queue = DList Int
-- type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = state (\q -> ((), snoc q x))

dequeue :: State Queue Int
dequeue = state (\q -> (Data.DList.head q, Data.DList.tail q))

{--
enqueue :: Int -> State Queue ()
enqueue x = state (\q -> ((), q ++ [x]))

dequeue :: State Queue Int
dequeue = state (\xs -> if (length xs > 0) then (head xs, tail xs) else (-1, []))
--}
