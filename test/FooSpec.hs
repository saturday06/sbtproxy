module FooSpec where

import           Control.Monad
import           Foo
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Control.Monad.Writer
import           Control.Monad.Reader
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.DList
-- import Prelude

spec :: Spec
spec = do
  describe "foo" $
    it "works!" $
       foo 2 `shouldBe` 4

  describe "guard" $
    it "works!" $ do
      (guard (5 > 2) >> return "cool") `shouldBe` ["cool"]
      (guard (1 > 2) >> return "cool") `shouldBe` []

  describe "sevensOnly" $ do
    it "returns sevens" $ do
      sevensOnly `shouldBe` [7, 17, 27, 37, 47]

  describe "moveKnight" $ do
    it "works!" $ do
      (moveKnight (1, 1)) `shouldBe` [(3, 2), (2, 3)]
{--
  describe "in3" $ do
    it "works!" $ do
      (in3 (1, 1)) `shouldBe` []
-}
  describe "canReachIn3" $ do
    it "works!" $ do
      (canReachIn3 (1, 1) (2, 3)) `shouldBe` True
      (canReachIn3 (1, 1) (3, 2)) `shouldBe` True

  describe "canReachIn3WithKeiro" $ do
--    it "works!" $ do
--      (in3x (6, 2) (6, 1)) `shouldBe`
--        [[(8,1),(7,3)],[(4,1),(5,3)],[(7,4),(5,3)],[(7,4),(8,2)],[(5,4),(7,3)],[(5,4),(4,2)]]

    it "is big gang" $ do
      (isBigGang 9) `shouldBe` (False, "Big!?")
      (isBigGang 10) `shouldBe` (True, "Big!?")


    it "gcd'" $ do
      gcd' 8 3 `shouldBe` 1

    it "gcdx" $ do
      fst (runWriter (gcdx 8 3)) `shouldBe` 1
--      snd (runWriter (gcdx 8 3)) `shouldBe` [""]


    it "works" $ do
      (Just 3 `applyMaybe` (\x -> Just(x + 1))) `shouldBe` Just 4
      (Just 9 >>= (\x -> return (x * 10))) `shouldBe` Just 90
      let (Just x) = Just 10
      x `shouldBe` 10

    it "works" $ do
      landLeft 100 (1, 2) `shouldBe` Nothing


    it "fib" $ do
      (fib 6) `shouldBe` 8
      (fib 10) `shouldBe` 55

{--
    it "runWriter" $ do
      runWriter (fibAndLog 1) `shouldBe` (1, ["1st"])
      runWriter (fibAndLog 2) `shouldBe` (1, ["2nd"])
      runWriter (fibAndLog 5) `shouldBe` (5, ["2nd", "1st", "2", "2nd", "3", "2nd", "1st", "2", "5"])



--}
{--
    it "runWriter2" $ do
      runWriter (fibAndLog2 1) `shouldBe` (1, ["1st"])
      runWriter (fibAndLog2 2) `shouldBe` (1, ["2nd"])
      runWriter (fibAndLog2 5) `shouldBe` (5, ["2nd", "1st", "2", "2nd", "3", "2nd", "1st", "2", "5"])
--}

{--
    it "reader1" $ do
      let f = (*5)
      let g = (+3)
      (fmap f g) 8 `shouldBe` 55

    it "reader2" $ do
      (runReader testReader 1) `shouldBe` (11, 111, 1)


toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList xs) = xs []
-}

    it "works!" $ do
      (Just (0, 0) >>= landRight 3) `shouldBe` Just (0, 3)

    it "works!" $ do
      (+) <$> Just 3 <*> Just 4 `shouldBe` Just 7
      (\a b c d -> a + b + c + d) <$> Just 3 <*> Just 4 <*> Just 5 <*> Just 6 `shouldBe` Just 18
      Identity 1 `shouldBe` Identity 1
      ((Identity 10) >>= (\x -> Identity (x + 5))) `shouldBe` Identity 15


    it "works!" $ do
      fmonad 5 `shouldBe` 23

    it "works!" $ do
      ((runState $ do
        enqueue 1
        dequeue
        enqueue 2
        enqueue 3
        dequeue
        enqueue 4
        enqueue 6
        dequeue
        ) (fromList [1])) `shouldBe` (
          2, (fromList [3, 4, 6]))
