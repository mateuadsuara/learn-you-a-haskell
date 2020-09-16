module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "head'" $ do
    it "throws an exception if used with an empty list like head does" $ do
      (head [])  `shouldThrowError` "Prelude.head: empty list"
      (head' []) `shouldThrowError` "empty list"
    it "works as head for non-empty lists" $
      property $ forAll nonEmptyLists $ \xs ->
        trace ("checking head' and head return the same for " ++ show xs) $
        (head xs) == (head' xs)
        where nonEmptyLists :: Gen [Int]
              nonEmptyLists = (arbitrary `suchThat` (not . null))

shouldThrowError :: a -> String -> IO ()
shouldThrowError result expectedErrorDescription =
  evaluate result `shouldThrow` errorCall expectedErrorDescription
