module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "head'" $ do
    it "works as head for non-empty lists" $
      property $ forAll nonEmptyLists $ \xs ->
        trace ("checking head' and head return the same for " ++ show xs) $
        (head xs) == (head' xs)
        where nonEmptyLists :: Gen [Int]
              nonEmptyLists = (arbitrary `suchThat` (not . null))
