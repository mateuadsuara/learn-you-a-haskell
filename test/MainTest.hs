module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

  describe "head'" $ do
    it "throws an exception if used with an empty list like head does" $ do
      (head [])  `shouldThrowError` "Prelude.head: empty list"
      (head' []) `shouldThrowError` "empty list"

    it "works as head for non-empty lists" $
      property $ forAll nonEmptyLists $ \xs ->
        (head xs) == (head' xs)

  describe "tail'" $ do
    it "throws an exception if used with an empty list like tail does" $ do
      (tail [])  `shouldThrowError` "Prelude.tail: empty list"
      (tail' []) `shouldThrowError` "empty list"

    it "works as head for non-empty lists" $
      property $ forAll nonEmptyLists $ \xs ->
        (tail xs) == (tail' xs)

nonEmptyLists :: Gen [Int]
nonEmptyLists = (arbitrary `suchThat` (not . null))

shouldThrowError :: a -> String -> IO ()
shouldThrowError result expectedErrorDescription =
  evaluate result `shouldThrow` errorCall expectedErrorDescription
