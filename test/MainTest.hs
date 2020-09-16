module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "head' behaves like head" $ do
    head' `behavesLike` head
  describe "tail' behaves like tail" $ do
    tail' `behavesLike` tail

behavesLike reimplemented original = do
  it "throws an exception if used with an empty list" $ do
    (reimplemented []) `shouldThrowError` "empty list"
    evaluate (original []) `shouldThrow` anyErrorCall
  it "returns the same for non-empty lists" $ do
    property $ forAll nonEmptyLists $ \xs ->
      (reimplemented xs) == (original xs)

nonEmptyLists :: Gen [Int]
nonEmptyLists = (arbitrary `suchThat` (not . null))

shouldThrowError :: a -> String -> IO ()
shouldThrowError result expectedErrorDescription =
  evaluate result `shouldThrow` errorCall expectedErrorDescription
