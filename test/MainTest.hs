module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "head' behaves like head" $ do
    head' `errorsOnEmptyAndBehavesLike` head
  describe "tail' behaves like tail" $ do
    tail' `errorsOnEmptyAndBehavesLike` tail
  describe "last' behaves like last" $ do
    last' `errorsOnEmptyAndBehavesLike` last
  describe "init' behaves like init" $ do
    init' `errorsOnEmptyAndBehavesLike` init
  describe "maximum' behaves like maximum" $ do
    maximum' `errorsOnEmptyAndBehavesLike` maximum
  describe "minimum' behaves like minimum" $ do
    minimum' `errorsOnEmptyAndBehavesLike` minimum
  describe "cycle' behaves like cycle" $ do
    (take l . cycle') `errorsOnEmptyAndBehavesLike` (take l . cycle)
    where l = 1000

errorsOnEmptyAndBehavesLike reimplemented original = do
  it "errors if used with an empty list" $ do
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
