{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "head' behaves like head" $
    head' `errorsOnEmptyAndBehavesLike` head
  describe "tail' behaves like tail" $
    tail' `errorsOnEmptyAndBehavesLike` tail
  describe "last' behaves like last" $
    last' `errorsOnEmptyAndBehavesLike` last
  describe "init' behaves like init" $
    init' `errorsOnEmptyAndBehavesLike` init
  describe "maximum' behaves like maximum" $
    maximum' `errorsOnEmptyAndBehavesLike` maximum
  describe "minimum' behaves like minimum" $
    minimum' `errorsOnEmptyAndBehavesLike` minimum
  describe "cycle' behaves like cycle" $
    let l = 1000 in
      (take l . cycle') `errorsOnEmptyAndBehavesLike` (take l . cycle)
  describe "length' behaves like length" $
    length' `behavesLike` length
  describe "null' behaves like null" $
    null' `behavesLike` null
  describe "reverse' behaves like reverse" $
    reverse' `behavesLike` reverse
  describe "sum' behaves like sum" $
    sum' `behavesLike` sum
  describe "product' behaves like product" $
    product' `behavesLike` product

reimplemented `errorsOnEmptyAndBehavesLike` original = do
  it "errors if used with an empty list" $ do
    (reimplemented []) `shouldThrowError` "empty list"
    evaluate (original []) `shouldThrow` anyErrorCall
  it "returns the same for non-empty lists" $
    property $ \(NonEmpty (xs :: [Int])) ->
      (reimplemented xs) == (original xs)

reimplemented `behavesLike` original =
  it "returns the same" $
    property $ \(xs :: [Int]) ->
      (reimplemented xs) == (original xs)

shouldThrowError :: a -> String -> IO ()
shouldThrowError result expectedErrorDescription =
  evaluate result `shouldThrow` errorCall expectedErrorDescription
