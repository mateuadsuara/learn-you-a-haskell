{-# LANGUAGE ScopedTypeVariables #-}

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
    let l = 1000 in
      (take l . cycle') `errorsOnEmptyAndBehavesLike` (take l . cycle)
  describe "length' behaves like length" $ do
    length' `behavesLike` length
  describe "null' behaves like null" $ do
    null' `behavesLike` null
  describe "reverse' behaves like reverse" $ do
    reverse' `behavesLike` reverse
  describe "sum' behaves like sum" $ do
    sum' `behavesLike` sum
  describe "product' behaves like product" $ do
    product' `behavesLike` product

errorsOnEmptyAndBehavesLike reimplemented original = do
  it "errors if used with an empty list" $ do
    (reimplemented []) `shouldThrowError` "empty list"
    evaluate (original []) `shouldThrow` anyErrorCall
  it "returns the same for non-empty lists" $ do
    property $ \(NonEmpty (xs :: [Int])) ->
      (reimplemented xs) == (original xs)

behavesLike reimplemented original = do
  it "returns the same" $ do
    property $ \(xs :: [Int]) ->
      (reimplemented xs) == (original xs)

shouldThrowError :: a -> String -> IO ()
shouldThrowError result expectedErrorDescription =
  evaluate result `shouldThrow` errorCall expectedErrorDescription
