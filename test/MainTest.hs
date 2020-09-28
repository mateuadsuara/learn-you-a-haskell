{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import ListFunctions
import TupleFunctions
import OtherFunctions

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.List (sort)

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
  describe "repeat' behaves like repeat" $
    let l = 1000 in
      (take l . repeat') `behavesLike` (take l . repeat)
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
  describe "take' behaves like take" $
    take' `behavesLike2` take
  describe "drop' behaves like drop" $
    drop' `behavesLike2` drop
  describe "elem' behaves like elem" $
    elem' `behavesLike2` elem
  describe "replicate' behaves like replicate" $
    replicate' `behavesLike2` replicate
  describe "+++ behaves like ++" $
    it "returns the same" $
      property $ \(xs :: [Int]) (ys :: [Int]) ->
        (xs +++ ys) == (xs ++ ys)
  describe "quicksort behaves like sort" $
    quicksort `behavesLike` Data.List.sort
  describe "zipWith' behaves like zipWith" $
    it "returns the same" $
      property $ \(xs :: [Int]) (ys :: [Int]) ->
        (zipWith' (+) xs ys) == (zipWith (+) xs ys)
  describe "flip' behaves like flip" $
    it "returns the same" $
      property $ \(Positive (x :: Int)) (y :: Int) ->
        (flip' div x y) == (flip div x y)
  describe "map' behaves like map" $
    it "returns the same" $
      property $ \(xs :: [Int]) ->
        (map' (*2) xs) == (map (*2) xs)

reimplemented `errorsOnEmptyAndBehavesLike` original = do
  it "errors if used with an empty list" $ do
    errorsOnEmpty reimplemented
    errorsOnEmpty original
  it "returns the same for non-empty lists" $
    property $ \(NonEmpty (xs :: [Int])) ->
      (reimplemented xs) == (original xs)

reimplemented `behavesLike` original =
  it "returns the same" $
    property $ \(xs :: [Int]) ->
      (reimplemented xs) == (original xs)

reimplemented `behavesLike2` original =
  it "returns the same" $
    property $ \(n :: Int) (xs :: [Int]) ->
      (reimplemented n xs) == (original n xs)

errorsOnEmpty fn =
  evaluate (fn []) `shouldThrow` anyErrorCall
