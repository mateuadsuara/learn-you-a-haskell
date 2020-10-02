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
    take' `behavesLikeB` take
  describe "drop' behaves like drop" $
    drop' `behavesLikeB` drop
  describe "elem' behaves like elem" $
    elem' `behavesLikeB` elem
  describe "replicate' behaves like replicate" $
    replicate' `behavesLikeB` replicate
  describe "+++ behaves like ++" $
    (+++) `behavesLike2` (++)
  describe "quicksort behaves like sort" $
    quicksort `behavesLike` Data.List.sort
  describe "zipWith' behaves like zipWith" $
    zipWith' (+) `behavesLike2` zipWith (+)
  describe "flip' behaves like flip" $
    it "returns the same" $
      property $ \(Positive (x :: Int)) (y :: Int) ->
        (flip' div x y) == (flip div x y)
  describe "map' behaves like map" $
    map' (*2) `behavesLike` map (*2)
  describe "filter' behaves like filter" $
    filter' odd `behavesLike` filter odd
  describe "takeWhile' behaves like takeWhile" $
    takeWhile' (<5) `behavesLike` takeWhile (<5)

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

reimplemented `behavesLikeB` original =
  it "returns the same" $
    property $ \(n :: Int) (xs :: [Int]) ->
      (reimplemented n xs) == (original n xs)

reimplemented `behavesLike2` original =
  it "returns the same" $
    property $ \(xs :: [Int]) (ys :: [Int]) ->
      (reimplemented xs ys) == (original xs ys)

errorsOnEmpty fn =
  evaluate (fn []) `shouldThrow` anyErrorCall
