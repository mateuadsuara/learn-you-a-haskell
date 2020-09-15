module Main (main) where

import ListFunctions

import Test.Hspec
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "head'" $ do
    it "works as head" $
      property $ \xs ->
        trace ("checking head' and head return the same for " ++ show xs) $
        xs == [] || (head xs) == (head' xs :: Int)
