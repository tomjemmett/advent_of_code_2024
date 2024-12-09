module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    day09 day09TestInput `shouldBe` ["1928", "2858"]

  it "Actual" $ do
    withFile
      "inputs/day09.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day09 actualInput `shouldBe` ["6310675819476", "6335972980679"]
      )