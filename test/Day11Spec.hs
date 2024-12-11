module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 day11TestInput `shouldBe` ["55312", "65601038650482"]

  it "Actual" $ do
    withFile
      "inputs/day11.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day11 actualInput `shouldBe` ["224529", "266820198587914"]
      )