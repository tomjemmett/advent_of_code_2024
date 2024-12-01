module Day01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 1" $ do
  it "Sample" $ do
    day01 day01TestInput `shouldBe` ["11", "31"]

  it "Actual" $ do
    withFile
      "inputs/day01.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day01 actualInput `shouldBe` ["1590491", "22588371"]
      )