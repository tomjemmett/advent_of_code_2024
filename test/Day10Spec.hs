module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 day10TestInput `shouldBe` ["36", "81"]

  it "Actual" $ do
    withFile
      "inputs/day10.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day10 actualInput `shouldBe` ["531", "1210"]
      )