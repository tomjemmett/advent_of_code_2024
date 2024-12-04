module Day04Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    let [p1, p2] = day04TestInput
    day04 p1 `shouldBe` ["18", "3"]
    day04 p2 `shouldBe` ["0", "9"]

  it "Actual" $ do
    withFile
      "inputs/day04.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day04 actualInput `shouldBe` ["2545", "1886"]
      )