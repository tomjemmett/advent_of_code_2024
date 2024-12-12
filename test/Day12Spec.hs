module Day12Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 day12TestInput `shouldBe` ["1930", "1206"]

  it "Actual" $ do
    withFile
      "inputs/day12.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day12 actualInput `shouldBe` ["1424472", "870202"]
      )