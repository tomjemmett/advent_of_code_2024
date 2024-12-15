module Day15Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 day15TestInput `shouldBe` ["10092", "9021"]

  it "Actual" $ do
    withFile
      "inputs/day15.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day15 actualInput `shouldBe` ["1441031", "1425169"]
      )