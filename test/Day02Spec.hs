module Day02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 2" $ do
  it "Sample" $ do
    day02 day02TestInput `shouldBe` ["2", "4"]

  it "Actual" $ do
    withFile
      "inputs/day02.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day02 actualInput `shouldBe` ["299", "364"]
      )