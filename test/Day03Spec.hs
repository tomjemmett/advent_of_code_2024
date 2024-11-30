module Day03Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    day03 day03TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day03.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day03 actualInput `shouldBe` []
      )