module Day04Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    day04 day04TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day04.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day04 actualInput `shouldBe` []
      )