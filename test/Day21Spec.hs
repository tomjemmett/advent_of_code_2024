module Day21Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 21" $ do
  it "Sample" $ do
    day21 day21TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day21.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day21 actualInput `shouldBe` []
      )