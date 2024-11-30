module Day18Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    day18 day18TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day18.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day18 actualInput `shouldBe` []
      )