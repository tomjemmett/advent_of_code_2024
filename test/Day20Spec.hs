module Day20Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 20" $ do
  it "Sample" $ do
    day20 day20TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day20.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day20 actualInput `shouldBe` []
      )