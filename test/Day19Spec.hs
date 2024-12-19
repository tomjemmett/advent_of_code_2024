module Day19Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    day19 day19TestInput `shouldBe` ["6", "16"]

  it "Actual" $ do
    withFile
      "inputs/day19.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day19 actualInput `shouldBe` ["269", "758839075658876"]
      )