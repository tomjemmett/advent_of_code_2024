module Day18Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 18" $ do
  it "Actual" $ do
    withFile
      "inputs/day18.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day18 actualInput `shouldBe` ["360", "58,62"]
      )