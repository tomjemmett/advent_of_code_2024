module Day24Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 day24TestInput `shouldBe` ["2024"]

  it "Actual" $ do
    withFile
      "inputs/day24.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day24 actualInput `shouldBe` ["48806532300520"]
      )