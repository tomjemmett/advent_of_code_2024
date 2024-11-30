module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    day09 day09TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day09.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day09 actualInput `shouldBe` []
      )