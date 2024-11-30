module Day16Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    day16 day16TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day16.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day16 actualInput `shouldBe` []
      )