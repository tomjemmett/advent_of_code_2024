module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 08" $ do
  it "Sample" $ do
    day08 day08TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day08.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day08 actualInput `shouldBe` []
      )