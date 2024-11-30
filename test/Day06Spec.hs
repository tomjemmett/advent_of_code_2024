module Day06Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    day06 day06TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day06.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day06 actualInput `shouldBe` []
      )