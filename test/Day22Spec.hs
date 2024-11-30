module Day22Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 day22TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` []
      )