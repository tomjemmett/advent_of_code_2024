module Day20Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 20" $ do
  it "Actual" $ do
    withFile
      "inputs/day20.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day20 actualInput `shouldBe` ["1197", "944910"]
      )