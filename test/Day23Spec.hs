module Day23Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 23" $ do
  it "Actual" $ do
    withFile
      "inputs/day23.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day23 actualInput `shouldBe` ["1370"]
      )