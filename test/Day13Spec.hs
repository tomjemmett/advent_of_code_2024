module Day13Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 day13TestInput `shouldBe` ["480", "875318608908"]

  it "Actual" $ do
    withFile
      "inputs/day13.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day13 actualInput `shouldBe` ["27157", "104015411578548"]
      )