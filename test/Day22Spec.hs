module Day22Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 22" $ do
  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` ["17008280254", "2152"]
      )