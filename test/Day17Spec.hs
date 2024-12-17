module Day17Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 17" $ do
  it "Sample" $ do
    let [t1, t2] = day17TestInput
    day17 t1 `shouldBe` ["4,6,3,5,6,3,5,2,1,0", "-"]
    day17 t2 `shouldBe` ["5,7,3,0", "117440"]

  it "Actual" $ do
    withFile
      "inputs/day17.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day17 actualInput `shouldBe` ["6,7,5,2,1,3,5,1,7", "216549846240877"]
      )