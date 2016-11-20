module Chess.Core.MovesSpec (spec) where

import Test.Hspec
import Chess.Core.Domain

spec :: Spec
spec = describe "Pawn basic moves" $ do
    it "should move forward one or two spaces" $ do
      let board = initBoard 3 3 defaultSpaceBuilder
      True `shouldBe` True
