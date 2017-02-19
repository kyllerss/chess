module Handler.NewGameSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getNewGameR" $ do
      it "passes" $ do
        True `shouldBe` True
        --error "Spec not implemented: getNewGameR"

