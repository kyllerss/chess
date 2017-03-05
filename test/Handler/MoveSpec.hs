module Handler.MoveSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "postMoveR" $ do
      it "is dummy" $ do
        get RobotsR
        statusIs 200
        --error "Spec not implemented: postMoveR"

