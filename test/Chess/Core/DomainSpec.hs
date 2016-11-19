module Chess.Core.DomainSpec ( spec ) where

import           Test.Hspec
import Chess.Core.Domain
import qualified Data.List as DL

spec :: Spec
spec = describe "board" $ do
    describe "new (default)" $ do
        it "1 by 1" $ do
          let (Board spaces) = initBoard 1 1 defaultSpaceBuilder
              expectedCoord = Coord 1 1
          x <- (DL.length spaces)
          x `shouldBe` (1::Int)
          coord (head spaces) `shouldBe` expectedCoord
        it "colors alternate" $ do
            let board = initBoard 2 2
                color1 = White
                color2 = Black
                color3 = White
                color4 = Black
                coord1 = Coord 1 1
                coord2 = Coord 1 2
                coord3 = Coord 2 1
                coord4 = Coord 2 2
            color (fetchSpace board coord1) `shouldBe` color1
            color (fetchSpace board coord2) `shouldBe` color2
            color (fetchSpace board coord3) `shouldBe` color3
            color (fetchSpace board coord4) `shouldBe` color4
            
