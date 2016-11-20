module Chess.Core.DomainSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import qualified Data.List         as DL

spec :: Spec
spec = describe "board" $ do
    describe "when created" $ do
        it "is 1 by 1" $ do
            let (Board spaces) = initBoard 1 1 defaultSpaceBuilder
            return (DL.length spaces) >>=
                (`shouldBe` (1 :: Int))
        it "has coordinate (1, 1)" $ do
            let (Board spaces) = initBoard 1 1 defaultSpaceBuilder
            coord (head spaces) `shouldSatisfy`
                (\(Coord x y) -> x == 1 && y == 1)
        it "colors alternate by rows" $ do
            let board = initBoard 2 2 defaultSpaceBuilder
                coord1 = Coord 1 1
                coord2 = Coord 1 2
                coord3 = Coord 2 1
                coord4 = Coord 2 2
            (fetchSpace board coord1) `shouldSatisfy`
                (\(Just s) -> (color (s :: Space)) == White)
            (fetchSpace board coord2) `shouldSatisfy`
                (\(Just s) -> (color (s :: Space)) == Black)
            (fetchSpace board coord3) `shouldSatisfy`
                (\(Just s) -> (color (s :: Space)) == Black)
            (fetchSpace board coord4) `shouldSatisfy`
                (\(Just s) -> (color (s :: Space)) == White)
