module Chess.Core.DomainSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import           Chess.Core.Moves
import qualified Data.List         as DL
import qualified Data.Maybe        as DM
import qualified Data.Map          as M

spec :: Spec
spec = describe "board" $ do
    describe "when created" $ do
      describe "when 1 x 1" $ do
        it "has right dimensions" $ do
            let (Board spaces) = initBoard 1 1 defaultSpaceBuilder
            return (M.size spaces) >>=
                (`shouldBe` (1 :: Int))
        it "has coordinate (1, 1)" $ do
            let (Board spsMap) = initBoard 1 1 defaultSpaceBuilder
                space = M.lookup (Coord 0 0) spsMap
            space `shouldNotBe` Nothing
            spaceCoord (DM.fromJust space) `shouldSatisfy`
                (\(Coord x y) -> x == 0 && y == 0)
        it "has no pieces" $ do
            let board = initBoard 1 1 defaultSpaceBuilder
                coord = Coord 0 0
            (fetchSpace board coord) `shouldSatisfy`
                (\(Just s) -> DM.isNothing $ spacePiece (s :: Space))
        it "can have piece added" $ do

            -- initial empty board setup
            let board :: Board
                board = initBoard 2 2 defaultSpaceBuilder
                coord' = Coord 0 1

                space' :: Maybe Space
                space' = fetchSpace board coord'

                player' = Player { playerName = "dummy"
                                 , playerType = Human
                                 , playerId = 1
                                 , playerDirection = North}
                pawn = buildPiece (buildPieceId coord') Pawn White player'

            space' `shouldNotBe` Nothing

            -- verify target space empty
            let space'' :: Space
                space'' = DM.fromJust space'
                piece' :: Maybe Piece
                piece' = spacePiece $ space''

            piece' `shouldBe` Nothing

            -- add piece to board
            let newBoard :: Maybe Board
                newBoard = addPieceToBoard board pawn coord'

            newBoard `shouldNotBe` Nothing

            -- verify target space no longer empty
            let newSpace' :: Maybe Space
                newSpace' = fetchSpace (DM.fromJust newBoard) coord'

            newSpace' `shouldNotBe` Nothing

            -- verify space's piece
            let piece'' :: Maybe Piece
                piece'' = spacePiece $ DM.fromJust newSpace'

            piece'' `shouldNotBe` Nothing
            DM.fromJust piece'' `shouldBe` pawn

      describe "when 2 x 2" $ do
        it "has right dimensions" $ do
            let (Board spaces) = initBoard 2 2 defaultSpaceBuilder
            return (M.size spaces) >>=
                (`shouldBe` (4 :: Int))
        it "has alternating colors by rows" $ do
            let board = initBoard 2 2 defaultSpaceBuilder
                coord1 = Coord 0 0
                coord2 = Coord 0 1
                coord3 = Coord 1 0
                coord4 = Coord 1 1
            (fetchSpace board coord1) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == White)
            (fetchSpace board coord2) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == Black)
            (fetchSpace board coord3) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == Black)
            (fetchSpace board coord4) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == White)
