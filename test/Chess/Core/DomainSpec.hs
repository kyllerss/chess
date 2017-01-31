module Chess.Core.DomainSpec ( spec ) where

import Import
import           Test.Hspec
import           Chess.TestUtils
import           Chess.Core.Domain
import           Chess.Core.Moves
import Data.Map (size)
import Data.Maybe (fromJust)

spec :: Spec
spec = describe "board" $ do
    describe "when created" $ do
      describe "when 1 x 1" $ do
        it "has right dimensions" $ do
            let Board {spacesMap = spaces} = initBoard 1 1 defaultSpaceBuilder
            return (size spaces) >>=
                (`shouldBe` (1 :: Int))
        it "has coordinate (1, 1)" $ do
            let Board {spacesMap = spsMap} = initBoard 1 1 defaultSpaceBuilder
                space = lookup (Coord 0 0) spsMap
            space `shouldNotBe` Nothing
            spaceCoord (fromJust space) `shouldSatisfy`
                (\(Coord x y) -> x == 0 && y == 0)
        it "has no pieces" $ do
            let board = initBoard 1 1 defaultSpaceBuilder
                coord = Coord 0 0
            (fetchSpace coord board) `shouldSatisfy`
                (\(Just s) -> isNothing $ spacePiece (s :: Space))
        it "can have piece added" $ do

            -- initial empty board setup
            let board :: Board
                board = initBoard 2 2 defaultSpaceBuilder
                coord = Coord 0 1

                space :: Maybe Space
                space = fetchSpace coord board

                player = Player { playerName = "dummy"
                                 , playerType = Human
                                 , playerId = 1
                                 , playerDirection = North}
                pawn = buildPiece (buildPieceId coord) Pawn White player Nothing

            space `shouldNotBe` Nothing

            -- verify target space empty
            let space' :: Space
                space' = fromJust space
                piece :: Maybe Piece
                piece = spacePiece $ space'

            piece `shouldBe` Nothing

            -- add piece to board
            let newBoard :: Maybe Board
                newBoard = addPieceToBoard pawn coord board

            newBoard `shouldNotBe` Nothing

            -- verify target space no longer empty
            let newSpace :: Maybe Space
                newSpace = fetchSpace coord (fromJust newBoard)

            newSpace `shouldNotBe` Nothing

            -- verify space's piece
            let piece' :: Maybe Piece
                piece' = spacePiece $ fromJust newSpace

            piece' `shouldNotBe` Nothing
            pieceId (fromJust piece') `shouldBe` pieceId pawn

            let po :: Maybe Coord
                po = pieceOrigin (fromJust piece') 

            po `shouldNotBe` Nothing
            fromJust po `shouldBe` coord

      describe "when 2 x 2" $ do
        it "has right dimensions" $ do
            let Board {spacesMap = spaces} = initBoard 2 2 defaultSpaceBuilder
            return (size spaces) >>= (`shouldBe` (4 :: Int))
        it "has alternating colors by rows" $ do
            let board = initBoard 2 2 defaultSpaceBuilder
                coord1 = Coord 0 0
                coord2 = Coord 0 1
                coord3 = Coord 1 0
                coord4 = Coord 1 1
            (fetchSpace coord1 board) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == White)
            (fetchSpace coord2 board) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == Black)
            (fetchSpace coord3 board) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == Black)
            (fetchSpace coord4 board) `shouldSatisfy`
                (\(Just s) -> (spaceColor (s :: Space)) == White)
                
    it "records move history" $ do

        let emptyBoard = initBoard 3 1 defaultSpaceBuilder
            originCoord = Coord 0 0
            king = buildTestPiece 1 King 1 South
        
            board = Just emptyBoard >>= addPieceToBoard king originCoord

        boardMoves <$> board `shouldBe` Just []

        let newBoard = board >>=
                       move (pieceId king) (Coord 1 0) >>=
                       move (pieceId king) (Coord 2 0)

        boardMoves <$> newBoard `shouldBe` Just [((pieceId king), Coord 2 0), ((pieceId king), Coord 1 0)]
