module Chess.Core.MovesSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import           Chess.Core.Moves
import qualified Data.Maybe        as DM

spec :: Spec
spec = describe "Pawn basic moves" $ do
    it "consist of forward two spots" $ do
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 1 0
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerOrientation = Down
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn

        length ms `shouldBe` 2

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 0) coords `shouldBe` True
        elem (Coord 3 0) coords `shouldBe` True

    it "can move forward" $ do
        -- initial board setup
        let emptyBoard = initBoard 2 2 defaultSpaceBuilder
            originCoord = Coord 0 0
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerOrientation = Up
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 0
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldNotBe` Nothing

        -- verify spaces still present
        let origSpace, destSpace :: Maybe Space
            origSpace = fetchSpace (DM.fromJust newBoard) originCoord
            destSpace = fetchSpace (DM.fromJust newBoard) destCoord

        origSpace `shouldNotBe` Nothing
        destSpace `shouldNotBe` Nothing

        -- verify piece moved
        let origSpace', destSpace' :: Space
            origSpace' = DM.fromJust origSpace
            destSpace' = DM.fromJust destSpace

        spacePiece origSpace' `shouldBe` Nothing
        spacePiece destSpace' `shouldNotBe` Nothing
