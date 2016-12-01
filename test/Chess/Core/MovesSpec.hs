module Chess.Core.MovesSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import           Chess.Core.Moves
import qualified Data.Maybe        as DM
import qualified Debug.Trace       as T

spec :: Spec
spec = describe "Pawn" $ do
  
    it "valid moves consists of forward two spots when unobstructed" $ do
      
        -- initial board setupo
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 2

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 1) coords `shouldBe` True

    it "no valid moves when facing edge of board" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = North
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 0

    it "can move to valid space" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
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

    it "cannot move to invalid space" $ do

        -- initial board setup
        let emptyBoard = initBoard 2 2 defaultSpaceBuilder
            originCoord = Coord 0 0
            pawn = buildPiece (buildPieceId originCoord)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldBe` Nothing

