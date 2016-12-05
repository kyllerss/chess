module Chess.Core.MovesSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import           Chess.Core.Moves
import qualified Data.Maybe        as DM
import qualified Debug.Trace       as T

spec :: Spec
spec = describe "Pieces" $ do
  describe "Pawn" $ do
  
    it "valid moves consists of forward two spots when unobstructed" $ do
      
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

    it "can move to valid space vertically when unmoved" $ do
      
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

    it "can move only once forward when previously moved" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 0 4
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
        let destCoord = Coord 1 4
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldNotBe` Nothing

        -- verify piece marked as moved
        let destSpace :: Maybe Space
            destSpace = fetchSpace (DM.fromJust newBoard) destCoord

        destSpace `shouldNotBe` Nothing

        let destSpace' :: Space
            destSpace' = DM.fromJust destSpace
            movedPiece = spacePiece destSpace'

        movedPiece `shouldNotBe` Nothing

        let movedPawn :: Piece
            movedPawn = DM.fromJust movedPiece

        pieceMoved movedPawn `shouldBe` True

        let ms :: [Move]
            ms = validMoves (DM.fromJust newBoard) movedPawn (spaceCoord destSpace')

        length ms `shouldBe` 1
        (spaceCoord . moveSpace) (ms !! 0) `shouldBe` Coord 2 4

    it "valid moves contains diagonals when opp present" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 1 4
            pawn = buildPiece (PieceId 1)
                              Pawn
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            pawn2 = buildPiece (PieceId 2)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = North
                                       })

            pawn3 = buildPiece (PieceId 3)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = North
                                       })


            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (pawn, originCoord)
                          , (pawn2, Coord 2 5)
                          , (pawn3, Coord 2 3)]

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 2 3) coords `shouldBe` True
        elem (Coord 2 5) coords `shouldBe` True

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

  describe "Rook" $ do

    it "has vaid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (buildPieceId originCoord)
                              Rook
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard rook originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 16

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        elem (Coord 8 4) coords `shouldBe` True
        elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        elem (Coord 4 8) coords `shouldBe` True

    it "has vaid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (PieceId 1)
                              Rook
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            pawn1 = buildPiece (PieceId 2)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn2 = buildPiece (PieceId 3)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn3 = buildPiece (PieceId 4)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn4 = buildPiece (PieceId 5)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- elem (Coord 0 4) coords `shouldBe` True
        -- elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        -- elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        -- elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        -- elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True

    it "has vaid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (PieceId 1)
                              Rook
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            pawn1 = buildPiece (PieceId 2)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn2 = buildPiece (PieceId 3)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn3 = buildPiece (PieceId 4)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn4 = buildPiece (PieceId 5)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 12

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True

  describe "Bishop" $ do

    it "has vaid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (buildPieceId originCoord)
                              Bishop
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            board :: Maybe Board
            board = addPieceToBoard emptyBoard rook originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 16

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 0 0) coords `shouldBe` True
        elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        elem (Coord 7 1) coords `shouldBe` True
        elem (Coord 8 0) coords `shouldBe` True
        elem (Coord 0 8) coords `shouldBe` True
        elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        elem (Coord 7 7) coords `shouldBe` True
        elem (Coord 8 8) coords `shouldBe` True

    it "has vaid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (PieceId 1)
                              Bishop
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            pawn1 = buildPiece (PieceId 2)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn2 = buildPiece (PieceId 3)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn3 = buildPiece (PieceId 4)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            pawn4 = buildPiece (PieceId 5)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 1
                                       , playerDirection = South
                                       })

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 2 2)
                          , (pawn2, Coord 2 6)
                          , (pawn3, Coord 6 2)
                          , (pawn4, Coord 6 6)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        --elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        --elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        --elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        --elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True

    it "has vaid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildPiece (PieceId 1)
                              Bishop
                              White
                              (Player { playerName = "dummy"
                                      , playerType = Human
                                      , playerId = 1
                                      , playerDirection = South
                                      })

            pawn1 = buildPiece (PieceId 2)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn2 = buildPiece (PieceId 3)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn3 = buildPiece (PieceId 4)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            pawn4 = buildPiece (PieceId 5)
                               Pawn
                               White
                               (Player { playerName = "dummy"
                                       , playerType = Human
                                       , playerId = 2
                                       , playerDirection = South
                                       })

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 2 2)
                          , (pawn2, Coord 2 6)
                          , (pawn3, Coord 6 2)
                          , (pawn4, Coord 6 6)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True
