module Chess.Core.Logic.BoardBuilderSpec (spec) where

import Import
import Test.Hspec
import Chess.Core.Domain.Base
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import Chess.Core.Logic.BoardBuilder
import Data.Maybe (fromJust)

spec :: Spec
spec = describe "Board builder" $ do

  let player1 :: Player
      player1 = Player { playerName = pack "Player 1"
                       , playerId = 1
                       , playerType = Human
                       , playerDirection = North
                       }
      player2 :: Player
      player2 = Player { playerName = pack "Player 2"
                       , playerId = 2
                       , playerType = Human
                       , playerDirection = South
                       }
      
  it "can construct simple 1x1" $ do

    let colorMatrix :: BoardMatrix
        colorMatrix = [['v']]

        nullMatrix :: BoardMatrix
        nullMatrix = [['.']]

        playerPieceMatrix :: [PlayerMatrix]
        playerPieceMatrix = [(player1, nullMatrix)]
    
        board :: Maybe Board
        board = constituteBoard 1 1 colorMatrix nullMatrix playerPieceMatrix

    board `shouldNotBe` Nothing

    let space :: Maybe Space
        space = fetchSpace (Coord 0 0) (fromJust board)

    space `shouldBe` Just (Void (Coord 0 0))

  it "can construct diverse 3x3" $ do

    let colorMatrix :: BoardMatrix
        colorMatrix = [ ['b', 'v', 'w']
                      , ['w', 'b', 'v']
                      , ['v', 'w', 'b'] ]

        spacePropsMatrix :: BoardMatrix
        spacePropsMatrix = [ ['p', 'p', 'p']
                           , ['.', '.', '.']
                           , ['.', 'p', '.'] ]

        player1Matrix = [ ['.', '.', '.']
                        , ['.', 'p', '.']
                        , ['r', 'k', 'q'] ]
        player2Matrix = [ ['b', 'k', 'n']
                        , ['p', '.', 'p']
                        , ['.', '.', '.'] ]
    
        playerPieceMatrix :: [PlayerMatrix]
        playerPieceMatrix = [ (player1, player1Matrix)
                            , (player2, player2Matrix) ]
    
        board :: Maybe Board
        board = constituteBoard 3 3 colorMatrix spacePropsMatrix playerPieceMatrix

    board `shouldNotBe` Nothing

    let space1 = fetchSpace (Coord 0 0) (fromJust board)
        space2 = fetchSpace (Coord 0 1) (fromJust board)
        space3 = fetchSpace (Coord 0 2) (fromJust board)
        space4 = fetchSpace (Coord 1 0) (fromJust board)
        space5 = fetchSpace (Coord 1 1) (fromJust board)
        space6 = fetchSpace (Coord 1 2) (fromJust board)
        space7 = fetchSpace (Coord 2 0) (fromJust board)
        space8 = fetchSpace (Coord 2 1) (fromJust board)
        space9 = fetchSpace (Coord 2 2) (fromJust board)

    space1 `shouldBe` Just Space{ spaceColor = Black
                                , spaceCoord = (Coord 0 0)
                                , spaceSideEffectType = Just PawnPromotion
                                , spacePiece = Just Piece{ pieceColor = White
                                                         , pieceType = Bishop
                                                         , piecePlayer = player2
                                                         , pieceId = buildPieceId (Coord 0 0)
                                                         , pieceOrigin = Just (Coord 0 0)
                                                         , pieceMoved = False}}
    space2 `shouldBe` Just (Void (Coord 0 1))
    space3 `shouldBe` Just Space{ spaceColor = White
                                , spaceCoord = (Coord 0 2)
                                , spaceSideEffectType = Just PawnPromotion
                                , spacePiece = Just Piece{ pieceColor = White
                                                         , pieceType = Knight
                                                         , piecePlayer = player2
                                                         , pieceId = buildPieceId (Coord 0 2)
                                                         , pieceOrigin = Just (Coord 0 2)
                                                         , pieceMoved = False}}
    space4 `shouldBe` Just Space{ spaceColor = White
                                , spaceCoord = (Coord 1 0)
                                , spaceSideEffectType = Nothing
                                , spacePiece = Just Piece{ pieceColor = Black
                                                         , pieceType = Pawn
                                                         , piecePlayer = player2
                                                         , pieceId = buildPieceId (Coord 1 0)
                                                         , pieceOrigin = Just (Coord 1 0)
                                                         , pieceMoved = False}}
    space5 `shouldBe` Just Space{ spaceColor = Black
                                , spaceCoord = (Coord 1 1)
                                , spaceSideEffectType = Nothing
                                , spacePiece = Just Piece{ pieceColor = Black
                                                         , pieceType = Pawn
                                                         , piecePlayer = player2
                                                         , pieceId = buildPieceId (Coord 1 1)
                                                         , pieceOrigin = Just (Coord 1 1)
                                                         , pieceMoved = False}}
    space6 `shouldBe` Just (Void (Coord 1 2))
    space7 `shouldBe` Just (Void (Coord 2 0))
    space8 `shouldBe` Just Space{ spaceColor = White
                                , spaceCoord = (Coord 2 1)
                                , spaceSideEffectType = Just PawnPromotion
                                , spacePiece = Just Piece{ pieceColor = White
                                                         , pieceType = King
                                                         , piecePlayer = player1
                                                         , pieceId = buildPieceId (Coord 2 1)
                                                         , pieceOrigin = Just (Coord 2 1)
                                                         , pieceMoved = False}}
    space9 `shouldBe` Just Space{ spaceColor = Black
                                , spaceCoord = (Coord 2 2)
                                , spaceSideEffectType = Nothing
                                , spacePiece = Just Piece{ pieceColor = White
                                                         , pieceType = Queen
                                                         , piecePlayer = player1
                                                         , pieceId = buildPieceId (Coord 2 2)
                                                         , pieceOrigin = Just (Coord 2 2)
                                                         , pieceMoved = False}}

