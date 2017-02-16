module Handler.NewGame where

import Import
import Chess.Core.Domain

getNewGameR :: Handler Value
getNewGameR = do
  let
    player1, player2 :: Player
    player1 = Player { playerName = pack "Player 1"
                     , playerId = 1
                     , playerType = Human
                     , playerDirection = North
                     }
    player2 = Player { playerName = pack "Player 2"
                     , playerId = 2
                     , playerType = Human
                     , playerDirection = South
                     }
    board :: Board
    board = initStandardBoard player1 player2
    gameState :: GameState
    gameState = GameState{ board = board
                         , moves = []
                         , players = [player1, player2]
                         , playerTurn = player1
                         , token = "" }
  return $ toJSON gameState
      
 
