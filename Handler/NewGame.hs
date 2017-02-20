module Handler.NewGame where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Player
  
getNewGameR :: Handler Value
getNewGameR = do
  return $ traceShow (board gameState) $ toJSON gameState
  where 
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
    gameState :: GameState
    gameState = initGame Standard [player1, player2] 

      
 
