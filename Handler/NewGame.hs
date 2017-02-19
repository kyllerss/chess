module Handler.NewGame where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Player
import qualified Data.Maybe as DM

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
    gameState :: Maybe GameState
    gameState = initGame Standard [player1, player2] 
  return $ toJSON $ DM.fromJust gameState
      
 
