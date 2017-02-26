module Handler.NewGame where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Player
import Chess.Core.Logic.GameStatePersistence

getNewGameR :: Handler Value
getNewGameR = do
  result <- liftIO $ storeNewGame gameState
  liftIO $ print result
  liftIO $ print $ "Created new game w/ id: " ++ (show $ gameId gameState)
  return $ toJSON gameState
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

      
 
