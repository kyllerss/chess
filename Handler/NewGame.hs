module Handler.NewGame where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Player
import Database.Redis as R

storeNewGame :: GameState -> IO (Either Reply R.Status) 
storeNewGame gameState = do
  conn <- R.checkedConnect R.defaultConnectInfo
  R.runRedis conn $ do
     R.set "hello" (fromString . show $ gameState)
     
     --R.set "world" "world"
     --hello <- get "hello"
     --world <- get "world"
     --liftIO $ print (hello,world)
    
getNewGameR :: Handler Value
getNewGameR = do
  result <- liftIO $ storeNewGame gameState
  liftIO $ print result
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

      
 
