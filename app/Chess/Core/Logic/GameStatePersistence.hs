module Chess.Core.Logic.GameStatePersistence where

import Import
import Chess.Core.Domain.GameState
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as DM
import qualified Text.Read as TR

storeNewGame :: GameState -> IO (Either R.Reply R.Status) 
storeNewGame gameState@GameState{gameId = GameId gId} = do
  conn <- R.checkedConnect R.defaultConnectInfo
  R.runRedis conn $ do
    liftIO $ print $ "Storing game w/ id: " ++ (show gId)
    R.set (fromString gId) (fromString . show $ gameState)
         
updateGame :: GameState -> IO (Either R.Reply R.Status) 
updateGame gameState@GameState{gameId = GameId gId} = do
  conn <- R.checkedConnect R.defaultConnectInfo
  R.runRedis conn $ do
    liftIO $ print $ "Updating game w/ id: " ++ (show gId)
    R.set (fromString gId) (fromString . show $ gameState)

fetchGame :: GameId -> IO (Maybe GameState)
fetchGame (GameId gId) = do
  conn <- R.checkedConnect R.defaultConnectInfo
  R.runRedis conn $ do
    liftIO $ print $ "Fetching game w/ id: " ++ (show gId)
    gameStateResult <- R.get $ fromString gId
    return $ either (\reply -> Nothing) (\val -> if isNothing val then Nothing else Just $ TR.read (B.unpack $ DM.fromJust val)) gameStateResult
    --return (Just $ right $ read gameStateResult)
     --return Nothing
     --world <- get "world"
     --liftIO $ print (hello,world)
  
