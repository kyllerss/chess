module Handler.Game where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Player
import Chess.Core.Logic.GameStatePersistence
import qualified Data.Maybe as DM
import Handler.Move()

getNewGameR :: GameType -> Handler Value
getNewGameR gameType = do
  _ <- liftIO $ storeNewGame gameState
  liftIO $ print $ "Created new game w/ id: " ++ (show $ gameId gameState)
  redirect (ViewGameR (gameId gameState))
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
    player3 = Player { playerName = pack "Player 3"
                     , playerId = 3
                     , playerType = Human
                     , playerDirection = East
                     }
    player4 = Player { playerName = pack "Player 4"
                     , playerId = 4
                     , playerType = Human
                     , playerDirection = West
                     }
    gameState :: GameState
    gameState = initGame gameType [player1, player2, player3, player4] 

getViewGameR :: GameId -> Handler TypedContent
getViewGameR gId = do

  game <- liftIO $ fetchGame gId
  when (isNothing game) notFound
  
  liftIO $ print $ "Fetched game w/ id: " ++ (show $ gameId $ DM.fromJust game)
  --gameId <- liftIO $ gameId $ DM.fromJust game

  selectRep $ do
    provideRep $ do
      defaultLayout $ do
          addScript $ StaticR js_lib_nerve_js
          addScriptRemote "https://unpkg.com/react@15.3.2/dist/react-with-addons.js"
          addScriptRemote "https://unpkg.com/react-dom@15.3.2/dist/react-dom.js"
          addScript $ StaticR js_chess_controller_js
          addScript $ StaticR js_chess_model_js
          addScript $ StaticR js_chess_react_js
          addStylesheet $ StaticR css_chess_css
          $(widgetFile "view-game")
    provideJson game

 
