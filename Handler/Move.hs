module Handler.Move where

import Import
import Chess.Core.Domain.Coord
import Chess.Core.Domain.GameState
import Chess.Core.Domain.Piece
import Chess.Core.Logic.GameStatePersistence
import qualified Data.Maybe as DM
import qualified Data.Aeson as A
import qualified Data.String.Conversions as C

jsonError :: String -> Value
jsonError msg = object[ "error" .= msg]

unknownGame :: GameId -> Value
unknownGame (GameId gId) = jsonError $ "Unknown game id " ++ show gId

missingValidInput :: Value
missingValidInput = jsonError "Missing valid inputs"

invalidMove :: PieceId -> Coord -> Value
invalidMove pieceId coord = jsonError $ "Move is invalid: " ++ show pieceId ++ " -> " ++ show coord

unableToPersistState :: Value
unableToPersistState = jsonError $ "Unable to persist state."

postMoveR :: String -> Handler Value
postMoveR gId = do

  liftIO $ print $ "Handling move request for game " ++ show gId
  
  -- fetch/validate game
  game <- liftIO $ fetchGame $ GameId gId
  when (isNothing game) (sendResponseStatus status403 $ unknownGame (GameId gId))

  -- fetch/validate parameters
  pIdS <- runInputPost $ ireq textField "pieceId" --runlookupPostParam "pieceId"
  coordS <- runInputPost $ ireq textField "coord" -- lookupPostParam "coord"
  -- when (isNothing pIdS) (sendResponse $ missingValidInput)
  -- when (isNothing coordS) (sendResponse $ missingValidInput)
  let maybePId = (A.decode $ C.convertString pIdS) -- Just $ PieceId 1 -- (A.decode $ decodeUtf8 pIdS) -- :: Maybe PieceId
  when (isNothing maybePId) (sendResponseStatus status403 missingValidInput)

  let maybeCoord = (A.decode $ C.convertString coordS) -- :: Maybe Coord
  when (isNothing maybeCoord) (sendResponseStatus status403 missingValidInput)

  let pId = DM.fromJust maybePId
      coord = DM.fromJust maybeCoord
  
  -- validate move
  liftIO $ print $ "Applying move " ++ show pId ++ " -> " ++ show coord ++ " to game " ++ (show gId)
  let updatedGameState = applyMove pId coord (DM.fromJust game)
  when (isNothing $ updatedGameState) (sendResponseStatus status403 $ invalidMove pId coord)
  
  -- apply/store move
  liftIO $ print $ "Applied move " ++ show pId ++ " -> " ++ show coord ++ " to game " ++ (show $ gameId $ DM.fromJust updatedGameState)
  liftIO $ print $ "New player turn: " ++ (show $ playerTurn $ DM.fromJust updatedGameState)
  liftIO $ print $ "Moves: " ++ (show $ moves $ DM.fromJust updatedGameState)
  response <- liftIO $ updateGame (DM.fromJust updatedGameState)
  liftIO $ print $ "Response: " ++ (show response)
  --FIXME: when (E.isLeft <$> response) (sendResponse $ unableToPersistState)
  return $ toJSON updatedGameState
