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

postMoveR :: GameId -> Handler Value
postMoveR gId = do

  -- fetch/validate game
  game <- liftIO $ fetchGame gId
  when (isNothing game) (sendResponse $ unknownGame gId)

  -- fetch/validate parameters
  pIdS <- runInputPost $ ireq textField "pieceId" --runlookupPostParam "pieceId"
  coordS <- runInputPost $ ireq textField "coord" -- lookupPostParam "coord"
  -- when (isNothing pIdS) (sendResponse $ missingValidInput)
  -- when (isNothing coordS) (sendResponse $ missingValidInput)
  let maybePId = (A.decode $ C.convertString pIdS) -- Just $ PieceId 1 -- (A.decode $ decodeUtf8 pIdS) -- :: Maybe PieceId
  when (isNothing maybePId) (sendResponse $ missingValidInput)

  let maybeCoord = (A.decode $ C.convertString coordS) -- :: Maybe Coord
  when (isNothing maybeCoord) (sendResponse $ missingValidInput)

  let pId = DM.fromJust maybePId
      coord = DM.fromJust maybeCoord
  
  -- validate move
  let updatedGameState = applyMove pId coord (DM.fromJust game)
  when (isNothing $ updatedGameState) (sendResponse $ invalidMove pId coord)
  
  -- apply/store move
  liftIO $ print $ "Applied move " ++ show pId ++ " -> " ++ show coord ++ " to game " ++ (show $ gameId $ DM.fromJust updatedGameState)
  return $ toJSON updatedGameState
