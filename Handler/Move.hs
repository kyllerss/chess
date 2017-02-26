module Handler.Move where

import Import
import Chess.Core.Domain.Coord
import Chess.Core.Domain.GameState
import Chess.Core.Domain.Piece
import Chess.Core.Logic.GameStatePersistence
import qualified Text.Read as TR
import qualified Data.Maybe as DM

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

  -- fetch/validate parameters
  pIdS <- lookupPostParam "pieceId"
  coordS <- lookupPostParam "coord"
  when (isNothing pIdS) (sendResponse $ missingValidInput)
  when (isNothing coordS) (sendResponse $ missingValidInput)
  let pId = ((TR.read $ unpack $ (DM.fromJust pIdS))) :: PieceId
      coord = ((TR.read $ unpack $ (DM.fromJust coordS))) :: Coord

  -- fetch/validate game
  game <- liftIO $ fetchGame gId
  when (isNothing game) (sendResponse $ unknownGame gId)

  -- validate move
  let updatedGameState = applyMove pId coord (DM.fromJust game)
  when (isNothing $ updatedGameState) (sendResponse $ invalidMove pId coord)
  
  -- apply/store move
  liftIO $ print $ "Applied move " ++ show pId ++ " -> " ++ show coord ++ " to game " ++ (show $ gameId $ DM.fromJust updatedGameState)
  return $ toJSON updatedGameState
