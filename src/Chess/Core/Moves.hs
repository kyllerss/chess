module Chess.Core.Moves
    ( move
    , validMoves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import qualified Data.List         as DL

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: Board -> Piece -> Coord -> Maybe Board
move (Board{spacesMap = spsMap}) p c =
    move' originSpace destSpace
  where
    move' :: Maybe Space -> Maybe Space -> Maybe Board
    move' Nothing _ = Nothing
    move' _ Nothing = Nothing
    move' (Just os) (Just ds) =
        Just $ Board { spacesMap = updatedMap }
      where
        updatedMap :: M.Map Coord Space
        updatedMap = M.insert (spaceCoord ds) (addPiece ds p) $
            M.insert (spaceCoord os) (removePiece os) spsMap

    originSpace :: Maybe Space
    originSpace = fetchSpace' $
        M.toList $ M.filter (\s -> evalPiece $ spacePiece s) spsMap
      where
        fetchSpace' :: [(Coord, Space)] -> Maybe Space
        fetchSpace' [] = Nothing
        fetchSpace' [ (coord, spc) ] =
            Just spc

        evalPiece :: Maybe Piece -> Bool
        evalPiece Nothing = False
        evalPiece (Just p') = p' == p

    destSpace :: Maybe Space
    destSpace = M.lookup c spsMap

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> Piece -> [Move]
validMoves (Board{spacesMap = spMap}) p =
    []
