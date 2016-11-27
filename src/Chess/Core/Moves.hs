module Chess.Core.Moves
    ( move
    , validMoves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import qualified Data.List         as DL
import qualified Data.Maybe        as DM

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: Board -> Piece -> Coord -> Maybe Board
move b@Board{spacesMap = spsMap} p c =
    move' originSpace destSpace
  where
    move' :: Maybe Space -> Maybe Space -> Maybe Board
    move' Nothing _ = Nothing
    move' _ Nothing = Nothing
    move' (Just os) (Just ds) =
        Just $ b { spacesMap = updatedMap }
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
validMoves :: Board -> Piece -> Coord -> [Move]
validMoves b p c =
  (candidateMoves p c b North)
  ++ (candidateMoves p c b NorthEast)
  ++ (candidateMoves p c b East)
  ++ (candidateMoves p c b SouthEast)
  ++ (candidateMoves p c b South)
  ++ (candidateMoves p c b SouthWest)
  ++ (candidateMoves p c b West)
  ++ (candidateMoves p c b NorthWest)

{- Utility function for incrementing space based on direction. -}
moveD :: Coord -> Direction -> Int -> Coord
moveD (Coord row col) dir count
  | dir == North     = Coord (row - count) col
  | dir == NorthEast = Coord (row - count) (col + count)
  | dir == East      = Coord row (col + count)
  | dir == SouthEast = Coord (row + count) (col + count)
  | dir == South     = Coord (row + count) col
  | dir == SouthWest = Coord (row + count) (col - count)
  | dir == West      = Coord row (col - count)
  | dir == NorthWest = Coord (row - count) (col - count)
  
{- Returns candidate moves (legal and illegal) for given piece type.  -}
candidateMoves :: Piece -> Coord -> Board -> Direction -> [Move]

-- pawn
candidateMoves Piece{ pieceType = Pawn
                    , piecePlayer = Player{playerDirection = pd}
                    , pieceId = pId
                    }
               c@(Coord row col)
               b@Board{spacesMap = spsMap}
               d
  = (pawnMove $ M.lookup (moveD c d 1) spsMap) ++ (pawnMove $ M.lookup (moveD c d 2) spsMap) 
    where

      pawnMove :: Maybe Space -> [Move]
      pawnMove mSpace 
        | d /= pd = []
        | DM.isNothing mSpace = []
        | otherwise = [ Move { movePieceId = pId
                             , moveSpace = DM.fromJust mSpace } ]
    
-- knight
candidateMoves Piece{ pieceType = Knight, pieceId = pId }
               c@(Coord row col)
               b@Board{spacesMap = spsMap}
               d
  = map (\s -> Move { movePieceId = pId, moveSpace = DM.fromJust s } )
    $ DL.filter (\s -> DM.isJust s)
    $ [ lSpace $ Coord (row + 1) (col + 2)
      , lSpace $ Coord (row + 1) (col - 2)
      , lSpace $ Coord (row + 2) (col + 1)
      , lSpace $ Coord (row + 2) (col - 1)
      , lSpace $ Coord (row - 1) (col + 2)
      , lSpace $ Coord (row - 1) (col - 2)
      , lSpace $ Coord (row - 2) (col + 1)
      , lSpace $ Coord (row - 2) (col - 1)
  ]
    where lSpace = flip M.lookup spsMap
