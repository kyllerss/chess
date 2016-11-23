module Chess.Core.Moves
    ( move
    , moves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import           Data.List         as DL

move :: Board -> Piece -> Coord -> Maybe Board
move b p c = newBoard (spacesFromBoard b) p c
  where
    newBoard :: [Space] -> Piece -> Coord -> Maybe Board
    newBoard [] _ _ = Nothing
    newBoard sps p c = newBoard fromSpace toSpace otherSpaces
      where
        collectComponents :: (Maybe Space, Maybe Space, [Space])
                          -> Space
                          -> (Maybe Space, Maybe Space, [Space])
        collectComponents tr@(from, to, rsps) space =
          if ((DM.isJust $ piece space) and ((piece space) == (piece))) then XXXX determine if coordinate matches or piece    

        (fromSpace, toSpace, otherSpaces) =
            DL.foldl' collectComponents (Nothing, Nothing, []) sps

        removePiece :: Piece -> Space -> Space
        removePiece prem srem = srem { piece = Nothing }

        addPiece :: Piece -> Space -> Space
        addPiece padd sadd = sadd { piece = Just padd }

        newBoard :: Maybe Space -> Maybe Space -> [Space] -> Maybe Board
        newBoard Nothing _ _ = Nothing
        newBoard _ Nothing _ = Nothing
        newboard (Just newFromSpace) (Just newToSpace) newOtherSpaces =
            Board $
                [ removePiece p newFromSpace ] ++
                    [ addPiece p newToSpace ] ++ newOtherSpaces
    newSpaces :: [Space]
                                                                                                                                      -> Piece
                                                                                                                                      -> Coord
                                                                                                                                      -> [Space]
    newSpaces sps p c = undefined

validMoves :: Board -> Piece -> [Coord]
validMoves b p = []
