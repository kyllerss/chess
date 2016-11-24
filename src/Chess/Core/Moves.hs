module Chess.Core.Moves
    ( move
    , moves
    , handleTo
    , handleFrom
    , collectComponents
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import           Data.List         as DL

move :: Board -> Piece -> Coord -> Maybe Board
move b p c = newBoard (spacesFromBoard b) p c

newBoard :: [Space] -> Piece -> Coord -> Maybe Board
newBoard [] _ _ = Nothing
newBoard sps p c = newBoard' p fromSpace toSpace otherSpaces
  where
    (fromSpace, toSpace, otherSpaces) =
        DL.foldl' (collectComponents p c) (Nothing, Nothing, []) sps

newBoard' :: Piece -> Maybe Space -> Maybe Space -> [Space] -> Maybe Board
newBoard' _ Nothing _ _ = Nothing
newBoard' _ _ Nothing _ = Nothing
newBoard' p (Just newFromSpace) (Just newToSpace) newOtherSpaces =
    Just $
        Board $
            [ removePiece newFromSpace p ]
                ++ [ addPiece newToSpace p ]
                    ++ newOtherSpaces

collectComponents :: Piece
                  -> Coord
                  -> (Maybe Space, Maybe Space, [Space])
                  -> Space
                  -> (Maybe Space, Maybe Space, [Space])
collectComponents p c tr@(from, to, rsps) space =
    (handleFrom p from space, handleTo p c to space, handleRest p c rsps space)

handleFrom :: Piece -> Maybe Space -> Space -> Maybe Space
handleFrom _ Nothing Space{piece = Nothing} =
    Nothing
handleFrom p Nothing s@Space{piece = Just cp} =
    if p == cp then Just (s :: Space) { piece = Nothing } else Nothing
handleFrom _ mfrom _ = mfrom

handleTo :: Piece -> Coord -> Maybe Space -> Space -> Maybe Space
handleTo p c Nothing s@Space{coord = cc} =
    if c == cc then Just ((s :: Space) { piece = Just p }) else Nothing
handleTo _ _ mto _ = mto

handleRest :: Piece -> Coord -> [Space] -> Space -> [Space]
handleRest p c rest s@Space{piece = Nothing,coord = cc} =
    if c == cc then rest else s : rest
handleRest p c rest s@Space{piece = Just cp,coord = cc} =
    if p == cp || c == cc then rest else s : rest

newSpaces :: [Space] -> Piece -> Coord -> [Space]
newSpaces sps p c = undefined

validMoves :: Board -> Piece -> [Coord]
validMoves b p = []
