module Chess.Core.Moves
    ( move
    , moves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import           Data.List         as DL

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
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

{- Populates tuple where first element is from space, second is to space, and third is remaining spaces. -}
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
handleFrom p Nothing s@Space{piece = Just cp}
    | p == cp   = Just (s :: Space) { piece = Nothing }
    | otherwise = Nothing
handleFrom _ mfrom _ = mfrom

handleTo :: Piece -> Coord -> Maybe Space -> Space -> Maybe Space
handleTo p c Nothing s@Space{coord = cc}
    | c == cc   = Just ((s :: Space) { piece = Just p })
    | otherwise = Nothing
handleTo _ _ mto _ = mto

handleRest :: Piece -> Coord -> [Space] -> Space -> [Space]
handleRest p c rest s@Space{piece = Nothing,coord = cc}
    | c == cc   = rest
    | otherwise = s : rest
handleRest p c rest s@Space{piece = Just cp,coord = cc}
    | p == cp || c == cc = rest
    | otherwise          = s : rest
