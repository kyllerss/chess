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
        (fromSpace, toSpace, otherSpaces) =
            DL.foldl' collectComponents (Nothing, Nothing, []) sps

        collectComponents :: (Maybe Space, Maybe Space, [Space])
                          -> Space
                          -> (Maybe Space, Maybe Space, [Space])
        collectComponents tr@(from, to, rsps) space =
            (handleFrom from space, handleTo to space, handleRest rsps space)
          where
            handleFrom :: Maybe Space -> Space -> Maybe Space
            handleFrom Nothing Space{piece = Nothing} =
                Nothing
            handleFrom Nothing s@Space{piece = Just cp} =
                if p == cp
                then Just (s :: Space) { piece = Nothing }
                else Nothing
            handleFrom mfrom _ = mfrom

            handleTo :: Maybe Space -> Space -> Maybe Space
            handleTo Nothing s@Space{coord = cc} =
                if c == cc
                then Just ((s :: Space) { piece = Just p })
                else Nothing
            handleTo mto _ = mto

            handleRest :: [Space] -> Space -> [Space]
            handleRest rest s@Space{piece = Nothing,coord = cc} =
                if c == cc then rest else s : rest
            handleRest rest s@Space{piece = Just cp,coord = cc} =
                if p == cp || c == cc then rest else s : rest

        newBoard :: Maybe Space -> Maybe Space -> [Space] -> Maybe Board
        newBoard Nothing _ _ = Nothing
        newBoard _ Nothing _ = Nothing
        newboard (Just newFromSpace) (Just newToSpace) newOtherSpaces =
            Board $
                [ removePiece newFromSpace p ]
                    ++ [ addPiece newToSpace p ]
                        ++ newOtherSpaces
    newSpaces :: [Space] -> Piece -> Coord -> [Space]
    newSpaces sps p c = undefined

validMoves :: Board -> Piece -> [Coord]
validMoves b p = []
