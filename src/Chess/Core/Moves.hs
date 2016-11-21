module Chess.Core.Moves (move, moves) where

import Chess.Core.Domain

move :: Board -> Piece -> Coord -> Maybe Board
move b p c =  newBoard (spacesFromBoard b) p c
  where

    newBoard :: [Space] -> Piece -> Coord -> Maybe Board
    newBoard [] _ _ = Nothing
    newBoard sps p c =
      let updated  = undefined
    
    
    newSpaces :: [Space] -> Piece -> Coord -> [Space]
    newSpaces sps p c = undefined

moves :: Board -> Piece -> [Coord]
moves b p = []
