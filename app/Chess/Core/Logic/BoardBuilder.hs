module Chess.Core.Logic.BoardBuilder where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import Chess.Core.Domain.Board
import qualified Data.Map as Map
import qualified Data.Maybe as DM
import Data.List ((!!))

{-
Matrixes take the form of representational two dimensional arrays. '.' represents empty space:
[  ['.', '.', '.']
 , ['.', 'x', '.']
 , ['.', '.', '.']
]
-}

type BoardMatrix = [[Char]]
type PlayerMatrix = (Player, BoardMatrix)

{-
   colorMatrix character set is 'b' = Black, 'w' = White, 'v' = Void
   spacePropsMatrix character set is 'p' = promotion space
   pieceMatrix character set is 'p' = Pawn, 'r' = Rook, etc... 
-}
constituteBoard :: Int -> Int -> BoardMatrix -> BoardMatrix -> [PlayerMatrix] -> Maybe Board
constituteBoard width height colorMatrix spacePropsMatrix playerPieceMatrix
  | invalidColorMatrix = Nothing
  | invalidSpacePropsMatrix = Nothing
  | invalidPieceMatrix = Nothing
  | otherwise = board
  where
    invalidColorMatrix, invalidSpacePropsMatrix, invalidPieceMatrix :: Bool
    invalidColorMatrix = False
    invalidSpacePropsMatrix = False
    invalidPieceMatrix = False

    coords :: [Coord]
    coords = [Coord row col | row <- [0 .. (height - 1)], col <- [0 .. (width - 1)]] 
    
    board :: Maybe Board 
    board = Just nullBoard
            >>= initializeSpaces
            >>= applyColors
            >>= applySpaceProps
            >>= applyPlayerPieces

    applyToBoard :: ((Coord, Maybe Space) -> Space) -> Maybe Board -> Maybe Board
    applyToBoard _ Nothing = Nothing
    applyToBoard f (Just b@Board{spacesMap = spsMap}) = Just b{spacesMap = newMap}

      where newMap :: Map.Map Coord Space
            newMap = 
              foldl' (\m coord -> Map.insert coord (f (coord, Map.lookup coord spsMap)) m) spsMap coords

    initializeSpaces ::Board -> Maybe Board
    initializeSpaces b = applyToBoard
                         (\(coord, _) -> nullSpace coord)
                         (Just b)

    applyColors :: Board -> Maybe Board
    applyColors b = applyToBoard
                    (\(c@(Coord row col), mSpace) -> decodeSpaceType (DM.fromJust mSpace) c (colorMatrix !! row !! col))
                    (Just b)

    applySpaceProps :: Board -> Maybe Board
    applySpaceProps b = applyToBoard
                        (\(Coord row col, mSpace) -> decodeSpaceProps (DM.fromJust mSpace) (spacePropsMatrix !! row !! col))
                        (Just b)

    applyPlayerPieces :: Board -> Maybe Board
    applyPlayerPieces b = newBoard
      where
        newBoard :: Maybe Board
        newBoard =
          foldl' (\acc (player, boardMatrix) ->
                     applyToBoard
                     (\(Coord row col, mSpace) ->
                         decodeSpacePiece (DM.fromJust mSpace) (boardMatrix !! row !! col) player (assignPlayerColor player))
                     acc)
                 (Just b)
                 playerPieceMatrix
      
decodeSpaceType :: Space -> Coord -> Char -> Space
decodeSpaceType sp c char
  | char == 'v' = Void c
  | char == 'b' = sp{spaceColor = Black}
  | char == 'w' = sp{spaceColor = White}
  | otherwise = sp

decodeSpaceProps :: Space -> Char -> Space
decodeSpaceProps sp@(Void _) _ = sp
decodeSpaceProps sp c
  | c == 'p' = sp{spaceSideEffectType = Just PawnPromotion}
  | otherwise = sp

decodeSpacePiece :: Space -> Char -> Player -> Color -> Space
decodeSpacePiece sp@(Void _) _ _ _ = sp
decodeSpacePiece sp@Space{spaceCoord = spCoord} c pl color
  | c == 'p' = setPiece Pawn
  | c == 'r' = setPiece Rook
  | c == 'n' = setPiece Knight
  | c == 'b' = setPiece Bishop
  | c == 'k' = setPiece King
  | c == 'q' = setPiece Queen
  | otherwise = sp

  where
    setPiece :: PieceType -> Space
    setPiece pt = addPiece sp (newPiece pt)

    newPiece :: PieceType -> Piece
    newPiece pt = buildPiece (buildPieceId spCoord) pt color pl (Just spCoord)

