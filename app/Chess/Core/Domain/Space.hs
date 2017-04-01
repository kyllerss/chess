module Chess.Core.Domain.Space where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import qualified Data.List as DL
import qualified Data.Map as Map

data SpaceSideEffectType = PawnPromotion
  deriving (Show, Read, Generic, Eq, NFData)

data Space = Space { spacePiece :: Maybe Piece
                   , spaceColor :: Color
                   , spaceCoord :: Coord
                   , spaceSideEffectType :: Maybe SpaceSideEffectType
                   }
           | Void Coord
    deriving (Show, Read, Generic, Eq, NFData)

instance ToJSON SpaceSideEffectType

instance ToJSON Space

instance Ord Space where
    (Space{spaceCoord = c1}) `compare` (Space{spaceCoord = c2}) =
        c1 `compare` c2
    (Space{spaceCoord = c1}) `compare` (Void c2) = c1 `compare` c2
    (Void c1) `compare` (Space{spaceCoord = c2}) = c1 `compare` c2
    (Void c1) `compare` (Void c2) = c1 `compare` c2

buildSpaceMap :: [Space] -> Map Coord Space
buildSpaceMap sps = DL.foldl (\m s -> Map.insert (spaceCoord s) s m) Map.empty sps

{- default space builder -}
defaultSpaceBuilder :: Coord -> Space
defaultSpaceBuilder = \c@(Coord x y) -> buildSpace x y (calcSpaceColor c) Nothing

{- alternates piece color -}
calcSpaceColor :: Coord -> Color
calcSpaceColor (Coord x y) =
    if (even (x + y)) then White else Black

{- space builder -}
buildSpace :: Int -> Int -> Color -> Maybe SpaceSideEffectType -> Space
buildSpace x y c se = Space { spacePiece = Nothing
                            , spaceColor = c
                            , spaceCoord = Coord x y
                            , spaceSideEffectType = se
                            }

{- Remove a piece from a given space.  -}
removePiece :: Space -> Space
removePiece srem = srem { spacePiece = Nothing }

{- Add a piece to a given space.  -}
addPiece :: Space -> Piece -> Space
addPiece sadd padd = sadd { spacePiece = Just padd }

