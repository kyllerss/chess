module Chess.Core.Serialization where

import           Chess.Core.Domain
import qualified Data.Text.Lazy    as T
import qualified Data.Text         as TT
import           Data.Aeson        ( FromJSON, ToJSON )
import           Data.Aeson.Types
import qualified Data.Aeson.Encode as J
import           GHC.Generics
import           Data.List         as DL ( groupBy, sortBy )
import qualified Data.IntMap       as IM

instance ToJSON Color where
    toJSON Black = toJSON ("b" :: T.Text)
    toJSON White = toJSON ("w" :: T.Text)

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: T.Text)
    toJSON Rook = toJSON ("r" :: T.Text)
    toJSON Knight = toJSON ("n" :: T.Text)
    toJSON Bishop = toJSON ("b" :: T.Text)
    toJSON King = toJSON ("k" :: T.Text)
    toJSON Queen = toJSON ("q" :: T.Text)

instance ToJSON Piece

instance ToJSON Player where
    toJSON (Human name id) =
        object [ "name" .= name, "id" .= id ]
    toJSON (Computer name id) =
        object [ "name" .= name, "id" .= id ]

instance ToJSON Coord where
    toJSON (Coord x y) = toJSON $ [ x, y ]

instance ToJSON Space

instance ToJSON Board where
    toJSON (Board sp) = toJSON $ encode . toColor . shape . sortSpaces $ sp
      where
        multimap :: (a -> b) -> [[a]] -> [[b]]
        multimap f ss = map (\xs -> map f xs) ss

        -- group by row
        shape :: [Space] -> [[Space]]
        shape vs = DL.groupBy (\a -> (\b -> (extractRow a) == (extractRow b)))
                              vs
          where
            extractRow :: Space -> Int
            extractRow (Space{coord = Coord row _}) =
                row

        encode :: [[Color]] -> [[Value]]
        encode sp = multimap (\x -> toJSON x) sp

        toColor :: [[Space]] -> [[Color]]
        toColor sp = multimap (\s -> color (s :: Space)) sp

        sortSpaces :: [Space] -> [Space]
        sortSpaces sp = DL.sortBy orderByColumnAndRow sp

        orderByColumnAndRow :: Space -> Space -> Ordering
        orderByColumnAndRow a b =
            case (a, b) of
                (Void (Coord a1 b1), Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Void (Coord a1 b1), Space{coord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{coord = (Coord a1 b1)}, Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{coord = (Coord a1 b1)}, Space{coord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ

instance ToJSON GameState where
    toJSON GameState{board = b} =
        object [ "board" .= b, "pieces" .= (renderPieces b) ]
      where
        renderPieces :: Board -> IM.IntMap Value
        renderPieces (Board sp) = foldl appendPiece IM.empty sp

        appendPiece :: IM.IntMap Value -> Space -> IM.IntMap Value
        appendPiece m (Space{coord = c, piece = Just p}) = IM.insert (pieceId p) (renderPiece p c) m 

{-
            map (\(Space{coord = c, piece = Just p}) ->
                     object [ (TT.pack (show (pieceId p))) .= (renderPiece p c) ])
                sp

-}

        renderPiece :: Piece -> Coord -> Value
        renderPiece p c = object [ "t" .= (pieceType p)
                                 , "c" .= (color (p :: Piece))
                                 , "p" .= (playerId $ player p)
                                 , "xy" .= c]

        playerId :: Player -> Int
        playerId (Human _ id) = id
        playerId (Computer _ id) = id
