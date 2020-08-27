module Macbeth.Fics.Api.GameType where

import           Data.Aeson.Types
import           Data.Char
import           Data.Map
import           GHC.Generics

data Category = Chess | Suicide | Losers | Atomic | Wild | Crazyhouse | Bughouse deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data WildBoard = W1 | W2 | W3 | W4 | W5 | W8 | W8a | FisherRandom deriving (Eq, Bounded, Enum, Show, Read, Generic)

gameTypes :: Map Category [WildBoard]
gameTypes = fromList [ (Chess, []), (Suicide, []), (Losers, []), (Atomic, []),
                       (Wild, enumFrom (minBound :: WildBoard))
                     , (Crazyhouse, [])
                     , (Bughouse, [])]

displayBoard :: WildBoard -> String
displayBoard W1 = "Reversed King and Queen"
displayBoard W2 = "Shuffle position"
displayBoard W3 = "Shuffle position, mirrored"
displayBoard W4 = "Random pieces, balanced bishops"
displayBoard W5 = "Pawns on 7th rank"
displayBoard W8 = "Pawns on 4th rank"
displayBoard W8a = "Pawns on 5th rank"
displayBoard FisherRandom = "Fisher Random"

ficsId :: WildBoard -> String
ficsId W1 = "w1"
ficsId W2 = "w2"
ficsId W3 = "w3"
ficsId W4 = "w4"
ficsId W5 = "w5"
ficsId W8 = "w8"
ficsId W8a ="w8a"
ficsId FisherRandom = "wild fr"

gameTypeSelectionToString :: Category -> Maybe WildBoard -> String
gameTypeSelectionToString _ (Just board) = ficsId board
gameTypeSelectionToString cat _ = fmap toLower $ show cat


instance ToJSON Category
instance FromJSON Category

instance ToJSON WildBoard
instance FromJSON WildBoard

