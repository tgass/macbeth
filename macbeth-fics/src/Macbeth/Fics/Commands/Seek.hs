{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Fics.Commands.Seek where

import Control.Lens hiding (from, to)
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Api.GameType
import GHC.Generics hiding (from, to)

data SeekConfig' a b c d e = SeekConfig {
    _scCategory :: a
  , _scBoard :: b
  , _scColor :: c
  , _scTime :: d
  , _scInc :: d
  , _scRated :: e
  , _scManual :: e
  , _scRatingFrom :: d
  , _scRatingTo :: d
} deriving (Show, Generic)

makeLenses ''SeekConfig'

type SeekConfig = SeekConfig' Category (Maybe WildBoard) SeekColor Int Bool

mkSeekString :: SeekConfig -> String
mkSeekString config = ("seek " ++) $ unwords $ filter (/= "") [
    show $ config ^. scTime
  , show $ config ^. scInc
  , convertIsRated $ config ^. scRated
  , convertColor $ config ^. scColor
  , gameTypeSelectionToString (config ^. scCategory) (config ^. scBoard)
  , convertIsManual $ config ^. scManual
  , convertRatingRange (config ^. scRatingFrom) (config ^. scRatingTo)
  ]

convertIsRated :: Bool -> String
convertIsRated True = "r"
convertIsRated False = "u"

convertColor :: SeekColor -> String
convertColor White = "w"
convertColor Black = "b"
convertColor Automatic = ""

convertRatingRange :: Int -> Int -> String
convertRatingRange from to = show from ++ "-" ++ show to

convertIsManual :: Bool -> String
convertIsManual True = "m"
convertIsManual False = "a"

