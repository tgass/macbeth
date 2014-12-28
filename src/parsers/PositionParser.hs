{-# LANGUAGE OverloadedStrings #-}

module PositionParser (

) where

import Seek

import Data.Char (chr)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Char8 as A (take)

