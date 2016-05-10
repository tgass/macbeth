module Macbeth.OSX.Afplay (
  afplay
) where

import Control.Monad
import System.Process

afplay :: FilePath -> IO ()
afplay = void . runCommand . ("afplay " ++)
