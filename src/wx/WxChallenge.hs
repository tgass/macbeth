module WxChallenge (
  wxChallenge
) where

import Api
import System.IO (Handle, hPutStrLn)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore

import CommandMsg


--main = start $ wxChallenge undefined (Challenge "foobar" (Rating 1200) "barbaz" Guest "12 2 blitz")


--TODO: Create proper Challenge domain type
wxChallenge :: Handle -> CommandMsg -> IO ()
wxChallenge h c@(Challenge{}) = do
  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := hPutStrLn h "5 accept" >> close f]
  b_decline <- button p [text := "Decline", on command := hPutStrLn h "5 decline" >> close f]
  b_adjourn <- button p [text := "Adjourn", on command := hPutStrLn h "5 adjourn" >> close f]
  st_params <- staticText p [ text := toString c
                            , fontFace := "Avenir Next Medium"
                            , fontSize := 16
                            , fontWeight := WeightBold]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "You received a challenge." (
              grid 5 5 [
                [ hfill $ widget st_params]]
            )
            , floatBottomRight $ row 5 [widget b_accept, widget b_decline, widget b_adjourn]]
        ]

  windowShow f
  windowOnDestroy f $ hPutStrLn h "5 adjourn"
  return ()

wxChallenge h _ = return ()

toString (Challenge n1 r1 n2 r2 params) = n1 ++ " " ++ show r1 ++ " vs. " ++ n2 ++ " (" ++ show r2 ++ ") " ++ params
toString _ = ""

