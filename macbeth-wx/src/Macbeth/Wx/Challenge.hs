module Macbeth.Wx.Challenge (
  wxChallenge
) where

import           Control.Concurrent
import           Graphics.UI.WX
import           Graphics.UI.WXCore
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Game
import           Macbeth.Wx.Utils
import qualified Macbeth.Wx.Commands as Cmds
import           System.IO


wxChallenge :: Handle -> GameParams -> Chan Message  -> IO ()
wxChallenge h gameParams chan = do
  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := Cmds.accept h >> close f]
  b_decline <- button p [text := "Decline", on command := Cmds.decline h >> close f]
  st_params <- staticText p [ text := showGameParams gameParams
                            , fontFace := "Avenir Next Medium"
                            , fontSize := 16
                            , fontWeight := WeightBold]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "You received a challenge." (
              grid 5 5 [
                [ hfill $ widget st_params]]
            )
            , floatBottomRight $ row 5 [widget b_accept, widget b_decline]]
        ]

  threadId <- eventLoop f eventId chan $ \case
    Challenge newGameParams -> when (isUpdate gameParams newGameParams) $ close f
    WxClose -> close f
    _ -> return ()

  windowOnDestroy f $ killThread threadId



eventId :: Int
eventId = wxID_HIGHEST + 1
