module Macbeth.Wx.Config.DefaultSounds (
  defaultSounds
) where

import Macbeth.Wx.Config.Sounds

defaultSounds :: Maybe Sounds
defaultSounds = Just Sounds {
    enabled = True
  , enabledObservedGames = True
  , game = gameS
  , request = requestS
  , other = otherS
}


gameS :: GameS
gameS = GameS {
    newGame = Just "ding1.wav"
  , move = MoveS {
      normal = Just "woodthunk.wav"
    , capture = Just "thud.wav"
    , check = Just "slap.wav"
    , castling = Nothing
    , pieceDrop = Nothing
    , illegal = Just "penalty.wav"
    , takeback = Nothing
  }
  , endOfGame = EndOfGameS {
      youWin = Just "win.wav"
    , youLose = Just "lose.wav"
    , youDraw = Just "draw.wav"
    , whiteWins = Just "cymbal.wav"
    , blackWins = Just "cymbal.wav"
    , draw = Just "cymbal.wav"
    , abort = Nothing
  }
}

requestS :: RequestS
requestS = RequestS {
    challenge = Just "challenge.wav"
  , abortReq = Just "pop2.wav"
  , drawReq = Just "pop2.wav"
  , takebackReq = Just "pop2.wav"
}

otherS :: OtherS
otherS = OtherS {
  logonToServer = Just "gong.wav"
}
