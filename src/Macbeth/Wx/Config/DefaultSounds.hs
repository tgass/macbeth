module Macbeth.Wx.Config.DefaultSounds (
  defaultSounds
) where

import Macbeth.Wx.Config.Sounds

defaultSounds :: Sounds
defaultSounds = Sounds {
    enabled = True
  , enabledObservedGames = True
  , game = gameS
  , request = requestS
  , other = otherS
}


gameS :: GameS
gameS = GameS {
    newGame = Nothing
  , move = MoveS {
      normal = Just "move.wav"
    , capture = Nothing
    , check = Nothing
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
  logonToServer = Just "ding1.wav"
}
