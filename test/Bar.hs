module Bar ( tests ) where

import Distribution.TestSuite

import Lentils.Api.Challenge
import Lentils.Api.Rating
import Lentils.Api.CommandMsg
import Lentils.Api.Game
import Lentils.Fics.Parsers.CommandMsgParser

import qualified Data.ByteString.Char8 as BS

tests :: IO [Test]
tests = return $ (Test . succeeds) `fmap` (uncurry (flip compareCmdMsg) `fmap` commandMessageParserTest)



succeeds :: Result -> TestInstance
succeeds result = TestInstance
  { run = return $ Finished result
  , name = "succeeds"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ succeeds result
  }

commandMessageParserTest :: [(CommandMsg, String)]
commandMessageParserTest = [ (DrawDeclined, "GuestXDXP declines the draw request.")
      , (DrawOffered, "GuestDWXY offers you a draw.")
      , (GameResult 368 "CalicoCat resigns" WhiteWins, "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0")
      , (GameResult 406 "GuestQLHT resigns" BlackWins, "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done.")
      , (GameResult 181 "Danimateit forfeits on time" BlackWins, "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1")
      , (GameResult 196 "Game drawn by mutual agreement" Draw, "\NAK4\SYN34\SYN\n{Game 196 (GuestCWVD vs. GuestDWTL) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (GameResult 202 "Game drawn by mutual agreement" Draw, "\NAK5\SYN11\SYNYou accept the draw request from GuestNMNG.\n\n{Game 202 (GuestDKZD vs. GuestNMNG) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (NoSuchGame, "\NAK5\SYN80\SYNThere is no such game.\n\ETB")
      , (MatchRequested $ Challenge "GuestYWYK" Unrated "GuestMGSD" Unrated "unrated blitz 2 12", "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12.")
      ]

compareCmdMsg :: String -> CommandMsg -> Result
compareCmdMsg str cmd = case parseCommandMsg $ BS.pack str of
  Left txt -> Fail txt
  Right cmd' -> if cmd' == cmd then Pass else Fail $ show cmd ++ " <--> " ++ show cmd'
