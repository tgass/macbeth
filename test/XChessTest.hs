module XChessTest ( tests ) where

import Distribution.TestSuite

import Lentils.Api.Api
import Lentils.Api.Challenge
import Lentils.Api.Rating
import Lentils.Api.CommandMsg
import Lentils.Api.Game
import Lentils.Api.Seek
import Lentils.Api.Move
import Lentils.Fics.Parsers.CommandMsgParser
import Lentils.Fics.Parsers.PositionParser
import Lentils.Fics.Parsers.GamesParser

import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS

tests :: IO [Test]
tests = return $ (Test . succeeds) `fmap` (uncurry (flip compareCmdMsg) `fmap` commandMessageParserTest) ++
                 (Test . succeeds) `fmap` (uncurry (flip compareCmdMsg) `fmap` seekMsgParserTest) ++
                 (Test . succeeds) `fmap` (uncurry comparePosition `fmap` positionTest)
                 -- [Test $ succeeds parseGamesListTest]


succeeds :: Result -> TestInstance
succeeds result = TestInstance
  { run = return $ Finished result
  , Distribution.TestSuite.name = "succeeds"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ succeeds result
  }

commandMessageParserTest :: [(CommandMsg, String)]
commandMessageParserTest = [
        (DrawDeclined, "GuestXDXP declines the draw request.")
      , (DrawOffered, "GuestDWXY offers you a draw.")
      , (GameResult 368 "CalicoCat resigns" WhiteWins, "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0")
      , (GameResult 406 "GuestQLHT resigns" BlackWins, "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done.")
      , (GameResult 181 "Danimateit forfeits on time" BlackWins, "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1")
      , (GameResult 196 "Game drawn by mutual agreement" Draw, "\NAK4\SYN34\SYN\n{Game 196 (GuestCWVD vs. GuestDWTL) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (GameResult 202 "Game drawn by mutual agreement" Draw, "\NAK5\SYN11\SYNYou accept the draw request from GuestNMNG.\n\n{Game 202 (GuestDKZD vs. GuestNMNG) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (NoSuchGame, "\NAK5\SYN80\SYNThere is no such game.\n\ETB")
      , (MatchRequested $ Challenge "GuestYWYK" Unrated "GuestMGSD" Unrated "unrated blitz 2 12", "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12.")
      , (GuestLogin "FOOBAR", "Press return to enter the server as \"FOOBAR\":")
      -- playSeek
      , (Boxed [RemoveSeeks [25], GameMove defaultMove], "\NAK4\SYN158\SYN\n<sr> 25\nfics% \nCreating: chicapucp (1658) GuestFTYL (++++) unrated blitz 3 0\n{Game 18 (chicapucp vs. GuestFTYL) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n\nGame 18: A disconnection will be considered a forfeit.\n\ETB")
      -- seekMatchesAlreadyPosted
      , (Boxed [RemoveSeeks [130], GameMove defaultMove], "\NAK4\SYN155\SYNYou are unregistered - setting to unrated.\nYour seek matches one already posted by GuestLQFZ.\n\n<sr> 130\nfics% \nCreating: GuestLQFZ (++++) GuestSFKS (++++) unrated blitz 5 0\n{Game 214 (GuestLQFZ vs. GuestSFKS) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n\nGame 214: A disconnection will be considered a forfeit.\n\ETB")
      , (Boxed [RemoveSeeks [119], GameMove defaultMove], "\NAK4\SYN155\SYNYour seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n")
      -- seekInfoBlock
      , (Boxed [ClearSeek, NewSeek $ Seek 16 "CatNail" (Rating 1997) 3 0 False Suicide Nothing (0, 9999)], "\NAK4\SYN56\SYNseekinfo set.\n<sc>\n<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f\n")
      , (Observe defaultMove, "\NAK5\SYN80\SYNYou are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0" ++ defaultMoveStr)
      , (Finger "GuestSPRM(U)" "\n\nOn for: 4 mins   Idle: 0 secs\n\n\nTotal time online: 4 mins\n\nTimeseal   : Off", "\NAK5\SYN37\SYNFinger of GuestSPRM(U):\n\nOn for: 4 mins   Idle: 0 secs\n\n\nTotal time online: 4 mins\n\nTimeseal   : Off\n\n\ETB")
      , (PendingOffers [] [], "\NAK5\SYN87\SYNThere are no offers pending to other players.\n\nThere are no offers pending from other players.\n\ETB")
      , (PendingOffers [PendingOffer 45 "You are offering GuestSCPB a challenge: GuestSLFT (----) GuestSCPB (----) unrated blitz 5 0"] [], "\NAK5\SYN87\SYNOffers to other players:\n\n  45: You are offering GuestSCPB a challenge: GuestSLFT (----) GuestSCPB (----) unrated blitz 5 0.\n\nIf you wish to withdraw any of these offers type \"withdraw number\".\n\nThere are no offers pending from other players.\n\ETB")
      , (PendingOffers [] [PendingOffer 43 "GuestWXFZ is offering a challenge: GuestWXFZ (----) Schoon (1019) unrated blitz 5 0"], "\NAK5\SYN87\SYNThere are no offers pending to other players.\n\nOffers from other players:\n\n  43: GuestWXFZ is offering a challenge: GuestWXFZ (----) Schoon (1019) unrated blitz 5 0.\n\nIf you wish to accept any of these offers type \"accept number\".\nIf you wish to decline any of these offers type \"decline number\".\n\ETB")
      ]

defaultMove = Move "-------- -------- -------- -------- -------- -------- -------- --------" [] White Nothing [WhiteShort,WhiteLong,BlackShort,BlackLong] 0 18 "nameWhite" "nameBlack" OponentsMove 1 "none" "(0:00)" 180 180 Nothing
defaultMoveStr = "<12> -------- -------- -------- -------- -------- -------- -------- -------- W -1 1 1 1 1 0 18 nameWhite nameBlack -1 3 0 39 39 180 180 1 none (0:00) none 1 0 0\n"


seekMsgParserTest :: [(CommandMsg, String)]
seekMsgParserTest = [
    (ClearSeek, "<sc>")
  , (RemoveSeeks [59, 3, 11], "<sr> 59 3 11")
  , (NewSeek $ Seek 7 "GuestNMZJ" Guest 15 5 False Standard (Just White) (0,9999), "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t")
  , (NewSeek $ Seek 16 "CatNail" (Rating 1997) 3 0 False Suicide Nothing (0,9999), "<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f")
  , (NewSeek $ Seek 56 "GuestCXDH" Guest 7 0 False Wild (Just White) (0,9999), "<s> 56 w=GuestCXDH ti=01 rt=0P t=7 i=0 r=u tp=wild/4 c=W rr=0-9999 a=t f=f")
  ]


-- fuzzy compare
--, (GameMove _), "<12> rnbqkbnr pp-p-ppp ----p--- --p----- ----P--- ------P- PPPP-P-P RNBQKBNR W 2 1 1 1 1 0 232 tuffshaq mlaren 0 15 12 39 39 901 897 3 P/c7-c5 (0:15) c5 0 1 167")


positionTest :: [(Position, String)]
positionTest = [
    ([], "-------- -------- -------- -------- -------- -------- -------- --------")
  , ([ (Square A Eight, Piece Rook White)
     , (Square B Seven, Piece Knight White)
     , (Square C Six, Piece Bishop White)
     , (Square D Five, Piece Queen White)
     , (Square E Four, Piece King White)
     , (Square F Three, Piece Pawn White)
     , (Square G Two, Piece Rook Black)
     , (Square H One, Piece King Black)], "R------- -N------ --B----- ---Q---- ----K--- -----P-- ------r- -------k")
  ]


parseGamesListTest :: Result
parseGamesListTest = case parseCommandMsg $ BS.pack games of
  Left txt -> Fail txt
  Right (Games games) -> if length games == 584 then Pass else Fail $ show $ length games
  where
    games = unsafePerformIO $ readFile "./test/Games.txt"

comparePosition :: Position -> String -> Result
comparePosition pos str = let pos' = parsePosition str
                          in if pos' == pos then Pass else Fail $ show pos ++ " <<-->> " ++ show pos'

compareCmdMsg :: String -> CommandMsg -> Result
compareCmdMsg str cmd = case parseCommandMsg $ BS.pack str of
  Left txt -> Fail txt
  Right cmd' -> if cmd' == cmd then Pass else Fail $ show cmd ++ " <<-->> " ++ show cmd'
