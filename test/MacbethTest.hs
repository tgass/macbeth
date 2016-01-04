module MacbethTest ( tests ) where

import Distribution.TestSuite

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.Api.Rating
import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Parsers.FicsMessageParser
import Macbeth.Fics.Parsers.PositionParser
import Macbeth.Fics.Parsers.GamesParser

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

commandMessageParserTest :: [(FicsMessage, String)]
commandMessageParserTest = [
        (DrawRequest, "GuestDWXY offers you a draw.")
      , (GameResult 368 "CalicoCat resigns" WhiteWins, "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0")
      , (GameResult 406 "GuestQLHT resigns" BlackWins, "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done.")
      , (GameResult 181 "Danimateit forfeits on time" BlackWins, "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1")
      , (GameResult 196 "Game drawn by mutual agreement" Draw, "\NAK4\SYN34\SYN\n{Game 196 (GuestCWVD vs. GuestDWTL) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (GameResult 202 "Game drawn by mutual agreement" Draw, "\NAK5\SYN11\SYNYou accept the draw request from GuestNMNG.\n\n{Game 202 (GuestDKZD vs. GuestNMNG) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (NoSuchGame, "\NAK5\SYN80\SYNThere is no such game.\n\ETB")
      , (MatchRequested $ Challenge "GuestYWYK" Unrated "GuestMGSD" Unrated "unrated blitz 2 12", "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12.")
      , (GuestLogin "FOOBAR", "Press return to enter the server as \"FOOBAR\":")
      -- playSeek
      , (Boxed [RemoveSeeks [25], MatchAccepted defaultMove], "\NAK4\SYN158\SYN\n<sr> 25\nfics% \nCreating: chicapucp (1658) GuestFTYL (++++) unrated blitz 3 0\n{Game 18 (chicapucp vs. GuestFTYL) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n\nGame 18: A disconnection will be considered a forfeit.\n\ETB")
      -- seekMatchesAlreadyPosted
      , (Boxed [RemoveSeeks [130], MatchAccepted defaultMove], "\NAK4\SYN155\SYNYou are unregistered - setting to unrated.\nYour seek matches one already posted by GuestLQFZ.\n\n<sr> 130\nfics% \nCreating: GuestLQFZ (++++) GuestSFKS (++++) unrated blitz 5 0\n{Game 214 (GuestLQFZ vs. GuestSFKS) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n\nGame 214: A disconnection will be considered a forfeit.\n\ETB")
      , (Boxed [RemoveSeeks [119], MatchAccepted defaultMove], "\NAK4\SYN155\SYNYour seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n" ++ defaultMoveStr ++ "\n")
      -- seekInfoBlock
      , (Boxed [ClearSeek, NewSeek $ Seek 16 "CatNail" [Computer] (Rating 1997 None) 3 0 False Suicide Nothing (0, 9999)], "\NAK4\SYN56\SYNseekinfo set.\n<sc>\n<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f\n")
      , (Observe defaultMove, "\NAK5\SYN80\SYNYou are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0" ++ defaultMoveStr)
      , (Finger "GuestSPRM(U)" "\n\nOn for: 4 mins   Idle: 0 secs\n\n\nTotal time online: 4 mins\n\nTimeseal   : Off", "\NAK5\SYN37\SYNFinger of GuestSPRM(U):\n\nOn for: 4 mins   Idle: 0 secs\n\n\nTotal time online: 4 mins\n\nTimeseal   : Off\n\n\ETB")
      , (PendingOffers [] [], "\NAK5\SYN87\SYNThere are no offers pending to other players.\n\nThere are no offers pending from other players.\n\ETB")
      , (PendingOffers [PendingOffer 45 "You are offering GuestSCPB a challenge: GuestSLFT (----) GuestSCPB (----) unrated blitz 5 0"] [], "\NAK5\SYN87\SYNOffers to other players:\n\n  45: You are offering GuestSCPB a challenge: GuestSLFT (----) GuestSCPB (----) unrated blitz 5 0.\n\nIf you wish to withdraw any of these offers type \"withdraw number\".\n\nThere are no offers pending from other players.\n\ETB")
      , (PendingOffers [] [PendingOffer 43 "GuestWXFZ is offering a challenge: GuestWXFZ (----) Schoon (1019) unrated blitz 5 0"], "\NAK5\SYN87\SYNThere are no offers pending to other players.\n\nOffers from other players:\n\n  43: GuestWXFZ is offering a challenge: GuestWXFZ (----) Schoon (1019) unrated blitz 5 0.\n\nIf you wish to accept any of these offers type \"accept number\".\nIf you wish to decline any of these offers type \"decline number\".\n\ETB")
      , (MatchOfferIdentical, "\NAK4\SYN73\SYNYou are already offering an identical match to GuestSPLL.\n\ETB")
      , (AbortRequest "GuestSPLL", "GuestSPLL would like to abort the game; type \"abort\" to accept.")
      , (TakebackRequest "GuestTYLF" 2, "GuestTYLF would like to take back 2 half move(s).")
      , (GameResult 82 "Game aborted by mutual agreement" Aborted, "\NAK5\SYN11\SYNYou accept the abort request from GuestSPLL.\n\n{Game 82 (GuestTKHJ vs. GuestSPLL) Game aborted by mutual agreement} *\n\ETB")
      , (GameResult 112 "Game aborted on move 1" Aborted, "\NAK4\SYN10\SYNThe game has been aborted on move one.\n\n{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *\n\ETB")
      , (GameResult 383 "Game aborted by mutual agreement" Aborted, "\NAK5\SYN10\SYN\n{Game 383 (GuestRVNY vs. GuestZTNM) Game aborted by mutual agreement} *\n\ETB")
      , (GameResult 112 "Game aborted on move 1" Aborted, "{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *")
      , (GameResult 297 "Game aborted by mutual agreement" Aborted, "{Game 297 (GuestSPLL vs. GuestTKHJ) Game aborted by mutual agreement} *")
      , (MatchUserNotLoggedIn "GuestLHDG", "\NAK4\SYN73\SYNGuestLHDG is not logged in.\n\ETB")
      , (PieceHolding 455 [Pawn,Rook,Knight] [Bishop,Queen],  "<b1> game 455 white [PRN] black [BQ]")
      , (SeekNotAvailable, "\NAK4\SYN158\SYNThat seek is not available.\n\ETB")
      , (Boxed [NullCommand, GameMove {context = Nothing, move = Move {positionRaw = "-------- -------- -------- -------- -------- -------- -------- --------", position = [], turn = Black, doublePawnPush = Nothing, castlingAv = [WhiteShort,WhiteLong,BlackShort,BlackLong], ply = 1, Macbeth.Fics.Api.Move.gameId = 147, Macbeth.Fics.Api.Move.nameW = "Schoon", Macbeth.Fics.Api.Move.nameB = "GuestYBPD", relation = MyMove, initialTime = 5, incPerMove = 0, whiteRelStrength = 39, blackRelStrength = 39, remainingTimeW = 295, remainingTimeB = 297, moveNumber = 2, moveVerbose = Just(Simple (Square G One) (Square E Three)), timeTaken = "(0:05)", movePretty = Just "Qe3"}}],
          "\NAK6\SYN1\SYN\n\r\a\n\r<12> -------- -------- -------- -------- -------- -------- -------- -------- B -1 1 1 1 1 1 147 Schoon GuestYBPD 1 5 0 39 39 295 297 2 Q/g1-e3 (0:05) Qe3 1 1 0\n\r\ETB")
      , (Boxed [NullCommand, GameMove {context = Just Illegal, move = Move {positionRaw = "-------- -------- -------- -------- -------- -------- -------- --------", position = [], turn = Black, doublePawnPush = Nothing, castlingAv = [WhiteShort,WhiteLong,BlackShort,BlackLong], ply = 1, Macbeth.Fics.Api.Move.gameId = 147, Macbeth.Fics.Api.Move.nameW = "Schoon", Macbeth.Fics.Api.Move.nameB = "GuestYBPD", relation = MyMove, initialTime = 5, incPerMove = 0, whiteRelStrength = 39, blackRelStrength = 39, remainingTimeW = 295, remainingTimeB = 297, moveNumber = 2, moveVerbose = Just(Simple (Square G One) (Square E Three)), timeTaken = "(0:05)", movePretty = Just "Qe3"}}],
          "\NAK6\SYN1\SYNIllegal move (b7b7).\n\r\a\n\r<12> -------- -------- -------- -------- -------- -------- -------- -------- B -1 1 1 1 1 1 147 Schoon GuestYBPD 1 5 0 39 39 295 297 2 Q/g1-e3 (0:05) Qe3 1 1 0\n\r\ETB")
      , (MatchRequested $ Challenge "Schoon" (Rating 997 None) "GuestPCFH" Unrated "unrated blitz 5 0", "Challenge: Schoon ( 997) GuestPCFH (----) unrated blitz 5 0.\n\r\aYou can \"accept\" or \"decline\", or propose different parameters.")
      ]

defaultMove = Move "-------- -------- -------- -------- -------- -------- -------- --------" [] White Nothing [WhiteShort,WhiteLong,BlackShort,BlackLong] 0 18 "nameWhite" "nameBlack" OponentsMove 3 0 39 39 180 180 1 Nothing "(0:00)" Nothing
defaultMoveStr = "<12> -------- -------- -------- -------- -------- -------- -------- -------- W -1 1 1 1 1 0 18 nameWhite nameBlack -1 3 0 39 39 180 180 1 none (0:00) none 1 0 0\n"


seekMsgParserTest :: [(FicsMessage, String)]
seekMsgParserTest = [
    (ClearSeek, "<sc>")
  , (RemoveSeeks [59, 3, 11], "<sr> 59 3 11")
  , (NewSeek $ Seek 7 "GuestNMZJ" [Unregistered] (Rating 0 Provisional) 15 5 False Standard (Just White) (0,9999), "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t")
  , (NewSeek $ Seek 16 "CatNail" [Computer] (Rating 1997 None) 3 0 False Suicide Nothing (0,9999), "<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f")
  , (NewSeek $ Seek 56 "GuestCXDH" [Unregistered] (Rating 0 Provisional) 7 0 False Wild (Just White) (0,9999), "<s> 56 w=GuestCXDH ti=01 rt=0P t=7 i=0 r=u tp=wild/4 c=W rr=0-9999 a=t f=f")
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
parseGamesListTest = case parseFicsMessage $ BS.pack games of
  Left txt -> Fail txt
  Right (Games games) -> if length games == 584 then Pass else Fail $ show $ length games
  where
    games = unsafePerformIO $ readFile "./test/Games.txt"

comparePosition :: Position -> String -> Result
comparePosition pos str = let pos' = parsePosition str
                          in if pos' == pos then Pass else Fail $ show pos ++ " <<-->> " ++ show pos'

compareCmdMsg :: String -> FicsMessage -> Result
compareCmdMsg str cmd = case parseFicsMessage $ BS.pack str of
  Left txt -> Fail txt
  Right cmd' -> if cmd' == cmd then Pass else Fail $ show cmd ++ " <<-->> " ++ show cmd'
