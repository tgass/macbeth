{-# LANGUAGE OverloadedStrings #-}

module FicsMessageParserSpec (spec) where

import Test.Hspec

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import qualified Macbeth.Fics.Api.Rating as R
import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import qualified Macbeth.Fics.Api.Player as P
import Macbeth.Fics.Parsers.FicsMessageParser
import Macbeth.Fics.Parsers.MoveParser hiding (move)
import Macbeth.Fics.Parsers.PositionParser

import Data.Attoparsec.ByteString.Char8 hiding (Fail, Result, D)
import qualified Data.ByteString.Char8 as BS

data Result = Pass | Fail String deriving (Eq)

spec :: Spec
spec =
  describe "Parser test" $ do

    it "user not logged in after observe" $ parseFicsMessage "\NAK6\SYN80\SYNDharmadhikari is not logged in.\n\ETB\n"
      `shouldBe` (Right $ UserNotLoggedIn "Dharmadhikari")

    it "command message parser" $ commandMessageParserTest `shouldBe` True

    it "seek msg parser" $ seekMsgParserTest `shouldBe` True

    it "move parser" $ moveParserTest `shouldBe` True

    it "position parser" $ positionTest `shouldBe` True

    it "ping" $ parseFicsMessage ":min/avg/max/mdev = 131.497/132.073/132.718/0.460 ms\n"
      `shouldBe` Right (Ping 131 132 133)

    it "pending from" $ parseFicsMessage "<pf> 28 w=Schoon t=match p=Schoon ( 999) GuestNXQS (----) unrated blitz 5 0\n"
      `shouldBe ` Right (Pending $ PendingOffer From 28 (P.UserHandle "Schoon" []) "match" "Schoon ( 999) GuestNXQS (----) unrated blitz 5 0")

    it "pendingTo" $ parseFicsMessage "\NAK4\SYN73\SYNIssuing: GuestFTYL (----) GuestCVXP (----) unrated blitz 5 0.\n\n<pt> 3 w=GuestCVXP t=match p=GuestFTYL (----) GuestCVXP (----) unrated blitz 5 0\n\ETB\n"
      `shouldBe` Right (Pending $ PendingOffer To 3 (P.UserHandle "GuestCVXP" []) "match" "GuestFTYL (----) GuestCVXP (----) unrated blitz 5 0")

    it "pending removed" $ parseFicsMessage "<pr> 28\n"
      `shouldBe` Right (PendingRemoved 28)

    it "pending removed, after withdraw" $ parseFicsMessage "\NAK4\SYN147\SYNYou withdraw the match offer to GuestFZHQ.\n\n<pr> 1\n\ETB\n"
      `shouldBe` Right (PendingRemoved 1)


commandMessageParserTest :: Bool
commandMessageParserTest = all (== Pass) $ fmap compareCmdMsg commandMessageParserTestData

commandMessageParserTestData :: [(FicsMessage, String)]
commandMessageParserTestData = [
        (DrawRequest, "GuestDWXY offers you a draw.")
      , (NoSuchGame, "\NAK5\SYN80\SYNThere is no such game.\n\ETB")
      , (MatchRequested $ Challenge "GuestYWYK" R.Unrated "GuestMGSD" R.Unrated "unrated blitz 2 12", "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12.")
      , (GuestLogin "FOOBAR", "Press return to enter the server as \"FOOBAR\":")
      , (GameCreation 484, "{Game 484 (GuestYLCL vs. GuestBYPB) Creating unrated blitz match.}\n")
      , (AbortRequest "GuestSPLL", "GuestSPLL would like to abort the game; type \"abort\" to accept.")
      , (TakebackRequest "GuestTYLF" 2, "GuestTYLF would like to take back 2 half move(s).")
      , (GameResult 82 "Game aborted by mutual agreement" Aborted, "\NAK5\SYN11\SYNYou accept the abort request from GuestSPLL.\n\n{Game 82 (GuestTKHJ vs. GuestSPLL) Game aborted by mutual agreement} *\n\ETB")
      , (GameResult 112 "Game aborted on move 1" Aborted, "\NAK4\SYN10\SYNThe game has been aborted on move one.\n\n{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *\n\ETB")
      , (GameResult 383 "Game aborted by mutual agreement" Aborted, "\NAK5\SYN10\SYN\n{Game 383 (GuestRVNY vs. GuestZTNM) Game aborted by mutual agreement} *\n\ETB")
      , (GameResult 112 "Game aborted on move 1" Aborted, "{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *")
      , (GameResult 297 "Game aborted by mutual agreement" Aborted, "{Game 297 (GuestSPLL vs. GuestTKHJ) Game aborted by mutual agreement} *")
      , (GameResult 10 "Kratt forfeits on time" WhiteWins, "{Game 10 (GidonC vs. Kratt) Kratt forfeits on time} 1-0\n")
      , (GameResult 368 "CalicoCat resigns" WhiteWins, "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0")
      , (GameResult 406 "GuestQLHT resigns" BlackWins, "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done.")
      , (GameResult 181 "Danimateit forfeits on time" BlackWins, "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1")
      , (GameResult 196 "Game drawn by mutual agreement" Draw, "\NAK4\SYN34\SYN\n{Game 196 (GuestCWVD vs. GuestDWTL) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (GameResult 202 "Game drawn by mutual agreement" Draw, "\NAK5\SYN11\SYNYou accept the draw request from GuestNMNG.\n\n{Game 202 (GuestDKZD vs. GuestNMNG) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB")
      , (MatchUserNotLoggedIn "GuestLHDG", "\NAK4\SYN73\SYNGuestLHDG is not logged in.\n\ETB")
      , (PieceHolding 455 [Pawn,Rook,Knight] [Bishop,Queen],  "<b1> game 455 white [PRN] black [BQ]")
      , (PieceHolding 182 [Pawn,Pawn,Bishop] [Pawn,Queen,Queen], "<b1> game 182 white [PPB] black [PQQ] <- BQ\n")
      , (SeekNotAvailable, "\NAK4\SYN158\SYNThat seek is not available.\n\ETB")

      , (MatchRequested $ Challenge "Schoon" (R.Rating 997 R.None) "GuestPCFH" R.Unrated "unrated blitz 5 0", "Challenge: Schoon ( 997) GuestPCFH (----) unrated blitz 5 0.\n\r\aYou can \"accept\" or \"decline\", or propose different parameters.")
      , (PromotionPiece Knight, "\NAK5\SYN92\SYNPromotion piece set to KNIGHT.\n\ETB\n")
      , (PromotionPiece Queen, "\NAK5\SYN92\SYNPromotion piece set to QUEEN.\n\ETB\n")
      , (PromotionPiece Bishop, "\NAK5\SYN92\SYNPromotion piece set to BISHOP.\n\ETB\n")
      , (PromotionPiece Rook, "\NAK5\SYN92\SYNPromotion piece set to ROOK.\n\ETB\n")
      , (PromotionPiece King, "\NAK5\SYN92\SYNPromotion piece set to KING.\n\ETB\n") -- Suicide
      ]

defaultMove = Move "-------- -------- -------- -------- -------- -------- -------- --------" [] White Nothing [WhiteShort,WhiteLong,BlackShort,BlackLong] 0 18 "nameWhite" "nameBlack" OponentsMove 3 0 39 39 180 180 1 Nothing "(0:00)" Nothing
defaultMoveStr = "<12> -------- -------- -------- -------- -------- -------- -------- -------- W -1 1 1 1 1 0 18 nameWhite nameBlack -1 3 0 39 39 180 180 1 none (0:00) none 1 0 0\n"

seekMsgParserTest :: Bool
seekMsgParserTest = all (== Pass) $ fmap compareCmdMsg seekMsgParserTestData

seekMsgParserTestData :: [(FicsMessage, String)]
seekMsgParserTestData = [
    (ClearSeek, "<sc>")
  , (RemoveSeeks [59, 3, 11], "<sr> 59 3 11")
  , (NewSeek $ Seek 7 "GuestNMZJ" [Unregistered] (R.Rating 0 R.Provisional) 15 5 False Standard (Just White) (0,9999), "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t")
  , (NewSeek $ Seek 16 "CatNail" [Computer] (R.Rating 1997 R.None) 3 0 False Suicide Nothing (0,9999), "<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f")
  , (NewSeek $ Seek 56 "GuestCXDH" [Unregistered] (R.Rating 0 R.Provisional) 7 0 False Wild (Just White) (0,9999), "<s> 56 w=GuestCXDH ti=01 rt=0P t=7 i=0 r=u tp=wild/4 c=W rr=0-9999 a=t f=f")
  ]


positionTest :: Bool
positionTest = all (== Pass) $ fmap comparePosition positionTestData


positionTestData :: [(Position, String)]
positionTestData = [
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

moveParserTest :: Bool
moveParserTest = all (== Pass) $ fmap withParser moveParserTestData

moveParserTestData :: [(Parser (Maybe MoveDetailed), String, Maybe MoveDetailed)]
moveParserTestData = [
    (verboseMove', "P/c7-c5", Just $ Simple (Square C Seven) (Square C Five))
  , (verboseMove', "P/f2-f1=R", Just $ Simple (Square F Two) (Square F One))
  , (verboseMove', "o-o", Just CastleShort)
  , (verboseMove', "o-o-o", Just CastleLong)
  , (verboseMove', "B/@@-g6", Just $ Drop $ Square G Six)
  ]

{-
parseGamesListTest :: Result
parseGamesListTest = case parseFicsMessage $ BS.pack games of
  Left txt -> Fail txt
  Right (Games games) -> if length games == 584 then Pass else Fail $ show $ length games
  where games = unsafePerformIO $ readFile "./test/Games.txt"
-}

withParser :: (Show a, Eq a) => (Parser a, String, a) -> Result
withParser (parser, str, x) = case parseOnly parser (BS.pack str) of
  Left txt -> Fail txt
  Right x' -> if x' == x then Pass else Fail $ show x ++ " <<==>> " ++ show x'


comparePosition :: (Position, String) -> Result
comparePosition (pos, str) = let pos' = parsePosition str
                             in if pos' == pos then Pass else Fail $ show pos ++ " <<-->> " ++ show pos'

compareCmdMsg :: (FicsMessage, String) -> Result
compareCmdMsg (cmd, str) = case parseFicsMessage $ BS.pack str of
  Left txt -> Fail txt
  Right cmd' -> if cmd' == cmd then Pass else Fail $ show cmd ++ " <<-->> " ++ show cmd'
