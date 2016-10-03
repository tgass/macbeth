{-# LANGUAGE OverloadedStrings #-}

module FicsMessageParserSpec (spec) where

import Test.Hspec

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import qualified Macbeth.Fics.Api.Rating as R
import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Seek
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

    it "command message parser" $ commandMessageParserTest `shouldBe` True

    it "seek msg parser" $ seekMsgParserTest `shouldBe` True

    it "move parser" $ moveParserTest `shouldBe` True

    it "position parser" $ positionTest `shouldBe` True

    it "ping" $ parseFicsMessage ":min/avg/max/mdev = 131.497/132.073/132.718/0.460 ms\n"
      `shouldBe` Right (Ping 131 132 133)

    it "pending from" $ parseFicsMessage "<pf> 28 w=Schoon t=match p=Schoon ( 999) GuestNXQS (----) unrated blitz 5 0\n"
      `shouldBe ` Right (Pending $ PendingOffer From 28 (P.UserHandle "Schoon" []) "match" "Schoon ( 999) GuestNXQS (----) unrated blitz 5 0")

    it "pending removed" $ parseFicsMessage "<pr> 28\n"
      `shouldBe` Right (PendingRemoved 28)

    it "no such game" $ parseFicsMessage "There is no such game.\n" `shouldBe` Right NoSuchGame

    it "user not logged in" $ parseFicsMessage "Dharmadhikari is not logged in.\n" `shouldBe` Right (UserNotLoggedIn "Dharmadhikari")

    it "match declined" $ parseFicsMessage "GuestHHZP declines the match offer.\n" `shouldBe` Right (OponentDecline "GuestHHZP" MatchReq)

    it "illegal move" $ parseFicsMessage "Illegal move (e2d2)." `shouldBe` Right (IllegalMove "e2d2")

    it "observing game" $ parseFicsMessage "Game 289: Erron (1686) Donattello (1731) rated lightning 1 0\n" `shouldBe` Right (Observing (GameId 289) "Erron" "Donattello")

commandMessageParserTest :: Bool
commandMessageParserTest = all (== Pass) $ fmap compareCmdMsg commandMessageParserTestData

commandMessageParserTestData :: [(FicsMessage, String)]
commandMessageParserTestData = [
        (DrawRequest, "GuestDWXY offers you a draw.")
      , (MatchRequested $ Challenge "GuestYWYK" R.Unrated "GuestMGSD" R.Unrated "unrated blitz 2 12", "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12.")
      , (GuestLogin "FOOBAR", "Press return to enter the server as \"FOOBAR\":")
      , (GameCreation (GameId 484) "GuestYLCL" "GuestBYPB", "{Game 484 (GuestYLCL vs. GuestBYPB) Creating unrated blitz match.}\n")
      , (AbortRequest "GuestSPLL", "GuestSPLL would like to abort the game; type \"abort\" to accept.")
      , (TakebackRequest "GuestTYLF" 2, "GuestTYLF would like to take back 2 half move(s).")
      , (PieceHolding (GameId 455) [Pawn,Rook,Knight] [Bishop,Queen],  "<b1> game 455 white [PRN] black [BQ]")
      , (PieceHolding (GameId 182) [Pawn,Pawn,Bishop] [Pawn,Queen,Queen], "<b1> game 182 white [PPB] black [PQQ] <- BQ\n")
      , (SeekNotAvailable, "\NAK4\SYN158\SYNThat seek is not available.\n\ETB")

      , (MatchRequested $ Challenge "Schoon" (R.Rating 997 R.None) "GuestPCFH" R.Unrated "unrated blitz 5 0", "Challenge: Schoon ( 997) GuestPCFH (----) unrated blitz 5 0.\n\r\aYou can \"accept\" or \"decline\", or propose different parameters.")
      , (PromotionPiece Knight, "Promotion piece set to KNIGHT.\n")
      , (PromotionPiece Queen, "Promotion piece set to QUEEN.\n")
      , (PromotionPiece Bishop, "Promotion piece set to BISHOP.\n")
      , (PromotionPiece Rook, "Promotion piece set to ROOK.\n")
      , (PromotionPiece King, "Promotion piece set to KING.\n") -- Suicide
      ]

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
