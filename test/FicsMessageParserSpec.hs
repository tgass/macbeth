{-# LANGUAGE OverloadedStrings #-}

module FicsMessageParserSpec (spec) where

import Test.Hspec
import Data.Attoparsec.ByteString.Char8 (parseOnly)

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Parsers.FicsMessageParser
import Macbeth.Fics.Parsers.MoveParser hiding (move)
import Macbeth.Fics.Parsers.PositionParser

import qualified Macbeth.Fics.Api.Rating as R
import qualified Macbeth.Fics.Api.Player as P


spec :: Spec
spec =
  describe "Parser test" $ do

    it "ping" $ parseFicsMessage ":min/avg/max/mdev = 131.497/132.073/132.718/0.460 ms\n" `shouldBe` Right (Ping 131 132 133)

    it "pending from" $ parseFicsMessage "<pf> 28 w=Schoon t=match p=Schoon ( 999) GuestNXQS (----) unrated blitz 5 0\n" `shouldBe ` Right (Pending $ PendingOffer From 28 (P.UserHandle "Schoon" []) "match" $ GameParams "Schoon" (R.Rating 999 R.None) "GuestNXQS" R.Unrated False "blitz" 5 0)

    it "pending removed" $ parseFicsMessage "<pr> 28\n" `shouldBe` Right (PendingRemoved 28)

    it "no such game" $ parseFicsMessage "There is no such game.\n" `shouldBe` Right NoSuchGame

    it "user not logged in" $ parseFicsMessage "Dharmadhikari is not logged in.\n" `shouldBe` Right (UserNotLoggedIn "Dharmadhikari")

    it "match declined" $ parseFicsMessage "GuestHHZP declines the match offer.\n" `shouldBe` Right (OponentDecline "GuestHHZP" MatchReq)

    it "illegal move" $ parseFicsMessage "Illegal move (e2d2)." `shouldBe` Right (IllegalMove "e2d2")

    it "observing game" $ parseFicsMessage "Game 289: Erron (1686) Donattello (1731) rated lightning 1 0\n" `shouldBe` Right (GameCreation $ GameProperties (GameId 289) "Erron" "Donattello" False)

    it "create game" $ parseFicsMessage "{Game 484 (GuestYLCL vs. GuestBYPB) Creating unrated blitz match.}\n" `shouldBe` Right (GameCreation $ GameProperties (GameId 484) "GuestYLCL" "GuestBYPB" True)

    it "draw request" $ parseFicsMessage "GuestDWXY offers you a draw." `shouldBe` Right (DrawRequest "GuestDWXY")

    it "guest login" $ parseFicsMessage "Press return to enter the server as \"FOOBAR\":" `shouldBe` Right (GuestLogin "FOOBAR")

    it "challenge" $ parseFicsMessage "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12." `shouldBe` Right (MatchRequested $ Challenge $ GameParams "GuestYWYK" R.Unrated "GuestMGSD" R.Unrated False "blitz" 2 12)

    it "match requested" $ parseFicsMessage "Challenge: Schoon ( 997) GuestPCFH (----) unrated blitz 5 0.\n\r\aYou can \"accept\" or \"decline\", or propose different parameters." `shouldBe` Right (MatchRequested $ Challenge $ GameParams "Schoon" (R.Rating 997 R.None) "GuestPCFH" R.Unrated False "blitz" 5 0)

    it "abort request" $ parseFicsMessage "GuestSPLL would like to abort the game; type \"abort\" to accept." `shouldBe` Right (AbortRequest "GuestSPLL")

    it "takeback request" $ parseFicsMessage "GuestTYLF would like to take back 2 half move(s)." `shouldBe` Right (TakebackRequest "GuestTYLF" 2)

    it "parse piece holding" $ parseFicsMessage "<b1> game 455 white [PRN] black [BQ]" `shouldBe` Right (PieceHolding (GameId 455) [Pawn,Rook,Knight] [Bishop,Queen])

    it "parse piece holding" $ parseFicsMessage "<b1> game 182 white [PPB] black [PQQ] <- BQ\n" `shouldBe` Right (PieceHolding (GameId 182) [Pawn,Pawn,Bishop] [Pawn,Queen,Queen])

    it "parse seek not available" $ parseFicsMessage "That seek is not available.\n" `shouldBe` Right SeekNotAvailable

    it "parse promotion piece" $ parseFicsMessage "Promotion piece set to KNIGHT.\n" `shouldBe` Right (PromotionPiece Knight)

    it "parse promotion piece" $ parseFicsMessage "Promotion piece set to BISHOP.\n" `shouldBe` Right (PromotionPiece Bishop)

    it "parse promotion piece" $ parseFicsMessage "Promotion piece set to ROOK.\n" `shouldBe` Right (PromotionPiece Rook)

    it "parse promotion piece" $ parseFicsMessage "Promotion piece set to QUEEN.\n" `shouldBe` Right (PromotionPiece Queen)

    it "parse promotion piece" $ parseFicsMessage "Promotion piece set to KING.\n" `shouldBe` Right (PromotionPiece King)

    it "parse clear seek" $ parseFicsMessage "<sc>" `shouldBe` Right ClearSeek

    it "parse remove seek" $ parseFicsMessage "<sr> 59 3 11" `shouldBe` Right (RemoveSeeks [59, 3, 11])

    it "parse new seek" $ parseFicsMessage "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t" `shouldBe` Right (NewSeek $ Seek 7 "GuestNMZJ" [Unregistered] (R.Rating 0 R.Provisional) 15 5 False Standard (Just White) (0,9999))

    it "parse new seek" $ parseFicsMessage "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t" `shouldBe` Right (NewSeek $ Seek 7 "GuestNMZJ" [Unregistered] (R.Rating 0 R.Provisional) 15 5 False Standard (Just White) (0,9999))

    it "parse new seek" $ parseFicsMessage "<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f" `shouldBe` Right (NewSeek $ Seek 16 "CatNail" [Computer] (R.Rating 1997 R.None) 3 0 False Suicide Nothing (0,9999))

    it "parse new seek" $ parseFicsMessage "<s> 56 w=GuestCXDH ti=01 rt=0P t=7 i=0 r=u tp=wild/4 c=W rr=0-9999 a=t f=f" `shouldBe` Right (NewSeek $ Seek 56 "GuestCXDH" [Unregistered] (R.Rating 0 R.Provisional) 7 0 False Wild (Just White) (0,9999))

    it "parse move" $ parseOnly verboseMove' "P/c7-c5" `shouldBe` Right (Just $ Simple (Square C Seven) (Square C Five))

    it "parse move with conversion" $ parseOnly verboseMove' "P/f2-f1=R" `shouldBe` Right (Just $ Simple (Square F Two) (Square F One))

    it "parse short castle" $ parseOnly verboseMove' "o-o" `shouldBe` Right (Just CastleShort)

    it "parse long castle" $ parseOnly verboseMove' "o-o-o" `shouldBe` Right (Just CastleLong)

    it "parse piece drop" $ parseOnly verboseMove' "B/@@-g6" `shouldBe` Right (Just $ Drop $ Square G Six)

    it "parse empty position" $ parsePosition "-------- -------- -------- -------- -------- -------- -------- --------" `shouldBe` []

    it "parse position" $ parsePosition "R------- -N------ --B----- ---Q---- ----K--- -----P-- ------r- -------k" `shouldBe`
     [ (Square A Eight, Piece Rook White)
     , (Square B Seven, Piece Knight White)
     , (Square C Six, Piece Bishop White)
     , (Square D Five, Piece Queen White)
     , (Square E Four, Piece King White)
     , (Square F Three, Piece Pawn White)
     , (Square G Two, Piece Rook Black)
     , (Square H One, Piece King Black)]

