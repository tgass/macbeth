{-# LANGUAGE OverloadedStrings #-}

module GameResultSpec (spec) where

import Test.Hspec

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Parsers.FicsMessageParser

spec :: Spec
spec =
  describe "GameResult test" $ do

    it "abort request agreed" $ parseFicsMessage "\NAK5\SYN11\SYNYou accept the abort request from GuestSPLL.\n\n{Game 82 (GuestTKHJ vs. GuestSPLL) Game aborted by mutual agreement} *\n\ETB"
      `shouldBe` Right (GameResult $ Result (GameId 82) "GuestTKHJ" "GuestSPLL" "Game aborted by mutual agreement" Aborted)

    it "abort by mutual agreement" $ parseFicsMessage "\NAK5\SYN10\SYN\n{Game 383 (GuestRVNY vs. GuestZTNM) Game aborted by mutual agreement} *\n\ETB"
      `shouldBe` Right (GameResult $ Result (GameId 383) "GuestRVNY" "GuestZTNM" "Game aborted by mutual agreement" Aborted)

    it "abort my mutual agreement" $ parseFicsMessage "{Game 297 (GuestSPLL vs. GuestTKHJ) Game aborted by mutual agreement} *"
      `shouldBe` Right (GameResult $ Result (GameId 297) "GuestSPLL" "GuestTKHJ" "Game aborted by mutual agreement" Aborted)

    it "abort on move one" $ parseFicsMessage "\NAK4\SYN10\SYNThe game has been aborted on move one.\n\n{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *\n\ETB"
      `shouldBe` Right (GameResult $ Result (GameId 112) "GuestSPLL" "GuestTKHJ" "Game aborted on move 1" Aborted)

    it "abort on move one, variation" $ parseFicsMessage "{Game 112 (GuestSPLL vs. GuestTKHJ) Game aborted on move 1} *"
      `shouldBe` Right (GameResult $ Result (GameId 112) "GuestSPLL" "GuestTKHJ" "Game aborted on move 1" Aborted)

    it "forfeits on time" $ parseFicsMessage "{Game 10 (GidonC vs. Kratt) Kratt forfeits on time} 1-0\n"
      `shouldBe` Right (GameResult $ Result (GameId 10) "GidonC" "Kratt" "Kratt forfeits on time" WhiteWins)

    it "player resigns" $ parseFicsMessage "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
      `shouldBe` Right (GameResult $ Result (GameId 368) "ALTOTAS" "CalicoCat" "CalicoCat resigns" WhiteWins)

    it "player resigns, no ratings adjusted" $ parseFicsMessage "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done."
      `shouldBe` Right (GameResult $ Result (GameId 406) "GuestQLHT" "GuestVYVJ" "GuestQLHT resigns" BlackWins)

    it "forteits on time, black wins" $ parseFicsMessage "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1"
      `shouldBe` Right (GameResult $ Result (GameId 181) "Danimateit" "WhatKnight" "Danimateit forfeits on time" BlackWins)

    it "drawn by mutual agreement" $ parseFicsMessage "\NAK4\SYN34\SYN\n{Game 196 (GuestCWVD vs. GuestDWTL) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB"
      `shouldBe` Right (GameResult $ Result (GameId 196) "GuestCWVD" "GuestDWTL" "Game drawn by mutual agreement" Draw)

    it "drawn by mutual agreement 2" $ parseFicsMessage "\NAK5\SYN11\SYNYou accept the draw request from GuestNMNG.\n\n{Game 202 (GuestDKZD vs. GuestNMNG) Game drawn by mutual agreement} 1/2-1/2\n\nNo ratings adjustment done.\n\ETB"
      `shouldBe` Right (GameResult $ Result (GameId 202) "GuestDKZD" "GuestNMNG" "Game drawn by mutual agreement" Draw)
