{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Lib                            ( app )
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /decks" $ do
    it "responds with 200" $ do
      get "/decks" `shouldRespondWith` 200
    it "responds with [Deck]" $ do
      let
        decks
          = "[{\"deckId\":\"eb9b2844-c621-11eb-b8bc-0242ac130003\",\"deckName\":\"Fruits\",\"deckDescription\":\"Fruits in Spanish\"},{\"deckId\":\"f851c534-c621-11eb-b8bc-0242ac130003\",\"deckName\":\"Verbs\",\"deckDescription\":\"Verbs in Spanish\"}]"
      get "/decks" `shouldRespondWith` decks
  describe "GET /decks/:deckId" $ do
    describe "with valid deckId" $ do
      let deckUrl = "/decks/eb9b2844-c621-11eb-b8bc-0242ac130003"
      it "responds with 200" $ do
        get deckUrl `shouldRespondWith` 200
      it "responds with [Card]" $ do
        let
          cards
            = "[{\"cardId\":\"81a21d66-c622-11eb-b8bc-0242ac130003\",\"cardFront\":\"Apple\",\"cardBack\":\"Manzana\"},{\"cardId\":\"9f005602-c622-11eb-b8bc-0242ac130003\",\"cardFront\":\"Banana\",\"cardBack\":\"Plátano\"}]"
        get deckUrl `shouldRespondWith` cards
    describe "with invalid deckId" $ do
      it "responds with 404" $ do
        get "/decks/wrongId" `shouldRespondWith` 404
