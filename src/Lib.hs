{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Models                         ( Card
                                                , Deck
                                                , cardsById
                                                , decks
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type API =
  Get '[PlainText] String
  :<|> "decks" :> Get '[JSON] [Deck]
  :<|> "decks" :> Capture "deckId" String :> Get '[JSON] [Card]

port :: Int
port = 8080

startApp :: IO ()
startApp = do
  putStrLn $ "Server started on " ++ show port
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> getDecksHandler :<|> getCardsByIdHandler

homeHandler :: Handler String
homeHandler = return "We are alive!"

getDecksHandler :: Handler [Deck]
getDecksHandler = return decks

getCardsByIdHandler :: String -> Handler [Card]
getCardsByIdHandler deckId = case cardsById deckId of
  Just cards -> return cards
  Nothing    -> throwError $ err404 { errBody = "Deck not Found" }
