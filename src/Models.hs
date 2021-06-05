{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Models
  ( Deck
  , Card
  , decks
  , cardsById
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map                      as Map
                                                ( Map
                                                , fromList
                                                , lookup
                                                )
import           Data.Maybe


data Deck = Deck
  { deckId          :: String
  , deckName        :: String
  , deckDescription :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Deck)

data Card = Card
  { cardId    :: String
  , cardFront :: String
  , cardBack  :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Card)

cardsById :: String -> Maybe [Card]
cardsById key = Map.lookup key cards

decks :: [Deck]
decks =
  [ Deck "eb9b2844-c621-11eb-b8bc-0242ac130003" "Fruits" "Fruits in Spanish"
  , Deck "f851c534-c621-11eb-b8bc-0242ac130003" "Verbs"  "Verbs in Spanish"
  ]

cards :: Map.Map String [Card]
cards = Map.fromList
  [ ( "eb9b2844-c621-11eb-b8bc-0242ac130003"
    , [ Card "81a21d66-c622-11eb-b8bc-0242ac130003" "Apple"  "Manzana"
      , Card "9f005602-c622-11eb-b8bc-0242ac130003" "Banana" "Plátano"
      ]
    )
  , ( "f851c534-c621-11eb-b8bc-0242ac130003"
    , [ Card "7045c215-e686-49be-9cb6-8a0616f77369" "To Eat"   "Comer"
      , Card "631ccc72-c629-11eb-b8bc-0242ac130003" "To Open"  "Abrir"
      , Card "77b6098c-c629-11eb-b8bc-0242ac130003" "To Watch" "Mirar"
      ]
    )
  ]
