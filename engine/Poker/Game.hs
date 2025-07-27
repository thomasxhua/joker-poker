module Poker.Game
    ( Deck
    , starterDeck
    ) where

import Poker.Card

type Deck = [CardiB]

starterDeck :: Deck
starterDeck = [Card suit rank | suit <- allSuits, rank <- allRanks] -- ++ [Joker, Joker]

