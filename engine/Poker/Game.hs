module Poker.Game
    ( Deck
    , starterDeck
    ) where

import Poker.Card

type Deck = [CardiB]

starterDeck :: Deck
starterDeck = [Card suit rank | suit <- suits, rank <- ranks] -- ++ [Joker, Joker]
    where
        suits = [SS,SC,SH,SD]
        ranks = [R2,R3,R4,R5,R6,R7,R8,R9,RX,RJ,RQ,RK,RA]

