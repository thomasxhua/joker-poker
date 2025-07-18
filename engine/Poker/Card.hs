module Poker.Card
    ( Card(..)
    , CardiB(..)
    , CardSuit(..)
    , CardRank(..)
    , cardiB
    , card
    ) where

data CardSuit = SS|SC|SH|SD
    deriving (Eq)

instance Show CardSuit where
    show SS = "S"
    show SC = "C"
    show SH = "H"
    show SD = "D"

data CardRank = R2|R3|R4|R5|R6|R7|R8|R9|RX|RJ|RQ|RK|RA
    deriving (Eq,Ord,Enum)

instance Show CardRank where
    show R2 = "2"
    show R3 = "3"
    show R4 = "4"
    show R5 = "5"
    show R6 = "6"
    show R7 = "7"
    show R8 = "8"
    show R9 = "9"
    show RX = "X"
    show RJ = "J"
    show RQ = "Q"
    show RK = "K"
    show RA = "A"

type Card = (CardSuit,CardRank)

-- card inclusus bonus
data CardiB = Card CardSuit CardRank | Joker
    deriving (Eq)

instance Show CardiB where
    show Joker      = "J*"
    show (Card s r) = show s ++ show r

cardiB :: Card -> CardiB
cardiB (s,r) = Card s r

card :: CardiB -> Card
card (Card s r) = (s,r)
