module Poker.Card
    ( Card(..)
    , CardiB(..)
    , CardSuit(..)
    , CardRank(..)
    , allSuits
    , allRanks
    , toCardiB
    , toCard
    , getSuit
    , getRank
    , aceOrKing
    , secondHighestRankedCard
    ) where

data CardSuit = SS|SC|SH|SD
    deriving (Eq,Enum,Bounded)

instance Show CardSuit where
    show SS = "S"
    show SC = "C"
    show SH = "H"
    show SD = "D"

allSuits :: [CardSuit]
allSuits = [minBound..maxBound]

data CardRank = R2|R3|R4|R5|R6|R7|R8|R9|RX|RJ|RQ|RK|RA
    deriving (Eq,Ord,Enum,Bounded)

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

allRanks :: [CardRank]
allRanks = [minBound..maxBound]

type Card = (CardSuit,CardRank)

-- card inclusus bonus
data CardiB = Card CardSuit CardRank | Joker
    deriving (Eq)

instance Show CardiB where
    show Joker      = "J*"
    show (Card s r) = show s ++ show r

toCardiB :: Card -> CardiB
toCardiB (s,r) = Card s r

toCard :: CardiB -> Card
toCard (Card s r) = (s,r)

getSuit :: Card -> CardSuit
getSuit = fst

getRank :: Card -> CardRank
getRank = snd

aceOrKing :: CardRank -> CardRank
aceOrKing r = if r == maxBound then toEnum $ fromEnum (maxBound :: CardRank) - 1 else maxBound

secondHighestRankedCard :: Card
secondHighestRankedCard = (minBound, aceOrKing maxBound)

