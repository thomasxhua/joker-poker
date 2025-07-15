module Poker.Card
    ( Card(..)
    ) where

data Card = C2|C3|C4|C5|C6|C7|C8|C9|CX|CJ|CQ|CK|CA|C'
    deriving (Eq,Show)

