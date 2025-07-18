module Poker.Hand
    ( Hand
    , HandRank(..)
    , toList
    , toHand
    , handRank
    ) where

import Data.List (sort,sortOn)

import Utils.List (allEqual,sortedPartition,subset)
import Poker.Card

type Hand = (Card,Card,Card,Card,Card)

data HandRank
    = HighCard
    | Pair
    | TwoPair
    | Three
    | Straight
    | Flush
    | FullHouse
    | Four
    | StraightFlush
    | RoyalFlush
    deriving (Show,Eq,Ord)

-- TODO: anything with a joker
--handRankJoker :: Hand -> (HandRank, [Card])
--handRankJoker = handRank

toList :: Hand -> [Card]
toList (c0,c1,c2,c3,c4) = [c0,c1,c2,c3,c4]

toHand :: [Card] -> Maybe Hand
toHand [c0,c1,c2,c3,c4] = Just (c0,c1,c2,c3,c4)
toHand _                = Nothing

getRanks :: Hand -> [CardRank]
getRanks ((_, r0), (_, r1), (_, r2), (_, r3), (_, r4)) = sort [r0,r1,r2,r3,r4]

getSuits :: Hand -> [CardSuit]
getSuits ((s0, _), (s1, _), (s2, _), (s3, _), (s4, _)) = [s0,s1,s2,s3,s4]

isSuited :: Hand -> Bool
isSuited = allEqual . getSuits

straightRanks :: [[CardRank]]
straightRanks = (RA:map toEnum [0..3]) : [map toEnum [n..n+4] | n <- [0..8]]

isStraight :: Hand -> Bool
isStraight = flip elem straightRanks . getRanks

handRank :: Hand -> HandRank
handRank hand
  | isStraightFlush && rh == RA = RoyalFlush
  | isStraightFlush             = StraightFlush
  | 4 `elem` sizes              = Four
  | [2,3] `subset` sizes        = FullHouse
  | isSuitedRes                 = Flush
  | isStraightRes               = Straight
  | 3 `elem` sizes              = Three
  | hasTwoPairs                 = TwoPair
  | [2] `subset` sizes          = Pair
  | otherwise                   = HighCard
    where
        highCard@(sh,rh) = last $ sortOn (\(s,r) -> r) $ toList hand
        isSuitedRes      = isSuited hand
        isStraightRes    = isStraight hand
        isStraightFlush  = isSuitedRes && isStraightRes
        sizes            = (map length) . sortedPartition . getRanks $ hand
        hasTwoPairs      = (length $ filter ((==) 2) sizes) == 2

