module Poker.Hand
    ( Hand
    , HandRank(..)
    , toList
    , toHand
    , handRank
    , getCandidatesFour
    , getCandidatesPair
    , getCandidatesHighCard
    , getCandidates
    , bestHand
    ) where

import Data.List (sort,sortBy,sortOn,(\\),nub)
import Data.Ord (Down(..), comparing)

import Utils.List (allEqual,sortedPartitionOn,subset)
import Poker.Card

type Hand = (Card,Card,Card,Card,Card)

handSize :: Int
handSize = 5

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

data HandRankable = HandRankable HandRank [CardRank]
    deriving (Eq)

instance Show HandRankable where
    show (HandRankable r cs) = show r ++ ' ' : show cs

instance Ord HandRankable where
    compare (HandRankable r0 cs0) (HandRankable r1 cs1) =
        compare r0 r1 <> compare cs0 cs1

toList :: Hand -> [Card]
toList (c0,c1,c2,c3,c4) = [c0,c1,c2,c3,c4]

toHand :: [Card] -> Maybe Hand
toHand [c0,c1,c2,c3,c4] = Just (c0,c1,c2,c3,c4)
toHand _                = Nothing

getRanks :: [Card] -> [CardRank]
getRanks = sort . map getRank

getSuits :: [Card] -> [CardSuit]
getSuits = map getSuit

isSuited :: [Card] -> Bool
isSuited = allEqual . getSuits

straightRanks :: [[CardRank]]
straightRanks = (RA:map toEnum [0..3]) : [map toEnum [n..n+4] | n <- [0..8]]

isStraight :: [Card] -> Bool
isStraight = flip elem straightRanks . getRanks

handRank :: Hand -> HandRankable
handRank hand
  | isStraightFlush && rh == RA = HandRankable RoyalFlush    []
  | isStraightFlush             = HandRankable StraightFlush [last ranks]
  | 4 `elem` sizesParted        = HandRankable Four          $ reverse ranksParted
  | [2,3] `subset` sizesParted  = HandRankable FullHouse     $ reverse ranksParted
  | isSuitedRes                 = HandRankable Flush         $ reverse ranks
  | isStraightRes               = HandRankable Straight      [last ranks]
  | 3 `elem` sizesParted        = HandRankable Three         $ last ranksParted : sortBy (comparing Down) (init ranksParted)
  | hasTwoPairs                 = HandRankable TwoPair       $ sortBy (comparing Down) (tail ranksParted) ++ [head ranksParted]
  | [2] `subset` sizesParted    = HandRankable Pair          $ last ranksParted : sortBy (comparing Down) (init ranksParted)
  | otherwise                   = HandRankable HighCard      $ reverse ranks
    where
        highCard@(sh,rh) = last $ sortOn getRank $ toList hand
        isSuitedRes      = isSuited . toList $ hand
        isStraightRes    = isStraight . toList $ hand
        isStraightFlush  = isSuitedRes && isStraightRes
        parts            = sortOn length $ sortedPartitionOn getRank . toList $ hand
        ranksParted      = map (getRank . head) parts
        sizesParted      = map length parts
        hasTwoPairs      = length (filter (2 ==) sizesParted) == 2
        ranks            = sort . getRanks . toList $ hand

bestQuads :: [Card]
bestQuads = secondHighestRankedCard : [ (s,RA) | s <- allSuits ]

guardHandSize :: ([a] -> [b]) -> ([a] -> [b])
guardHandSize f xs
  | length xs > handSize = []
  | otherwise            = f xs

getCandidatesFour :: [Card] -> [Card]
getCandidatesFour = guardHandSize f
    where
        f []    = bestQuads
        f cards =
            case sortOn length $ sortedPartitionOn getRank cards of
              [a]
                | [c] <- a  -> [ (s, aceOrKing . getRank $ c) | s <- allSuits ]
                | otherwise -> (minBound, aceOrKing . getRank . head $ a) : quadify a
              [a,b]
                | [_] <- a  -> quadify b
                | [_] <- b  -> quadify a
                | otherwise -> []
              _ -> []
            where
                quadify part = [ (s, getRank . head $ part) | s <- allSuits \\ getSuits part ]

getCandidatesPair :: [Card] -> [Card]
getCandidatesPair = guardHandSize f
    where
        f cards
          | length cards <= 2 = getCandidatesFour cards
          | length cards == 3 = let acesDiff = [ (s,maxBound) | s <- allSuits ] \\ cards
                                in take (handSize - length cards) $ acesDiff ++ [secondHighestRankedCard]
          | otherwise         =
              case sortOn (getRank . head) $ sortedPartitionOn getRank cards of
                [a]    -> [(minBound, aceOrKing . getRank . head $ a)]
                (x:xs) -> [head $ [ (s, getRank . head $ x) | s <- allSuits ] \\ x]

getCandidatesHighCard :: [Card] -> [Card]
getCandidatesHighCard = guardHandSize f
    where 
        f cards = take (handSize - length cards) $ highCards \\ cards
            where
                highCards = [ (s,maxBound) | s <- allSuits ] ++ [secondHighestRankedCard]

-- TODO
getCandidates :: HandRank -> [Card] -> [Card]
getCandidates rank cards
  | cardsSize >= handSize = []
  | otherwise = case rank of
                  RoyalFlush    -> if isSuitedRes
                                      then possibleRanks $ diff ranksRoyalFlush
                                      else []
                  StraightFlush -> if isSuitedRes
                                      then foldl (\acc x -> if null acc then possibleRanks . diff $ x else acc) [] ranksStraightFlushs
                                      else []
                  Four          -> case cardsSize of
                                     1 -> cards ++ [(SS,RA),(SC,RA),(SH,RA),(SD,RA)]
                                     _ -> case parts of
                                            [a]   -> nub cards ++ [ (s, getRank . head $ a) | s <- allSuits ]
                                            [a,b] -> []
                                            _     -> []
    where
        cardsSize           = length cards
        missingCardsSize    = handSize - cardsSize
        isSuitedRes         = allEqual . getSuits $ cards
        ranks               = sort . map getRank $ cards
        ranksRoyalFlush     = [RX,RJ,RQ,RK,RA]
        ranksStraightFlushs = [ map toEnum [0+n..4+n] | n <- reverse [(fromEnum RX)..(fromEnum RX)] ] ++ [[RA,R2,R3,R4,R5]]
        diff ranks'         = ranks' \\ ranks
        possibleRanks ranks = if (not . null $ cards) && length ranks == missingCardsSize then [ (getSuit . head $ cards, r) | r <- ranks ] else []
        parts               = sortOn length $ sortedPartitionOn getRank cards
        missingSuits        = allSuits \\ getSuits cards

bestHand :: [Card] -> HandRankable
bestHand = const $ HandRankable HighCard []

