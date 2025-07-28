{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Poker.Game
    ( RoundState(..)
    , RoundAction(..)
    , Chips(..)
    , PlayerID(..)
    , Player(..)
    , GameState(..)
    , starterDeck
    ) where

import Data.Maybe (fromMaybe)

import Poker.Card
import Utils.Random
import Utils.List

starterDeck :: [CardiB]
starterDeck = [Card suit rank | suit <- allSuits, rank <- allRanks] -- ++ [Joker, Joker]


data RoundState = RSEmpty|RSPreFlop|RSFlop|RSTurn|RSRiver
    deriving (Show)

data RoundAction = RANone|RAFold|RACheck|RABet Int|RARaise Int
    deriving (Show)

type Chips    = Int
type PlayerID = Int

data PlayerState = PSFolded|PSActive|PSAllIn
    deriving (Show)

data Player = Player
    { playerID  :: PlayerID
    , state     :: PlayerState
    , stack     :: Chips
    , holeCards :: Maybe (Card,Card)
    } deriving (Show)

data GameState = GameState
    { roundNum     :: Int
    , roundState   :: RoundState
    , smallBlind   :: Chips
    , bigBlind     :: Chips
    , dealerID     :: PlayerID
    , smallBlindID :: PlayerID
    , bigBlindID   :: PlayerID
    , pot          :: Chips
    , splitPots    :: [(PlayerID,Chips)]
    , deck         :: [CardiB]
    , players      :: [Player]
    , toActID      :: PlayerID
    , lastAction   :: RoundAction
    , currentMaxID :: PlayerID
    } deriving (Show)

data GameStateGenerator = GameStateGenerator
    { playerCount    :: Int
    , startingStacks :: [(PlayerID,Chips)]
    , smallBlind     :: Chips
    , bigBlind       :: Chips
    , dealerID       :: PlayerID
    } deriving (Show)

nonePlayerID :: PlayerID
nonePlayerID = -1

nextPlayerID :: [Player] -> PlayerID -> Maybe PlayerID
nextPlayerID players id =
    case findNextInCycleOn (\r -> r.playerID) players id of
      Just player -> Just player.playerID
      _           -> Nothing

generateInitialGameState :: GameStateGenerator -> IO GameState
generateInitialGameState gsg = do
    shuffled <- shuffle starterDeck
    let maxID         = playerCount gsg - 1
        freshPlayers  = [ Player {playerID=id, state=PSFolded, stack=0, holeCards=Nothing} | id <- [0..maxID] ]
        nextID id     = fromMaybe nonePlayerID (nextPlayerID freshPlayers id)
        dealerPlus1ID = nextID gsg.dealerID
        dealerPlus2ID = nextID dealerPlus1ID
        (sbID,bbID)
          | gsg.playerCount == 2 = (gsg.dealerID, dealerPlus1ID)
          | otherwise            = (dealerPlus1ID, dealerPlus2ID)
    return GameState
        { roundNum     = 0
        , roundState   = RSEmpty
        , smallBlind   = gsg.smallBlind
        , bigBlind     = gsg.bigBlind
        , dealerID     = gsg.dealerID
        , smallBlindID = sbID
        , bigBlindID   = bbID
        , pot          = 0
        , splitPots    = []
        , deck         = shuffled
        , players      = freshPlayers
        , toActID      = nonePlayerID
        , lastAction   = RANone
        , currentMaxID = maxID 
        }

