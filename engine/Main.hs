import Control.Monad (forM_)

import Poker.Card
import Poker.Game
import Poker.Hand
import Utils.Random

analyze :: Hand -> String
analyze hand = show hand ++ " -> " ++ show (handRank hand)

hands =
    [ ((SD,RX),(SD,RJ),(SD,RQ),(SD,RK),(SD,RA))
    , ((SD,R9),(SD,RX),(SD,RJ),(SD,RQ),(SD,RK))
    , ((SS,RX),(SC,RX),(SH,RX),(SD,RX),(SS,RJ))
    , ((SS,RX),(SC,RX),(SH,RX),(SD,RJ),(SS,RJ))
    , ((SD,RX),(SD,RJ),(SD,RQ),(SD,R2),(SD,R9))
    , ((SD,RX),(SS,RJ),(SH,RQ),(SS,RK),(SS,RA))
    , ((SS,RX),(SC,RX),(SH,RX),(SD,RJ),(SS,R3))
    , ((SS,RX),(SC,RX),(SH,RJ),(SD,RJ),(SS,R3))
    , ((SS,RX),(SC,RX),(SH,RJ),(SD,R2),(SS,R3))
    , ((SS,RX),(SC,RA),(SH,RJ),(SD,R2),(SS,R3))
    ]

main :: IO ()
main = do
    forM_ hands $ \hand -> do
        putStrLn $ analyze hand

analyzeRandom :: IO ()
analyzeRandom = do
    x <- takeShuffled 5 starterDeck
    let x' = map card x
    let x'' = toHand x'
    case x'' of
      Just hand -> do
          putStrLn $ analyze hand
      Nothing -> return ()

