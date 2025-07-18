import Poker.Card
import Poker.Game
import Poker.Hand
import Utils.Random

main :: IO ()
main = do
    x <- takeShuffled 5 starterDeck
    let x' = map card x
    let x'' = toHand x'
    case x'' of
      Just hand -> do
          let rank = handRank hand
          putStrLn $ show x ++ " -> " ++ show rank
      Nothing -> return ()

