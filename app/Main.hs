module Main where

import qualified Data.Set        as Set
import           Debug.Trace
import           Game.Mastermind

trivialPlayer :: Player [[Colour]]
trivialPlayer = Player {
  initState           = \pcs -> [[Blue, Red, Blue], [Green, Yellow], [Blue, Blue]],
  incorporateResponse = \s cg r -> traceShow r $ tail s,
  guess = head
}

main :: IO ()
main = print guesses
  where guesses = playRound (Set.fromList [Blue, Red, Yellow]) [Blue, Blue] trivialPlayer
