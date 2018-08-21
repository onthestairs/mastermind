module Main where

import           Data.List
import qualified Data.Set                as Set
import           Debug.Trace
import           Game.Mastermind
import           Game.Mastermind.Players (goodPlayer, goodPlayer2,
                                          trivialPlayer)


possibleColours = Set.fromList [Red, Blue, Green, Yellow, Orange, Pink, Brown, White]
knownCode = [Blue, Orange, Brown, Red, Orange]
n = length knownCode

main :: IO ()
main = do
  guesses <- playRound (knownCodeResponder knownCode) possibleColours n goodPlayer2
  -- guesses <- playRound inputCodeResponder (Set.fromList [Blue, Red, Yellow]) 4 trivialPlayer
  mapM_ (\guess -> putStrLn $ intercalate " " (map show guess)) guesses
