{-# LANGUAGE ScopedTypeVariables #-}

module Game.Mastermind
    (
    Colour(..),
    Player(..),
    ResponsePegs(..),
    CodeLength,
    CodeGuess,
    knownCodeResponder,
    inputCodeResponder,
    playRound,
    ) where

import           Data.List
import           Data.Maybe
import qualified Data.Set   as Set

data Colour = Red | Blue | Green | Yellow | Orange | Pink | Brown | White deriving (Eq, Ord)
reset = "\x1b[0m"
printInColour Red s    = "\x1b[38;5;160m" ++ s ++ reset
printInColour Blue s   = "\x1b[38;5;21m" ++ s ++ reset
printInColour Green s  = "\x1b[38;5;118m" ++ s ++ reset
printInColour Yellow s = "\x1b[38;5;226m" ++ s ++ reset
printInColour Orange s = "\x1b[38;5;202m" ++ s ++ reset
printInColour Pink s   = "\x1b[38;5;206m" ++ s ++ reset
printInColour Brown s  = "\x1b[38;5;94m" ++ s ++ reset
printInColour White s  = "\x1b[38;5;15m" ++ s ++ reset
instance Show Colour where
  show c = printInColour c "⬤"

-- data ResponsePeg = CorrectColourAndPosition | CorrectColour deriving (Show)
type NumberOfCorrectColourAndPosition = Int
type NumberOfCorrectColour = Int
data ResponsePegs = ResponsePegs NumberOfCorrectColourAndPosition NumberOfCorrectColour deriving (Show)
data Response = Correct | Incorrect ResponsePegs deriving (Show)
type Code = [Colour]
type CodeGuess = [Colour]
type PlayableColours = Set.Set Colour
type CodeLength = Int

data Player s = Player {
  initState           :: PlayableColours -> CodeLength -> s,
  incorporateResponse :: s -> CodeGuess -> ResponsePegs -> s,
  guess               :: s -> CodeGuess
}

uncurry3 f = (\(a, b, c) -> f a b c)

getResponse :: Code -> CodeGuess -> Response
getResponse cs cgs =
  if cs == cgs
    then Correct
    else
      let numberOfCorrectColourAndPosition = length $ filter (uncurry (==)) $ zip cs cgs
          numberOfCorrectColour = (length cs) - (length (cs \\ cgs)) - numberOfCorrectColourAndPosition
      in Incorrect (ResponsePegs numberOfCorrectColourAndPosition numberOfCorrectColour)

knownCodeResponder :: Code -> CodeGuess -> IO Response
knownCodeResponder c cg = pure $ getResponse c cg

parseResponse :: String -> Response
parseResponse "CORRECT" = Correct
parseResponse xs        = Incorrect (ResponsePegs bothCorrectPegs colourCorrectPegs)
  where bothCorrectPegs   = length $ filter ((==) 'b') xs
        colourCorrectPegs = length $ filter ((==) 'w') xs
inputCodeResponder :: CodeGuess -> IO Response
inputCodeResponder cg = do
  putStrLn $ ("Player has placed: ") ++ (intercalate " " (map show cg))
  putStr "Enter the response: "
  fmap parseResponse getLine

data Step a b = Loop a | Done b
tailRecIO :: forall a b. (a -> IO (Step a b)) -> a -> IO b
tailRecIO f x =
  do
    y <- f x
    case y of
      Done z -> pure z
      Loop z -> tailRecIO f z

playRound :: forall s . (CodeGuess -> IO Response) -> PlayableColours -> CodeLength -> Player s -> IO [CodeGuess]
playRound responder possibleColours n player =
  tailRecIO go ((initState player) possibleColours n, [])
  where
    go (state, history) = do
      let nextGuess = (guess player) state
      response <- responder nextGuess
      case response of
        Correct -> pure $ Done (history ++ [nextGuess])
        Incorrect pegs -> pure $ Loop (((incorporateResponse player) state nextGuess pegs), history ++ [nextGuess])
