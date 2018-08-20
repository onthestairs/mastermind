{-# LANGUAGE ScopedTypeVariables #-}

module Game.Mastermind
    (
    Colour(..),
    Player(..),
    playRound
    ) where

import           Data.List
import           Data.Maybe
import qualified Data.Set   as Set

data Colour = Red | Blue | Green | Yellow | Orange | Pink | Brown | White deriving (Eq, Show, Ord)
data ResponsePeg = CorrectColourAndPosition | CorrectColour deriving (Show)
type Response = [ResponsePeg]
type Code = [Colour]
type CodeGuess = [Colour]
type PlayableColours = Set.Set Colour

data Player s = Player {
  initState           :: PlayableColours -> s,
  incorporateResponse :: s -> CodeGuess -> Response -> s,
  guess               :: s -> CodeGuess
}

uncurry3 f = (\(a, b, c) -> f a b c)

getResponse :: Code -> CodeGuess -> Response
getResponse cs cgs =
  bothCorrectPegs ++ colourCorrectPegs
  where numberOfCorrectColourAndPosition = length $ filter (uncurry (==)) $ zip cs cgs
        bothCorrectPegs = (replicate numberOfCorrectColourAndPosition CorrectColourAndPosition)
        numberOfCorrectColour = (length cs) - (length (cs \\ cgs)) - numberOfCorrectColourAndPosition
        colourCorrectPegs = (replicate numberOfCorrectColour CorrectColour)

data Step a b = Loop a | Done b
tailRec :: forall a b. (a -> Step a b) -> a -> b
tailRec f x = case f x of
  Done z -> z
  Loop y -> tailRec f y

playRound :: forall s . PlayableColours -> Code -> Player s -> [CodeGuess]
playRound possibleColours code player =
  tailRec go ((initState player) possibleColours, [])
  where
    go :: (s, [CodeGuess]) -> Step (s, [CodeGuess]) [CodeGuess]
    go (state, history) = let nextGuess = (guess player) state
           in if nextGuess == code
              then (Done (history ++ [code]))
              else Loop (((incorporateResponse player) state nextGuess (getResponse code nextGuess)), history ++ [nextGuess])s
