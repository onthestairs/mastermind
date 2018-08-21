module Game.Mastermind.Players
    (
    trivialPlayer,
    goodPlayer,
    goodPlayer2
    ) where

import           Data.List       (delete, maximumBy, minimumBy, partition)
import           Data.Ord        (comparing)
import qualified Data.Set        as Set
import           Game.Mastermind

makeProduct :: Int -> [a] -> [[a]]
makeProduct 0 xs = [[]]
makeProduct n xs = [x:ys | ys <- makeProduct (n-1) xs, x <- xs]

trivialPlayer :: Player [[Colour]]
trivialPlayer = Player {
  initState           = \pcs n -> makeProduct n (Set.toList pcs),
  incorporateResponse = \s cg r -> tail s,
  guess = head
}

---------------

intersection [] ys = []
intersection (x:xs) ys =
  if x `elem` ys
    then x:(intersection xs (delete x ys))
    else intersection xs ys
isPossible cgs (ResponsePegs correctCandP correctC) cs =
  hasCorrectColourAndPositions && hasCorrectColours
  where (same, different) = partition (uncurry (==)) (zip cgs cs)
        hasCorrectColourAndPositions = correctCandP == length same
        hasCorrectColours = correctC == (length $ intersection (map fst different) (map snd different))
pruneIllegalCandidates possibleCandidates cg r = Set.filter (isPossible cg r) possibleCandidates

maximumGuaranteedCandidatesLeft n candidates guess =
  maximum $ map (\responsePegs -> length $ pruneIllegalCandidates candidates guess responsePegs) allResponsePegs
  where
    allResponsePegs = [ResponsePegs p q | p <- [0..n], q <- [0..n], p+q <= n]
makeGuess :: CodeLength -> Set.Set CodeGuess -> CodeGuess
makeGuess n possibleCandidates =
  minimumBy (comparing (maximumGuaranteedCandidatesLeft n possibleCandidates)) possibleCandidates


goodPlayer = Player {
  initState           = \pcs n -> (n, Set.fromList (makeProduct n (Set.toList pcs))),
  incorporateResponse = \(n, possibleCandidates) cg r -> (n, pruneIllegalCandidates possibleCandidates cg r),
  guess = \(n, s) -> makeGuess n s
}

goodPlayer2 = Player {
  initState           = \pcs n -> (n, Set.fromList (makeProduct n (Set.toList pcs))),
  incorporateResponse = \(n, possibleCandidates) cg r -> (n, pruneIllegalCandidates possibleCandidates cg r),
  guess = \(n, s) -> if length s > 2000 then Set.elemAt 1 s else makeGuess n s
}
