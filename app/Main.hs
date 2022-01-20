module Main where

import Data.Foldable
import Data.Monoid
import Data.Semigroup
import Data.Tree
import Data.MultiSet (MultiSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Text.Printf

import qualified Data.MultiSet as MS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Parameters = Parameters
    { allowedGuesses :: Set Text
    , possibleSolutions :: Set Text
    } deriving (Eq, Ord, Read, Show)

data Match = Wrong | Misplaced | Correct deriving (Eq, Ord, Read, Show)
type Clue = [Match]

type Strategy = Parameters -> Text

right :: Char -> Char -> ([Either Match Char], MultiSet Char)
right g a = ([if mismatch then Right g else Left Correct], MS.fromList [a | mismatch])
    where mismatch = g /= a

rights :: Text -> Text -> ([Either Match Char], MultiSet Char)
rights guess answer = foldMap (uncurry right) (T.zip guess answer)

clue :: Text -> Text -> Clue
clue guess answer = uncurry go (rights guess answer) where
    go [] _ = []
    go (r:rs) s = c : go rs s' where
        (c, s') = case r of
            Left m -> (m, s)
            Right g | g `MS.member` s -> (Misplaced, MS.delete g s)
                    | otherwise -> (Wrong, s)

partition :: Text -> Set Text -> Map Clue (Set Text)
partition guess p = M.fromListWith (<>)
    [ (clue guess s, S.singleton s)
    | s <- S.toList p
    ]

type StrategyStatistics = (Max Int, Sum Int)

evaluateStrategy :: Parameters -> Strategy -> (Int, Double)
evaluateStrategy params strat = (depth, fromIntegral guesses / fromIntegral (S.size (possibleSolutions params))) where
    (Max depth, Sum guesses) = go 0 (possibleSolutions params)
    go d ss
        | S.size ss <= 1 = (Max d, Sum d)
        | otherwise = foldMap
            (go (d+1))
            (partition (strat params { possibleSolutions = ss }) ss)

flattenStrategy :: Parameters -> Strategy -> Forest (Text, Clue)
flattenStrategy params strat = go (possibleSolutions params) where
    go ss
        | S.size ss <= 1 = [Node (g', clue g' g') [] | g' <- S.toList ss]
        | otherwise =
            [ Node (g, c) (go ss')
            | (c, ss') <- M.assocs (partition g ss)
            ] where g = strat params { possibleSolutions = ss }

header, row :: String
header = "Maximum guesses needed\tAverage guesses needed\tStrategy\n"
row    = "%4d                  \t%7.2f               \t%s\n"

loadWordList :: FilePath -> IO (Set Text)
loadWordList fp = S.fromList . T.words <$> T.readFile fp

namedStrategies :: [(String, Strategy)]
namedStrategies = tail [undefined
    , ("alphabetical order", minStrat)
    , ("maximum entropy", maxEntropyStrat)
    , ("sum of squares", lNormStrat 2)
    , ("sum of cubes", lNormStrat 3)
    , ("sum of quads", lNormStrat 4)
    , ("worst case", lInfinityNormStrat)
    , ("clue count", clueCountStrat)
    ]

minStrat :: Strategy
minStrat = S.findMin . possibleSolutions

negEntropy :: (Foldable f, Functor f) => f Int -> Double
negEntropy ns = sum (h <$> ns) where
    h n = let p = fromIntegral n / denom in p * log p
    denom = fromIntegral (sum ns) :: Double

minimumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> a
minimumOn f = id
    . (\(Just (Min (Arg _ a))) -> a)
    . foldMap (\a -> Just (Min (Arg (f a) a)))

-- If your strategy only cares about how many solutions are left in each
-- category in the next step, you can use this to pick the guess that maximizes
-- some metric on the partitioning.
sizedStrat :: Ord a => (Map Clue Int -> a) -> Strategy
sizedStrat f (Parameters gs ss) = minimumOn (\g -> f (S.size <$> partition g ss)) gs

maxEntropyStrat :: Strategy
maxEntropyStrat = sizedStrat negEntropy

-- These next two avoid taking absolute values under the assumption that the
-- size of a set is always non-negative anyway.
lNormStrat :: Int -> Strategy
lNormStrat l = sizedStrat (sum . fmap (^l))

lInfinityNormStrat :: Strategy
lInfinityNormStrat = sizedStrat maximum

clueCountStrat :: Strategy
clueCountStrat = sizedStrat (negate . M.size)

pp :: (Text, Clue) -> String
pp (guess, clue) = concat (zipWith go (T.unpack guess) clue) where
    color Wrong = 47 :: Int
    color Misplaced = 43
    color Correct = 42
    go c m = printf "\ESC[97m\ESC[%dm%c\ESC[0m" (color m) c

printStrategy :: Parameters -> Strategy -> IO ()
printStrategy params strat = mapM_
    (putStr . drawTree . fmap pp)
    (flattenStrategy params strat)

main :: IO ()
main = do
    params <- pure Parameters
        <*> loadWordList "all.txt"
        <*> loadWordList "play.txt"
    -- printStrategy params maxEntropyStrat
    printf header
    for_ namedStrategies $ \(name, strat) -> do
        let (d, a) = evaluateStrategy params strat
        printf row d a name
