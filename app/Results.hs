{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Results(
    ExampleRun, makeExampleRun, layGrandSummary,
    ) where

import Data.List as List
import Data.List.Extra(lower)
import Data.Maybe
import Layout
import Lexicon
import Numeric
import Numeric.Extra(intToDouble)
import Parse(parseTree)
import Tree(Tree,layTree)
import qualified Tree

data ExampleRun = ExampleRun
    { i :: Int,
      input :: String,
      lexicon :: Lexicon,
      results :: Maybe Results,
      known :: [String],
      unknown :: [String]
    }

data Results = Results
    { fullAmbCount :: Int
    , fullAmbIndex :: Double
    , selectedAmbCount :: Int
    , selectedAmbIndex :: Double
    , understanding :: Double
    , full :: [Result]
    , selected :: [Result]
    , discarded :: [Result]
    , minPenalty :: Int
    , penalties :: [Int]
    } deriving (Eq)

data Result = Result
    { tree :: Tree
    , fullCount :: Int , blahCount :: Int, xfrags :: Int
    , penalty :: Int
    , understood :: Double
    -- , nps :: [String]
    } deriving (Eq)

makeExampleRun :: Lexicon -> (Int,String) -> ExampleRun
makeExampleRun lexicon (i,input) = ExampleRun {..}
    where
        results = case parseTree lexicon input of
            Left _ -> Nothing
            Right trees -> Just $ makeResults trees
        (known,unknown) = partition (Lexicon.inLexicon lexicon) parsedWords
        parsedWords = nub $ case results of
            Nothing -> []
            Just r -> (map lower . Tree.words . tree . head . selected) r

makeResults :: [Tree] -> Results
makeResults trees = Results{..}
    where full = map makeResult trees

          (selected,discarded) = partition ((== minPenalty) . penalty) full
          minPenalty = minimum penalties
          penalties = map penalty full

          fullAmbCount = length full
          fullAmbIndex = log (intToDouble fullAmbCount)
          selectedAmbCount = length selected
          selectedAmbIndex = log (intToDouble selectedAmbCount)
          understanding = sum (map understood selected) / fromIntegral (length selected)

makeResult :: Tree -> Result
makeResult tree = Result {..}
  where fullCount = Tree.fullCount tree
        blahCount = Tree.blahCount tree
        xfrags = Tree.fragCount tree - 1
        understood = intToDouble (fullCount - blahCount) / intToDouble fullCount
        penalty = 2 * xfrags + blahCount -- + fullCount
        -- nps = Tree.allNps tree


layGrandSummary :: [ExampleRun] -> Layout ()
layGrandSummary xs = do
    flip (layListSep newline) xs $ \x -> do layExampleRun x
    lay "#runs = "; lay (show n); newline
    lay "#nope = "; lay (show (length nopes));
    lay " ("; layListSep (lay " ") lay (map (show . i) nopes); lay ")"; newline
    lay "Effort "; layIndex 1 a1; newline
    lay "Understood "; lay (show (percent uu)); newline;
    lay "Ambiguity "; layIndex 1 a2; newline
    lay "Ambiguity/example "; layIndex 3 (a2 / fromIntegral nGood); newline
    newline
    lay "Next "; scope (flip (layListSep newline) top $ \(i,w) -> do lay (show i); lay " - "; lay w)
    newline
        where
            nGood = max 1 (n - length nopes)
            n = length xs
            nopes = filter (\x -> results x == Nothing) xs
            uu = (sum $ map understanding $ catMaybes $ map results xs) / fromIntegral n
            rs = catMaybes $ map results xs
            a1 = sum $ map fullAmbIndex $ rs
            a2 = sum $ map selectedAmbIndex $ rs
            top = reverse (take 10 (reverse h))
            h = hist $ ws
            ws = xs >>= unknown

layExampleRun :: ExampleRun -> Layout ()
layExampleRun ExampleRun{..} = do
    bar; newline
    lay "EXAMPLE "; lay (show i); lay ": "; lay input; newline
    bar; newline;
    lay "known = "; layListSep (lay " ") lay known; newline
    lay "unknown = "; layListSep (lay " ") lay unknown; newline
    scope (layMaybe layResults results)
    newline
    where bar = lay "--------------------------------------------------"


showDiscarded :: Bool
showDiscarded = False

layResults :: Results -> Layout ()
layResults Results{..} = do
    lay "minPenalty = "; lay (show minPenalty); newline
    --lay "penalties = "; lay (show penalties); newline
    lay "understanding = "; lay (show understanding); newline
    lay "amb = "; lay (show amb); newline
    newline
    lay "selected:  "; scope $ layListSep (do newline; newline) (\(i,p) -> do lay (show i); lay ": "; layResult p) xs
    if showDiscarded
        then do newline; newline; lay "discarded: "; scope $ layListSep (do newline; newline) (\(i,p) -> do lay (show i); lay ": "; layResult p) ys
        else return ()
    where amb = Fraction selectedAmbCount fullAmbCount
          xs = zip [1::Int ..] selected
          ys = zip [1::Int ..] (sortOn penalty discarded)


layResult :: Result -> Layout ()
layResult Result{..} = scope $ do
    lay "penalty = "; lay (show penalty); newline
    lay "#xfrags = "; lay (show xfrags); newline
    lay "blah/size = "; lay (show be); newline
    lay "understood = "; layIndex 3 understood; newline
    layTree tree
    --lay "#nps = "; scope (mapM_ (\np -> do lay np; newline) nps)
 where
     be = Fraction blahCount fullCount


layIndex :: Int -> Double -> Layout ()
layIndex n d = lay $ showFFloat (Just n ) d ""

layListSep :: Layout () -> (a -> Layout ()) -> [a] -> Layout ()

layListSep sep layA = \case
    [] -> return ()
    [a] -> layA a
    a:as -> do layA a; sep; layListSep sep layA as

layMaybe :: (a -> Layout ()) -> Maybe a -> Layout ()
layMaybe layA = \case
    Nothing -> lay "<Nothing>"
    Just a -> layA a

hist :: (Eq a,Ord a) => [a] -> [(Int,a)]
hist xs = sort counted
    where counted = map (\ys -> (length ys,head ys)) (group (sort xs))

data Percent = Percent Int
instance Show Percent where show (Percent n) = show n <> "%"
percent :: Double -> Percent
percent = Percent . truncate . (* 100.0)

data Fraction = Fraction Int Int
instance Show Fraction where show (Fraction a b) = show a <> "/" <> show b
