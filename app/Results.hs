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
import Numeric.Extra(intToDouble)
import Parse(parseTree)
import Tree(Tree,layTree)
import qualified Tree

data ExampleRun = ExampleRun
    { i :: Int,
      input :: String,
      lexicon :: Lexicon,
      results :: Maybe Results,
      unknown :: [String]
    }

data Results = Results
    { fullAmbCount :: Int
    , fullAmbIndex :: Double
    , selectedAmbCount :: Int
    , selectedAmbIndex :: Double
    , understanding :: Double
    , selected :: [Result]
    } deriving (Eq)

data Result = Result
    { tree :: Tree
    , wordCount :: Int
    , nodeCount :: Int
    , blahCount :: Int
    , understood :: Double
    , nps :: [String]
    } deriving (Eq)

makeExampleRun :: Lexicon -> (Int,String) -> ExampleRun
makeExampleRun lexicon (i,input) = ExampleRun {i,input,lexicon,results,unknown}
    where
        results = case parseTree lexicon input of
            Left _ -> Nothing
            Right trees -> Just $ makeResults trees
        unknown = nub parsedWords \\ dictWords
        parsedWords = case results of
            Nothing -> []
            Just r -> (map lower . Tree.words . tree . head . selected) r
        dictWords = Lexicon.words lexicon

makeResults :: [Tree] -> Results
makeResults trees = Results{..}
    where full = map makeResult trees
          selected = filter ((== minBlah) . blahCount) full
          minBlah = minimum (map blahCount full)
          fullAmbCount = length full
          fullAmbIndex = log (intToDouble fullAmbCount)
          selectedAmbCount = length selected
          selectedAmbIndex = log (intToDouble selectedAmbCount)
          understanding = maximum (map understood full)

makeResult :: Tree -> Result
makeResult tree = Result {..}
    where wordCount = Tree.wordCount tree
          nodeCount = Tree.nodeCount tree
          blahCount = Tree.blahCount tree
          understood = intToDouble (nodeCount-blahCount) / intToDouble nodeCount
          nps = Tree.allNps tree


layGrandSummary :: [ExampleRun] -> Layout ()
layGrandSummary xs = do
    flip (layListSep newline) xs $ \x -> do layExampleRun x
    lay "#runs = "; lay (show n); newline
    lay "#nope = "; lay (show (length nope)); newline
    lay "UNDERSTANDING "; lay (show (percent uu)); newline;
    lay "AMBIGUITY-1 "; lay (show a1); newline
    lay "AMBIGUITY-2 "; lay (show a2); newline
    newline
    lay "NEXT "; scope (flip (layListSep newline) top $ \(i,w) -> do lay (show i); lay " - "; lay w)
    newline
        where
            n = length xs
            nope = filter (\x -> results x == Nothing) xs
            uu = (sum $ map understanding $ catMaybes $ map results xs) / fromIntegral n
            rs = catMaybes $ map results xs
            a1 = sum $ map fullAmbIndex $ rs
            a2 = sum $ map selectedAmbIndex $ rs
            top = reverse (take 20 (reverse h))
            h = hist $ ws
            ws = concat $ map unknown xs

layExampleRun :: ExampleRun -> Layout ()
layExampleRun ExampleRun{i,input,results,unknown} = do
    bar; newline
    lay (show i); lay ": "; lay input; newline
    bar; newline;
    lay "unknown = "; layListSep (lay " ") lay unknown; newline
    scope (layMaybe layResults results)
    newline
    where bar = lay "--------------------------------------------------"

layResults :: Results -> Layout ()
layResults Results{..} = do
    --lay "fullAmbCount = "; lay (show fullAmbCount); newline
    --lay "fullAmbIndex = "; lay (show fullAmbIndex); newline
    --lay "selectedAmbCount = "; lay (show selectedAmbCount); newline
    --lay "selectedAmbIndex = "; lay (show selectedAmbIndex); newline
    lay "understanding = "; lay (show understanding); newline
    lay "amb = "; lay (show amb); newline
    newline
    lay "  "; scope $ layListSep (do newline; newline) (\(i,p) -> do lay (show i); lay ": "; layResult p) xs
    where amb = Fraction selectedAmbCount fullAmbCount
          xs = zip [1::Int ..] selected


layResult :: Result -> Layout ()
layResult Result{..} = scope $ do
    --lay "#words = "; lay (show wordCount); newline
    --lay "#nodes = "; lay (show nodeCount); newline
    --lay "#blahs = "; lay (show blahCount); newline
    --lay "understood = "; lay (show understood); newline
    layTree tree
    --lay "#nps = "; scope (mapM_ (\np -> do lay np; newline) nps)

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
