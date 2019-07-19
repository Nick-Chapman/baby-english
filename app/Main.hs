{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cat
import Lexicon
import Parse(parseTree)
import Results
import Layout
import Data.Maybe

main :: IO ()
main = do
    contents <- readFile "data/86.text"
    let xs = map (makeExampleRun the_lexicon) (zip [1..] (lines contents))
    print (layGrandSummary xs)
    return ()

data ExampleRun = ExampleRun
    { i :: Int,
      input :: String,
      lexicon :: Lexicon,
      results :: Maybe Results
    }

makeExampleRun :: Lexicon -> (Int,String) -> ExampleRun
makeExampleRun lexicon (i,input) = ExampleRun {i,input,lexicon,results}
    where
        results =
            case parseTree lexicon input of
                Left _ -> Nothing
                Right trees -> Just $ makeResults trees

layMaybe :: (a -> Layout ()) -> Maybe a -> Layout ()
layMaybe layA = \case
    Nothing -> lay "<Nothing>"
    Just a -> layA a

layExampleRun :: ExampleRun -> Layout ()
layExampleRun ExampleRun{i,input,results} = do
    lay (show i); lay ": "; lay input; newline
    lay "-- "; scope (layMaybe layResults results)

layGrandSummary :: [ExampleRun] -> Layout ()
layGrandSummary xs = do
    flip mapM_ xs $ \x -> do layExampleRun x; newline
    lay "#runs = "; lay (show n); newline
    lay "#nope = "; lay (show (length nope)); newline
    lay "UNDERSTANDING "; lay (show (percent uu)); newline;
    lay "AMBIGUITY-1 "; lay (show a1); newline
    lay "AMBIGUITY-2 "; lay (show a2); newline
        where
            n = length xs
            nope = filter (\x -> results x == Nothing) xs
            uu = (sum $ map understanding $ catMaybes $ map results xs) / fromIntegral n
            a1 = sum $ map fullAmbIndex $ catMaybes $ map results xs
            a2 = sum $ map selectedAmbIndex $ catMaybes $ map results xs


data Percent = Percent Int
instance Show Percent where show (Percent n) = show n <> "%"
percent :: Double -> Percent
percent = Percent . truncate . (* 100.0)


the_lexicon :: Lexicon
the_lexicon = everything
    where
        everything = combine [dets,pros,preps,cops,to_,that_]

        dets = combine $ map det ["the","a","your","her"]
        pros = combine $ map pro ["you","he","she","it","they","i","we"]
        preps = combine $ map prep ["of","to","at","with","on","in"]

        det = entry NP [nom]
        pro = entry NP []
        prep = entry PP [np]

        cops = combine $ map cop ["was","is","are","been","be"]

        cop w = combine [
            entry VP [vp] w,
            entry VP [np] w
            ]

        to_ = entry Inf [vp] "to"

        that_ = combine [pro "that",det "that"] -- TODO: add complementizer
