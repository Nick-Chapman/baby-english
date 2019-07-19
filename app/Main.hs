{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cat
import Lexicon
import Results
import Data.List

main :: IO ()
main = do
    contents <- readFile "data/86.text"
    let xs = map (makeExampleRun the_lexicon) (zip [1..] (lines contents))
    putStr (show (layGrandSummary xs))
    return ()

the_lexicon :: Lexicon
the_lexicon = everything
    where
        everything = combine [dets,pros,preps,cops,to_,that_,conjs,iverbs,tverbs,had_]

        dets = combine $ map det ["the","a","your","her","his","my","these"]
        pros = combine $ map pro ["you","he","she","it","they","i","we","her"]
        preps = combine $ map prep ["of","to","at","with","on","in","by","for","from"]

        det = entry NP [nom]
        pro = entry NP []
        prep = entry PP [np]

        cops = combine $ map cop ["was","is","are","been","be"]

        cop w = combine [
            entry VP [vp] w,
            entry VP [np] w,
            entry VP [pp] w
            ]

        to_ = entry Inf [vp] "to"
        that_ = combine [pro "that",det "that"] -- TODO: add complementizer

        conjs = combine $ map (entry Conj []) ["and","or"]

        iverbs = mkRule verbInflecion VP []
        tverbs = mkRule verbInflecion VP [np]

        verbInflecion w = isEd w || isIng w

        isEd = ("ed" `isSuffixOf`)
        isIng = ("ing" `isSuffixOf`)

        had_ = combine [
            entry VP [vp] "had",
            entry VP [np] "had"]
