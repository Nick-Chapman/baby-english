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


comb :: [a -> Lexicon] -> a -> Lexicon
comb fs arg = combine $ map (\f -> f arg) fs

the_lexicon :: Lexicon
the_lexicon = everything
    where
        everything = combine [

            det   ["the","a","your","her","his","my","these","that","those"],
            pro   ["you","he","she","it","they","i","we","her","there","that","me","this"],
            prep  ["of","to","at","with","on","in","by","for","from","into","till"],
            prep' ["by","till"],
            cop   ["was","is","are","been","be","were"],
            conj  ["and","or","but"],
            particle ["up","down","out"],

            aux ["to","never","even","all","not","ne'er"],
            complementizer ["that","why","as","what"],

            --verbs ["do","has"],
            inflectedVerbs,
            advp ["slightly","playfully"], -- TODO, make a "ly" rule
            advp ["forward"],

            entry VP AdvP [vp] ["almost"], -- pre adverb

            entry VP Verb [vp] ["had","can","do"],

            properNoun
            ]

        complementizer = entry Comp Complementizer [sen]
        advp = entry AdvP AdvP []
        particle = entry PP Prep []
        aux = comb $ map (entry VP Aux) [[vp],[comp]]
        det = entry NP Det [nom]
        pro = entry NP Pro []
        prep = entry PP Prep [np]
        prep' = comb $ map (entry PP Prep) [[nom],[comp],[sen]]
        conj = entry Conj Conj []
        cop = comb $ map (entry VP Cop) ([pp]:[sen]:frames)

        inflectedVerbs = combine $ map (mkRule verbInflecion VP Verb) [[]]
        verbInflecion w = isEd w || isIng w
        isEd = ("ed" `isSuffixOf`)
        isIng = ("ing" `isSuffixOf`)

        frames = [
            [],
            [vp],
            [np],
            [comp]
            ]

        properNoun = combine [
            mkRule startsWithCapital NP Proper [],
            mkRule startsWithCapital Nom BlahNoun []
            ]

        startsWithCapital :: String -> Bool
        startsWithCapital s = head s `elem` ['A'..'Z']
