{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lexicon(
    Comps(..),Lexicon,combine,entry,
    lookLexicon,inLexicon,
    ) where

import Prelude hiding(exp,fail,words,seq)
import EarleyM (Gram)
import qualified Data.List as List

import Cat
import Tree

type G = Gram Tree

data Comps = Comps {nom::G, np::G, vp::G, sen::G, phrase::G, comp::G}

type Sel = Comps -> G

newtype Lexicon = Lexicon { unLexicon :: [(String, ([Sel], Cat))] }

entry :: Cat -> [Sel] -> String -> Lexicon
entry cat sels w = Lexicon [(w,(sels,cat))]

combine :: [Lexicon] -> Lexicon
combine = Lexicon . concat . map unLexicon

lookLexicon :: Lexicon -> String -> Maybe ([Sel], Cat)
lookLexicon (Lexicon lexicon) w  = List.lookup w lexicon

inLexicon :: Lexicon -> String -> Bool
inLexicon (Lexicon lexicon) w = w `elem` map fst lexicon
