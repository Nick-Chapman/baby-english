{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lexicon(
    Comps(..),Lexicon,combine,entry,mkRule,
    lookLexicon,inLexicon,
    Lexicon.words,
    ) where

import Prelude hiding(exp,fail,words,seq,pred)
import EarleyM (Gram)
import Cat
import Tree

import Data.Map(Map)
import qualified Data.Map as Map hiding (map,filter)

type G = Gram Tree
data Comps = Comps {nom::G, np::G, pp::G, advp::G, vp::G, sen::G, phrase::G, comp::G}
type Sel = Comps -> G

type Pos = Cat

type Entry = (Cat, (Pos, [Sel]))

data Lexicon = Lexicon
    { entries :: Map String [Entry]
    , rule :: String -> [Entry]
    }

empty :: Lexicon
empty = Lexicon { entries = Map.empty, rule = const [] }

union :: Lexicon -> Lexicon -> Lexicon
union a b =
    Lexicon
    { entries = Map.unionWith (++) (entries a) (entries b)
    , rule = \w -> rule a w ++ rule b w
    }

combine :: [Lexicon] -> Lexicon
combine = foldl union empty

inLexicon :: Lexicon -> String -> Bool
inLexicon Lexicon{entries} w = Map.member w entries

words :: Lexicon -> [String]
words Lexicon {entries} = Map.keys entries

lookLexicon :: Lexicon -> String -> [Entry]
lookLexicon Lexicon{entries,rule} w = Map.findWithDefault [] w entries ++ rule w

entry :: Cat -> Cat -> [Sel] -> [String] -> Lexicon
entry cat pos sels ws =
    combine $ flip map ws $ \w ->
    Lexicon
    { entries = Map.singleton w [(cat,(pos,sels))]
    , rule = const []
    }

mkRule :: (String -> Bool) -> Cat -> Cat -> [Sel] -> Lexicon
mkRule pred cat pos sels =
    Lexicon
    { entries = Map.empty
    , rule = \w -> if pred w then [(cat,(pos,sels))] else []
    }
