{-# LANGUAGE LambdaCase #-}

module Tree (
    Tree, mkWord, mkNode,
    layTree,
    catOf,
    size, blahCount,
    isSingleWord,
    isCat,
    words,
    allNps,
    ) where

import qualified Data.List as List
import Layout
import Cat
import Prelude hiding(words)
import Data.List (intersperse)

data Tree = Word Cat String | Node Cat [Tree] deriving (Eq)

catOf :: Tree -> Cat
catOf = \case
    Word pos _ -> pos
    Node cat _ -> cat

replaceCat :: Cat -> Tree -> Tree
replaceCat cat = \case
    Word _ w -> Word cat w
    Node _ trees -> Node cat trees


mkWord :: Cat -> String -> Tree
mkWord = Word

mkNode :: Cat -> [Tree] -> Tree
mkNode cat = \case
    [] -> error "mkNode,0"
    --[_] -> error "mkNode,1"
    [tree] -> replaceCat cat tree
    xs -> Node cat xs

instance Show Tree where
    show = oneline

oneline :: Tree -> String
oneline = \case
    Word pos s -> show pos <> "-" <> s
    Node cat trees -> "(" <> List.intercalate " " (show cat : map oneline trees) <> ")"

layTree :: Tree -> Layout ()
layTree = \case
    Word pos s -> layBracketed pos (lay s)
    Node cat trees ->
        layBracketed cat $
        scope $ sequence_ $ List.intersperse newline (map layTree trees)

layBracketed :: Cat -> Layout () -> Layout ()
layBracketed cat x = do
    lay "("; lay (show cat); lay " "; x; lay ")"


size :: Tree -> Int
size = \case
    Word _ _ -> 1
    Node _ trees -> 1 + (sum $ map size trees)

blahCount :: Tree -> Int
blahCount = \case
    Word Frag _ -> 1
    Word Blah _ -> 1
    Word _ _ -> 0
    Node Frag trees -> (length trees - 1) + (sum $ map blahCount trees)
    Node Blah trees -> (length trees - 1) + (sum $ map blahCount trees)
    Node _ trees -> sum $ map blahCount trees


isSingleWord :: Tree -> Bool
isSingleWord = \case
    Word _ _ -> True
    Node _ _ -> False

isCat :: Cat -> Tree -> Bool
isCat cat = \case
    Word pos _ -> cat==pos
    Node cat' _ -> cat==cat'

words :: Tree -> [String]
words = \case
    Word _ w -> [w]
    Node _ trees -> concat $ map words trees

text :: Tree -> String
text = concat . intersperse " " . words

allSubs :: Tree -> [Tree]
allSubs tree = tree : case tree of
    Word _ _ -> []
    Node _ trees -> trees >>= allSubs

allNps :: Tree -> [String]
allNps = map text . filter (isCat NP) . allSubs
