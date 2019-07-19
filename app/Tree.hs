{-# LANGUAGE LambdaCase #-}

module Tree (
    Tree, mkWord, mkNode,
    layTree,
    wordCount,
    nodeCount,
    blahCount,
    rankTrees,
    isSingleWord,
    isCat,
    allNps,
    ) where

import qualified Data.List as List
import Layout
import Cat
import Prelude hiding(words)
import Data.List (intersperse)

data Tree = Word String | Node Cat [Tree] deriving (Eq)

mkWord :: String -> Tree
mkWord = Word

mkNode :: Cat -> [Tree] -> Tree
mkNode = Node

instance Show Tree where
    show = _oneline
    --show = Layout.toString . layTree

_oneline :: Tree -> String
_oneline = \case
    Word s -> s
    Node cat trees -> "(" <> List.intercalate " " (show cat : map _oneline trees) <> ")"

layTree :: Tree -> Layout ()
layTree = \case
    Word s -> lay s
    Node cat trees -> do
        lay "("
        lay (show cat);
        lay " "
        scope $ sequence_ $ List.intersperse newline (map layTree trees)
        lay ")"

rankTrees :: [Tree] -> [Tree]
rankTrees = List.sortOn blahCount

wordCount :: Tree -> Int
wordCount = \case
    Word _ -> 1
    Node _ trees -> sum $ map wordCount trees

nodeCount :: Tree -> Int
nodeCount = \case
    Word _ -> 0
    Node _ trees -> 1 + (sum $ map nodeCount trees)

blahCount :: Tree -> Int
blahCount = \case
    Word _ -> 0
    Node Frag trees -> (length trees - 1) + (sum $ map blahCount trees)
    Node Blah trees -> (length trees - 1) + (sum $ map blahCount trees)
    Node _ trees -> sum $ map blahCount trees

isSingleWord :: Tree -> Bool
isSingleWord = \case
    Word _ -> True
    Node _ _ -> False

isCat :: Cat -> Tree -> Bool
isCat cat = \case
    Word _ -> False
    Node cat' _ -> cat==cat'

words :: Tree -> [String]
words = \case
    Word w -> [w]
    Node _ trees -> concat $ map words trees

text :: Tree -> String
text = concat . intersperse " " . words

allSubs :: Tree -> [Tree]
allSubs tree = tree : case tree of
    Word _ -> []
    Node _ trees -> trees >>= allSubs

allNps :: Tree -> [String]
allNps = map text . filter (isCat NP) . allSubs
