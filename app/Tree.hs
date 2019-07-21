{-# LANGUAGE LambdaCase #-}

module Tree (
    Tree, mkWord, mkNode,
    layTree,
    catOf,

    fragCount,
    fullCount, blahCount,

    isSingleWord,
    isCat,
    words,

    --allNps,
    ) where

import qualified Data.List as List
import Layout
import Cat
import Prelude hiding(words,pred)
--import Data.List (intersperse)

data Tree = Word Cat String | Node Cat [Tree] deriving (Eq)

catOf :: Tree -> Cat
catOf = \case
    Word pos _ -> pos
    Node cat _ -> cat

mkWord :: Cat -> String -> Tree
mkWord = Word

mkNode :: Cat -> [Tree] -> Tree
mkNode cat = \case
    [] -> error "mkNode,0"
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


fragCount :: Tree -> Int
fragCount = \case
    Node (FragN n) _ -> n
    Node _ _ -> 1
    Word _ _ -> 1


{-  NAH...
-- For size, we count each word as 1, and each node as its number of children
-- so we regard additional structure as having size
-- here are two ways in which 3 leafs may be composed as a tree, and their sizes:

-- (N3 x y z) -- size 6 -- 3 + 1 + 1 + 1
-- (N2 (N2 x y) z) -- size 7 -- 2 + 2 + 1 + 1 + 1
-}

countP :: (Cat -> Bool) -> Tree -> Int
countP pred = \case
    Word cat _ -> if pred cat then 1 else 0
    --Node cat trees -> (if pred cat then length trees else 0) + (sum $ map (countP pred) trees)
    Node cat trees -> (if pred cat then length trees - 1 else 0) + (sum $ map (countP pred) trees)

fullCount :: Tree -> Int
fullCount = countP (const True)

blahCount :: Tree -> Int
blahCount = countP Cat.isBlah


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

{-
text :: Tree -> String
text = concat . intersperse " " . words

allSubs :: Tree -> [Tree]
allSubs tree = tree : case tree of
    Word _ _ -> []
    Node _ trees -> trees >>= allSubs

allNps :: Tree -> [String]
allNps = map text . filter (isCat NP) . allSubs
-}
