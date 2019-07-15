{-# LANGUAGE LambdaCase #-}

module Tree (
    Tree(..),
    Cat(..),
    rankTrees,
    blahCount,
    isSingleWord,
    isCat,
    ) where

import qualified Data.List as List
import Layout(Layout,lay)
import qualified Layout

data Cat = Blah | Sen | Comp | Inf | VP | PP | NP | Nom | AdvP deriving (Eq)

instance Show Cat where
    show = \case
        Blah -> "?"
        Sen -> "Sen"
        Comp -> "Comp"
        Inf -> "Inf"
        VP -> "VP"
        PP -> "PP"
        NP -> "NP"
        Nom -> "NOM"
        AdvP -> "AdvP"

data Tree = Word String | Node Cat [Tree]

instance Show Tree where show = _show2

_show1 :: Tree -> String
_show1 = \case
    Word s -> s
    Node cat trees -> "(" <> List.intercalate " " (show cat : map _show1 trees) <> ")"

_show2 :: Tree -> String
_show2 = Layout.toString . layout
    where
        layout :: Tree -> Layout ()
        layout = \case
            Word s -> lay s
            Node cat trees -> do
                lay "("
                lay (show cat);
                lay " "
                Layout.mark $ sequence_ $ List.intersperse Layout.newline (map layout trees)
                lay ")"

rankTrees :: [Tree] -> [Tree]
rankTrees = List.sortOn blahCount

blahCount :: Tree -> Int
blahCount = \case
    Word _ -> 0
    Node Blah trees -> 1 + (sum $ map blahCount trees)
    Node _ trees -> sum $ map blahCount trees

isSingleWord :: Tree -> Bool
isSingleWord = \case
    Word _ -> True
    Node _ _ -> False

isCat :: Cat -> Tree -> Bool
isCat cat = \case
    Word _ -> False
    Node cat' _ -> cat==cat'
