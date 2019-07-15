{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseTree) where

import Data.List.Extra(lower)
import Prelude hiding(exp,fail,words,seq)
import EarleyM (Gram,Lang,fail,alts,produce,declare,getToken,many,skipWhile)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified EarleyM as EM (parseAmb,Parsing(..))

import Tree

parseTree :: String -> Either String [Tree]
parseTree s =
    case EM.parseAmb theLang s of
        EM.Parsing{EM.outcome} -> case outcome of
            Left pe -> Left $ show pe
            Right trees -> return (rankTrees trees)

theLang :: Lang Char (Gram Tree)
theLang = do
    token <- getToken
    let space = skipWhile1 $ do x <- token; if Char.isSpace x then return () else fail
    let word = many1 $ do x <- token; if Char.isSpace x then fail else return x
    (phrase',phrase) <- declare "phrase"
    produce phrase' $ phraseLang space word phrase
    let start = optLeadingTrailingSpace space phrase
    return start

phraseLang :: Gram () -> Gram String -> Gram Tree -> Gram Tree
phraseLang space word phrase0 = phrase'
    where
        dict :: [(String, ([Gram Tree], Cat))]
        dict =
            [
                ("to", ([vp], Inf)),
                ("it", ([], NP)),
                ("is", ([phrase0], VP)),
                ("saw", ([np], VP)),
                ("attack", ([np], VP)),
                ("republished", ([np], VP)),
                ("a", ([nom], NP)),
                ("an", ([nom], NP)),
                ("the", ([nom], NP)),
                ("in",  ([np], PP)),
                ("on",  ([np], PP)),
                ("of",  ([np], PP)),
                ("with",  ([np], PP)),
                ("that",  ([sen], Comp)),
                ("solely",  ([phrase0], AdvP))
            ]

        unknownWord :: Gram Tree
        unknownWord = fmap Word $ do w <- word; if w `elem` map fst dict then fail else return w

        noun = unknownWord

        nom = alts [phraseCat Nom, phraseCat Blah, unknownWord]
        np = alts [phraseCat NP, phraseCat Blah, unknownWord]
        pp = phraseCat PP
        vp = alts [phraseCat VP] -- unknownWords?
        sen = alts [phraseCat Sen, phraseCat Blah]
        advp = alts [phraseCat AdvP]

        phrase' :: Gram Tree
        phrase' = alts [unknownWord, headPhrase, adjunctivePhrase, specifiedPhrase]

        phraseCat :: Cat -> Gram Tree
        phraseCat cat = do
            tree <- phrase0
            if isCat cat tree then return tree else fail

        adjunctivePhrase = alts [
            seq Nom [noun,pp],
            seq VP [vp,pp],
            seq VP [vp,advp]
            ]

        specifiedPhrase = alts [
            seq Sen [np,vp]
            ]

        headPhrase :: Gram Tree
        headPhrase = do
            w <- word
            case lookupInDict w of
                (args,cat) ->
                    seq cat (return (Word w) : args)

        lookupInDict :: String -> ([Gram Tree], Cat)
        lookupInDict w =
            case List.lookup (lower w) dict of
                Just (args,cat) -> (args,cat)
                Nothing -> ([phrase0], Blah)

        seq :: Cat -> [Gram Tree] -> Gram Tree
        seq cat args = do
            xs <- spaceSep args
            return $ Node cat xs

        spaceSep :: [Gram a] -> Gram [a]
        spaceSep = \case
            [] -> return []
            [g] -> do x <- g; return [x]
            g:gs ->  do x <- g; space; xs <- spaceSep gs; return $ x:xs


optLeadingTrailingSpace :: Gram () -> Gram a -> Gram a
optLeadingTrailingSpace space gram = do
    opt space;
    x <- gram
    opt space;
    return x

opt :: Gram () -> Gram ()
opt p = alts [p, return ()]

many1 :: Gram a -> Gram [a]
many1 p = do x <- p; xs <- many p; return $ x:xs

skipWhile1 :: Gram () -> Gram ()
skipWhile1 p = do p; skipWhile p
