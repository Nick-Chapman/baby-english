{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseTree) where

import Data.List.Extra(lower)
import Prelude hiding(exp,fail,words,seq)
import EarleyM (Gram,Lang,fail,alts,produce,declare,getToken,many,skipWhile)
import qualified Data.Char as Char
import qualified EarleyM as EM (parseAmb,Parsing(..))

import Cat
import Tree
import Lexicon

parseTree :: Lexicon -> String -> Either String [Tree]
parseTree lexicon s =
    case EM.parseAmb (theLang lexicon) s of
        EM.Parsing{EM.outcome} -> case outcome of
            Left pe -> Left $ show pe
            Right trees -> return (rankTrees trees)

theLang :: Lexicon -> Lang Char (Gram Tree)
theLang lexicon = do
    token <- getToken
    let space = skipWhile1 $ do x <- token; if Char.isSpace x then return () else fail
    let word = many1 $ do x <- token; if Char.isSpace x then fail else return x
    (phrase',phrase) <- declare "phrase"
    produce phrase' $ phraseLang space word phrase lexicon
    let start = optLeadingTrailingSpace space phrase
    return start

phraseLang :: Gram () -> Gram String -> Gram Tree -> Lexicon -> Gram Tree
phraseLang space word phrase0 lexicon = phrase'
    where

        sen = alts [phraseCat Sen] --, phraseCat Blah]
        comp = alts [phraseCat Comp]
        vp = alts [phraseCat VP] --phraseCat Blah] -- unknownWords?
        np = alts [phraseCat NP, phraseCat Blah, unknownWord]
        nom = alts [phraseCat Nom, phraseCat Blah, unknownWord]
        pp = phraseCat PP
        advp = alts [phraseCat AdvP]

        comps = Comps {nom,np,vp,sen,comp,phrase = phrase0}

        unknownWord :: Gram Tree
        unknownWord = fmap mkWord $ do w <- word; if inLexicon lexicon (lower w) then fail else return w

        noun = unknownWord
        --verb = unknownWord

        phrase' :: Gram Tree
        phrase' = alts [unknownWord, headPhrase, adjunctivePhrase, specifiedPhrase]

        phraseCat :: Cat -> Gram Tree
        phraseCat cat = do
            tree <- phrase0
            if isCat cat tree then return tree else fail

        adjunctivePhrase = alts [
            --seq VP [verb,np],
            seq Nom [noun,pp],
            seq VP [vp,pp],
            seq VP [vp,advp],
            fail
            ]

        specifiedPhrase = alts [
            seq Sen [np,vp]
            ]

        headPhrase :: Gram Tree
        headPhrase = do
            w <- word
            case lookupInDict w of
                (sels,cat) -> do
                    let args = map (\sel -> sel comps) sels
                    seq cat (return (mkWord w) : args)

        lookupInDict :: String -> ([Comps -> Gram Tree], Cat)
        lookupInDict w =
            case lookLexicon lexicon (lower w) of
                Just (sels,cat) -> (sels,cat)
                Nothing -> ([const phrase0], Blah)

        seq :: Cat -> [Gram Tree] -> Gram Tree
        seq cat args = do
            xs <- spaceSep args
            return $ mkNode cat xs

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
