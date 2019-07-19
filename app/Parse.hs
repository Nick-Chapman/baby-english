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
    let sepChar x = Char.isSpace x || x `elem` ".,"
    let space = skipWhile1 $ do x <- token; if sepChar x then return () else fail
    let word = many1 $ do x <- token; if sepChar x then fail else return x
    (phrase',phrase) <- declare "phrase"
    produce phrase' $ phraseLang space word phrase lexicon
    let start = optLeadingTrailingSpace space phrase
    return start

phraseLang :: Gram () -> Gram String -> Gram Tree -> Lexicon -> Gram Tree
phraseLang space word phrase lexicon = phrase'
    where

        phraseCat :: Cat -> Gram Tree
        phraseCat cat = do
            tree <- phrase
            if isCat cat tree then return tree else fail

        unknownWord = fmap mkWord $ do
            w <- word;
            if inLexicon lexicon (lower w) then fail else return w

        blah = alts [unknownWord, blah2]
        blah2 = seq Blah [unknownWord,blah]

        nom = alts [blah,phraseCat Nom]
        np = alts [blah,phraseCat NP]
        pp = phraseCat PP
        vp = phraseCat VP -- adding blah here causes massive full-AMB
        sen = alts [] --phraseCat Sen
        comp = alts [] --phraseCat Comp

        comps = Comps {nom,np,pp,vp,sen,comp,phrase}

        lexicalPhrase = do
            w <- word
            let frames = lookLexicon lexicon (lower w)
            alts $ flip map frames $ \(sels,cat) -> do
                let args = map (\sel -> sel comps) sels
                seq cat (return (mkWord w) : args)

        adjunctivePhrase = alts [
            seq Nom [unknownWord,pp],
            seq VP [unknownWord,pp],
            seq Sen [np,vp],
            fail
            ]

        conjunction = do
            left <- phrase
            case catOf left of
                Nothing -> fail -- TODO: conj with single word on left
                Just cat -> seq cat [return left, phraseCat Conj, phraseCat cat]

        chunk = alts [blah,lexicalPhrase,adjunctivePhrase,conjunction]

        chunks = alts [chunk, seq Frag [chunk, chunks]]

        phrase' = chunks

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
