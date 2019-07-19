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
            Right trees -> return trees

theLang :: Lexicon -> Lang Char (Gram Tree)
theLang lexicon = do
    token <- getToken
    let sepChar x = Char.isSpace x || x `elem` ".,"
    let space = skipWhile1 $ do x <- token; if sepChar x then return () else fail
    let word = many1 $ do x <- token; if sepChar x then fail else return x
    (phrase',phrase0) <- declare "phrase"
    let (phrase,blah) = phraseLang space word phrase0 lexicon
    produce phrase' phrase

    let seq = separated space

    let chunk = phrase
    let chunks = alts [chunk, seq Frag [chunk, chunks]]

    let thing = alts [
            blah,
            seq Frag [blah,chunks],
            seq Frag [chunks,blah],
            seq Frag [blah,chunks,blah],
            chunks
            ]

    return $ optLeadingTrailingSpace space thing


phraseLang :: Gram () -> Gram String -> Gram Tree -> Lexicon -> (Gram Tree, Gram Tree)
phraseLang space word phrase lexicon = (phrase',blah)
    where
        phraseCat :: Cat -> Gram Tree
        phraseCat cat = do
            tree <- phrase
            if isCat cat tree then return tree else fail

        unknown cat = fmap (mkWord cat) $ do
            w <- word;
            if inLexicon lexicon (lower w) then fail else return w

        blah = alts [unknown Blah, seq Blah [unknown Blah,blah]]

        nommy = alts [unknown Blah, seq Nom [unknown Blah,nommy]]

        np = alts [nommy,phraseCat NP]
        pp = phraseCat PP
        vp = phraseCat VP -- adding blah here (or even unknown) causes to much full-AMB..
        advp = phraseCat AdvP

        comps = Comps {nom,np,pp,advp,vp,sen,comp,phrase}
            where
                nom = alts [nommy,phraseCat Nom]
                sen = phraseCat Sen
                comp = phraseCat Comp

        lexicalPhrase = do
            w <- word
            let frames = lookLexicon lexicon (lower w)
            alts $ flip map frames $ \(cat,(pos,sels)) -> do
                let args = map (\sel -> sel comps) sels
                seq cat (return (mkWord pos w) : args)

        adjunctivePhrase = alts [
            seq Nom [unknown Noun,pp],

            seq VP [unknown Verb,pp], --50,74
            --seq VP [vp,pp],

            seq Sen [np,vp],
            seq VP [vp,advp],
            fail
            ]

        conjunction = do
            left <- phrase
            let cat = catOf left
            seq cat [return left, phraseCat Conj, phraseCat cat]

        phrase' = alts [lexicalPhrase,adjunctivePhrase,conjunction]

        seq = separated space


separated :: Gram () -> Cat -> [Gram Tree] -> Gram Tree
separated sep cat args = do
    xs <- spaceSep args
    return $ mkNode cat xs
    where
        spaceSep :: [Gram a] -> Gram [a]
        spaceSep = \case
            [] -> return []
            [g] -> do x <- g; return [x]
            g:gs ->  do x <- g; sep; xs <- spaceSep gs; return $ x:xs


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
