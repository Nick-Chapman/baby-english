{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse (parseTree) where

import Data.List.Extra(lower)
import Prelude hiding(exp,fail,words,seq,pred)
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
    let chunk = phrase
    let thing = do xs <- frags space blah chunk; return $ fraggy xs
    return $ optLeadingTrailingSpace space thing

fraggy :: [Tree] -> Tree
fraggy = \case [] -> error "fraggy,[]"; [t] -> t; ts -> mkNode (FragN (length ts)) ts

frags :: Gram () -> Gram a -> Gram a -> Gram [a]
frags space blah chunk = do
    let chunks = alts [
            do x <- chunk; return [x],
            do x <- chunk; space; b <- blah; return [x,b],
            --do x <- chunk; space; b <- blah; space; xs <- chunks; return $ x:b:xs,
            do x <- chunk; space; xs <- chunks; return $ x:xs
            ]
    alts [
        do b <- blah; return [b],
        chunks,
        do b <- blah; space; xs <- chunks; return $ b:xs
        ]

-- TODO: inline into theLang above
phraseLang :: Gram () -> Gram String -> Gram Tree -> Lexicon -> (Gram Tree, Gram Tree)
phraseLang space word phrase lexicon = (phrase',blah)
    where

        blah = multiword (anyword Blah) Blah

        pickWord pred cat = fmap (mkWord cat) $ do
            w <- word;
            if pred w then return w else fail

        phraseCat :: Cat -> Gram Tree
        phraseCat cat = do
            tree <- phrase
            if isCat cat tree then return tree else fail

        multiword w cat = alts [w, seq cat [w, multiword w cat]]

        anyword = pickWord (const True)
        unknown = pickWord (not . inLexicon lexicon . lower)

        nommy = multiword (unknown BlahNoun) Nom
        nom = alts [phraseCat Nom, nommy]
        np = phraseCat NP
        pp = phraseCat PP
        vp = phraseCat VP
        advp = phraseCat AdvP
        comp = phraseCat Comp
        sen = phraseCat Sen

        comps = Comps {nom,np,pp,advp,vp,sen,comp,phrase}

        lexicalPhrase = do
            w <- word
            let frames = lookLexicon lexicon (lower w)
            alts $ flip map frames $ \(cat,(pos,sels)) -> do
                let args = map (\sel -> sel comps) sels
                seq cat (return (mkWord pos w) : args)

        adjunctivePhrase = alts [

            seq Sen [np,vp], -- sentence construction
            seq Nom [nom,pp], -- PP attachment to NOM
            --seq NP [nom], -- NP without D -- Too general. Invents structure / Not lexical
            fail
            ]

        vp' = alts [unknown BlahVerb, vp]

        verbPhrase = alts [
            seq VP [vp',np],
            seq VP [vp',pp], -- PP attachment to VP (or VP taking PP)
            seq VP [vp',advp]
            ]

        conjunction x = do
            left <- x
            let cat = catOf left
            seq cat [return left, phraseCat Conj, phraseCat cat]

        phrase' = alts [lexicalPhrase,
                        verbPhrase,
                        adjunctivePhrase,
                        conjunction phrase
                       ]

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
