{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.List.Extra(lower)
import Prelude hiding(exp,fail,words,seq)
import EarleyM (Gram,Lang,fail,alts,produce,declare,getToken,many,skipWhile)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified EarleyM as EM (parseAmb,Parsing(..))

main :: IO ()
main = do
    let line = sample
    putStrLn $ "input: " <> line
    case parseDef line of
        Left s -> do
            putStrLn $ "parse error: " <> s <> " : " <> line
            return ()
        Right trees -> do
            let minBlah = minimum (map blahCount trees)
            let selected = filter ((== minBlah) . blahCount) trees
            putStrLn $ "parse: amb=" <> show (length trees) <> ", #selected=" <> show (length selected)
            mapM_ (\(i,t) -> putStrLn $ show i <> ": " <> see t) $ zip [1::Int ..] selected
    where
        --see t = "[?=" <> show (blahCount t) <> "] " <> show t
        see t = show t

parseDef :: String -> Either String [Tree]
parseDef s =
    case EM.parseAmb theLang s of
        EM.Parsing{EM.outcome} -> case outcome of
            Left pe -> Left $ show pe
            Right trees -> return (rankTrees trees)

rankTrees :: [Tree] -> [Tree]
rankTrees = List.sortOn blahCount

sample :: String
--sample = "The man on the hill saw the boy with a telescope."
sample = "A brief outline of the events is that the editor of a major medical journal republished a previously published paper solely in order to attack it in an editorial."

data Tree = Word String | Node Cat [Tree]

blahCount :: Tree -> Int
blahCount = \case
    Word _ -> 0
    Node Blah trees -> 1 + (sum $ map blahCount trees)
    Node _ trees -> sum $ map blahCount trees

instance Show Tree where
    show = \case
        Word s -> s
        Node cat trees -> "(" <> List.intercalate " " (show cat : map show trees) <> ")"

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

        nom = alts [phraseCat True Nom, phraseCat False Blah]
        np = alts [phraseCat True NP, phraseCat False Blah]
        pp = phraseCat False PP
        vp = alts [phraseCat False VP]
        sen = alts [phraseCat False Sen, phraseCat False Blah]
        advp = alts [phraseCat False AdvP]

        phrase' :: Gram Tree
        phrase' = alts [unknownWord, headPhrase, adjunctivePhrase, specifiedPhrase]

        phraseCat :: Bool -> Cat -> Gram Tree
        phraseCat allowSingleWord cat = do
            tree <- phrase0
            if isCat tree then return tree else fail
                where
                    isCat :: Tree -> Bool
                    isCat = \case
                        Word _ -> allowSingleWord
                        Node cat' _ -> cat==cat'

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

theLang :: Lang Char (Gram Tree)
theLang = do
    token <- getToken
    let space = skipWhile1 $ do x <- token; if Char.isSpace x then return () else fail
    let word = many1 $ do x <- token; if Char.isSpace x then fail else return x
    (phrase',phrase) <- declare "phrase"
    produce phrase' $ phraseLang space word phrase
    let start = optLeadingTrailingSpace space phrase
    return start

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
