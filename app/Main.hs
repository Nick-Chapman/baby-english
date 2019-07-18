
module Main (main) where

import Cat
import Lexicon
import Parse(parseTree)
import Results

main :: IO ()
main = do
    let line = sample
    putStrLn $ "input: " <> line
    case parseTree the_lexicon line of
        Left s -> do
            putStrLn $ "parse error: " <> s <> " : " <> line
            return ()
        Right trees -> do
            let results = makeResults trees
            print results

sample :: String
--sample = "The man on the hill saw the boy."
--sample = "The man on the hill saw the fat boy with a telescope."
sample = "A brief outline of the events is that the editor of a major medical journal republished a previously published paper solely in order to attack it in an editorial."


the_lexicon :: Lexicon
the_lexicon = everything
    where
        everything = combine [dets,preps,hacks,functionWords,tverbs]

        dets = combine $ map det ["the","a","an"]
        preps = combine $ map prep ["in","on","of","with"]
        tverbs = combine $ map tverb ["saw","attack","republished"]

        det = entry NP [nom]
        prep = entry PP [np]
        tverb = entry VP [np]

        functionWords = combine [is,it,that,to]

        to = entry Inf [vp] "to"
        that = entry Comp [sen] "that"
        it = entry NP [] "it"
        is = entry VP [phrase] "is"

        hacks = combine [solely]
            where
                solely = entry AdvP [phrase] "solely"
