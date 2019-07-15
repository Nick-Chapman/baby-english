
module Main (main) where

import Tree
import Parse

main :: IO ()
main = do
    let line = sample
    putStrLn $ "input: " <> line
    case parseTree line of
        Left s -> do
            putStrLn $ "parse error: " <> s <> " : " <> line
            return ()
        Right trees -> do
            let minBlah = minimum (map blahCount trees)
            let selected = filter ((== minBlah) . blahCount) trees
            putStrLn $ "parse: amb=" <> show (length trees) <> ", #selected=" <> show (length selected)
            mapM_ (\(i,t) -> putStrLn $ show i <> ":\n" <> show t) $ zip [1::Int ..] selected

sample :: String
--sample = "The man on the hill saw the boy with a telescope."
sample = "A brief outline of the events is that the editor of a major medical journal republished a previously published paper solely in order to attack it in an editorial."
