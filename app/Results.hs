{-# LANGUAGE RecordWildCards #-}

module Results(Results(..),makeResults,layResults) where

import Numeric.Extra(intToDouble)
import Tree(Tree,layTree)
import qualified Tree
import Layout

data Results = Results
    { fullAmbCount :: Int
    , fullAmbIndex :: Double
    , selectedAmbCount :: Int
    , selectedAmbIndex :: Double
    , understanding :: Double
    , selected :: [Result]
    } deriving (Eq)

data Result = Result
    { tree :: Tree
    , wordCount :: Int
    , nodeCount :: Int
    , blahCount :: Int
    , understood :: Double
    , nps :: [String]
    } deriving (Eq)


makeResults :: [Tree] -> Results
makeResults trees = Results{..}
    where full = map makeResult trees
          selected = filter ((== minBlah) . blahCount) full
          minBlah = minimum (map blahCount full)
          fullAmbCount = length full
          fullAmbIndex = log (intToDouble fullAmbCount)
          selectedAmbCount = length selected
          selectedAmbIndex = log (intToDouble selectedAmbCount)
          understanding = maximum (map understood full)

makeResult :: Tree -> Result
makeResult tree = Result {..}
    where wordCount = Tree.wordCount tree
          nodeCount = Tree.nodeCount tree
          blahCount = Tree.blahCount tree
          understood = intToDouble (nodeCount-blahCount) / intToDouble nodeCount
          nps = Tree.allNps tree


layResults :: Results -> Layout ()
layResults Results{..} = do
    newline
    mapM_ (\(i,p) -> do lay (show i); lay ": "; layResult p; newline) (zip [1::Int ..] selected)
    lay "fullAmbCount = "; lay (show fullAmbCount); newline
    lay "fullAmbIndex = "; lay (show fullAmbIndex); newline
    lay "selectedAmbCount = "; lay (show selectedAmbCount); newline
    lay "selectedAmbIndex = "; lay (show selectedAmbIndex); newline
    lay "understanding = "; lay (show understanding); newline

layResult :: Result -> Layout ()
layResult Result{..} = scope $ do
    lay "#words = "; lay (show wordCount); newline
    lay "#nodes = "; lay (show nodeCount); newline
    lay "#blahs = "; lay (show blahCount); newline
    lay "understood = "; lay (show understood); newline
    layTree tree; newline
    lay "#nps = "; scope (mapM_ (\np -> do lay np; newline) nps)

