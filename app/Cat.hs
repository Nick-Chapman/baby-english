{-# LANGUAGE LambdaCase #-}

module Cat(Cat(..)) where

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
