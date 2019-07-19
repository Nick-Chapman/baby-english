{-# LANGUAGE LambdaCase #-}

module Cat(Cat(..)) where

data Cat = Frag | Blah | Sen | Inf | VP | PP | NP | Nom | Conj  deriving (Eq)

instance Show Cat where
    show = \case
        Frag -> "*"
        Blah -> "?"
        Sen -> "S"
        Inf -> "Inf"
        VP -> "VP"
        PP -> "PP"
        NP -> "NP"
        Nom -> "NOM"
        Conj -> "&"
