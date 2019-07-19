{-# LANGUAGE LambdaCase #-}

module Cat(Cat(..)) where

data Cat =
    Frag | Blah
    | Comp | Sen | Aux | Inf | VP | AdvP | PP | NP | Nom | Conj
    | Noun | Verb | Det | Pro | Prep | Cop | Complementizer
    deriving (Eq)

instance Show Cat where
    show = \case
        Frag -> "*"
        Blah -> "?"

        Comp -> "Comp"
        Sen -> "S"
        Aux -> "Aux"
        Inf -> "Inf"
        VP -> "VP"
        AdvP -> "AdvP"
        PP -> "PP"
        NP -> "NP"
        Nom -> "NOM"
        Conj -> "&"

        Noun -> "N"
        Verb -> "V"
        Det -> "D"
        Pro -> "Pro"
        Prep -> "Prep"
        Cop -> "Cop"
        Complementizer -> "Complementizer"
