{-# LANGUAGE LambdaCase #-}

module Cat(Cat(..),isBlah) where

data Cat =
    -- TODO: MOve the Frag stuff to a sep type
    FragN Int

    | Blah
    | BlahNoun
    | BlahVerb

    -- Nodes
    | Comp
    | Sen | Aux | Inf
    | VP | PP | NP | Nom | AdvP

    -- Words
    | Noun
    | Verb
    | Det | Pro | Prep | Cop | Complementizer
    | Conj
    | Proper

    deriving (Eq)


isBlah :: Cat -> Bool
isBlah = \case
    Blah -> True
    BlahNoun -> True
    BlahVerb -> True
    _ -> False

instance Show Cat where
    show = \case
        FragN n -> "*" <> show n

        Blah -> "?"
        BlahNoun -> "?N"
        BlahVerb -> "?V"

        Comp -> "Comp"
        Sen -> "S"
        Aux -> "Aux"
        Inf -> "Inf"
        VP -> "VP"
        PP -> "PP"
        NP -> "NP"
        Nom -> "Nom"
        AdvP -> "AdvP"

        Noun -> "N"
        Verb -> "V"
        Det -> "D"
        Pro -> "Pro"
        Prep -> "Prep"
        Cop -> "Cop"
        Complementizer -> "Complementizer"

        Conj -> "&"
        Proper -> "@"
