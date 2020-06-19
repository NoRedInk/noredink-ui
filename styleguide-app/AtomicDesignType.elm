module AtomicDesignType exposing
    ( AtomicDesignType(..)
    , sorter
    , toString
    )

{-|

@docs AtomicDesignType
@docs sorter
@docs toString

-}

import Sort exposing (Sorter)


{-| -}
type AtomicDesignType
    = Atom
    | Molecule
    | Organism
    | Template
    | Page


{-| -}
sorter : Sorter AtomicDesignType
sorter =
    Sort.by toString Sort.alphabetical


{-| -}
toString : AtomicDesignType -> String
toString atomicDesignType =
    case atomicDesignType of
        Atom ->
            "Atom"

        Molecule ->
            "Molecule"

        Organism ->
            "Organism"

        Template ->
            "Template"

        Page ->
            "Page"
