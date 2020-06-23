module AtomicDesignType exposing
    ( AtomicDesignType(..)
    , all, sorter, toString
    )

{-|

@docs AtomicDesignType
@docs all, sorter, toString

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
all : List AtomicDesignType
all =
    [ Atom
    , Molecule
    , Organism
    , Template
    , Page
    ]


{-| -}
sorter : Sorter AtomicDesignType
sorter =
    Sort.by
        (\v ->
            case v of
                Atom ->
                    0

                Molecule ->
                    1

                Organism ->
                    2

                Template ->
                    3

                Page ->
                    4
        )
        Sort.increasing


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
