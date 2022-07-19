module Section exposing (Section(..), headerId, initiallyExpanded, name, sorter)

import Nri.Ui.Util exposing (dashify)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


initiallyExpanded : Set Section
initiallyExpanded =
    Set.fromList sorter
        [ Settings
        , Example
        ]


type Section
    = KeyboardSupport
    | Settings
    | Example
    | ExampleCode


headerId : Section -> String
headerId section =
    dashify (name section)


name : Section -> String
name section =
    case section of
        KeyboardSupport ->
            "Keyboard Support"

        Settings ->
            "Settings"

        Example ->
            "Example"

        ExampleCode ->
            "Code"


sorter : Sorter Section
sorter =
    Sort.by name Sort.alphabetical
