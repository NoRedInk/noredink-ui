module Category exposing
    ( Category(..)
    , fromString
    , forDisplay, forId, forRoute
    , all
    , sorter
    )

{-|

@docs Category
@docs fromString
@docs forDisplay, forId, forRoute
@docs all
@docs sorter

-}

import Sort exposing (Sorter)


{-| -}
type Category
    = Inputs
    | Buttons
    | Icons
    | Layout
    | Messaging
    | Atoms
    | Text
    | Interactions
    | Animations


{-| -}
all : List Category
all =
    [ Animations
    , Atoms
    , Buttons
    , Icons
    , Inputs
    , Interactions
    , Layout
    , Text
    , Messaging
    ]


{-| Used for route changes
-}
fromString : String -> Result String Category
fromString string =
    case string of
        "Inputs" ->
            Ok Inputs

        "Layout" ->
            Ok Layout

        "Buttons" ->
            Ok Buttons

        "Icons" ->
            Ok Icons

        "Interactions" ->
            Ok Interactions

        "Messaging" ->
            Ok Messaging

        "Atoms" ->
            Ok Atoms

        "Text" ->
            Ok Text

        "Animations" ->
            Ok Animations

        _ ->
            Err "Invalid String"


{-| -}
forDisplay : Category -> String
forDisplay category =
    case category of
        Inputs ->
            "Inputs"

        Layout ->
            "Layout"

        Buttons ->
            "Buttons and Links"

        Icons ->
            "Icons"

        Interactions ->
            "Interactions"

        Messaging ->
            "Alerts and Messages"

        Atoms ->
            "Atoms"

        Text ->
            "Text and Fonts"

        Animations ->
            "Animations"


{-| -}
forRoute : Category -> String
forRoute =
    Debug.toString


{-| -}
sorter : Sorter Category
sorter =
    Sort.by forId Sort.alphabetical


{-| -}
forId : Category -> String
forId category =
    case category of
        Inputs ->
            "inputs"

        Layout ->
            "layout"

        Buttons ->
            "buttons-and-links"

        Icons ->
            "icons"

        Interactions ->
            "interactions"

        Messaging ->
            "alerts-and-messages"

        Atoms ->
            "atoms"

        Text ->
            "text-and-fonts"

        Animations ->
            "animations"
