module Category exposing
    ( Category(..)
    , fromString
    , forDisplay, forId
    , all
    , sorter
    )

{-|

@docs Category
@docs fromString
@docs forDisplay, forId
@docs all
@docs sorter

-}

import Sort exposing (Sorter)


{-| -}
type Category
    = Tables
    | Inputs
    | Buttons
    | Icons
    | Widgets
    | Layout
    | Messaging
    | Atoms
    | Text
    | Pages
    | Animations


{-| -}
all : List Category
all =
    [ Animations
    , Atoms
    , Buttons
    , Icons
    , Inputs
    , Layout
    , Pages
    , Tables
    , Text
    , Widgets
    , Messaging
    ]


{-| Used for route changes
-}
fromString : String -> Result String Category
fromString string =
    case string of
        "Tables" ->
            Ok Tables

        "Inputs" ->
            Ok Inputs

        "Widgets" ->
            Ok Widgets

        "Layout" ->
            Ok Layout

        "Buttons" ->
            Ok Buttons

        "Icons" ->
            Ok Icons

        "Messaging" ->
            Ok Messaging

        "Atoms" ->
            Ok Atoms

        "Text" ->
            Ok Text

        "Pages" ->
            Ok Pages

        "Animations" ->
            Ok Animations

        _ ->
            Err "Invalid String"


{-| -}
forDisplay : Category -> String
forDisplay category =
    case category of
        Tables ->
            "Tables"

        Inputs ->
            "Inputs"

        Widgets ->
            "Widgets"

        Layout ->
            "Layout"

        Buttons ->
            "Buttons and Links"

        Icons ->
            "Icons"

        Messaging ->
            "Alerts and Messages"

        Atoms ->
            "Atoms"

        Text ->
            "Text and Fonts"

        Pages ->
            "Pages"

        Animations ->
            "Animations"


{-| -}
sorter : Sorter Category
sorter =
    Sort.by forId Sort.alphabetical


{-| -}
forId : Category -> String
forId category =
    case category of
        Tables ->
            "tables"

        Inputs ->
            "inputs"

        Widgets ->
            "widgets"

        Layout ->
            "layout"

        Buttons ->
            "buttons-and-links"

        Icons ->
            "icons"

        Messaging ->
            "alerts-and-messages"

        Atoms ->
            "atoms"

        Text ->
            "text-and-fonts"

        Pages ->
            "pages"

        Animations ->
            "animations"
