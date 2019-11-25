module ModuleExample exposing
    ( Category(..)
    , ModuleExample
    , ModuleMessages
    , categoryForDisplay
    , categoryForId
    , categoryFromString
    , view
    )

import Css exposing (..)
import Html.Styled as Html exposing (Html, img)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)


type alias ModuleExample msg =
    { name : String
    , content : List (Html msg)
    , category : Category
    }


{-| -}
type alias ModuleMessages moduleMsg parentMsg =
    { noOp : parentMsg
    , showItWorked : String -> parentMsg
    , wrapper : moduleMsg -> parentMsg
    }


type Category
    = Tables
    | Inputs
    | Buttons
    | Icons
    | Widgets
    | Layout
    | Messaging
    | Modals
    | Colors
    | Text
    | Pages
    | Animations


{-| Used for route changes
-}
categoryFromString : String -> Result String Category
categoryFromString string =
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

        "Modals" ->
            Ok Modals

        "Colors" ->
            Ok Colors

        "Text" ->
            Ok Text

        "Pages" ->
            Ok Pages

        "Animations" ->
            Ok Animations

        _ ->
            Err "Invalid String"


categoryForDisplay : Category -> String
categoryForDisplay category =
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

        Modals ->
            "Modals"

        Colors ->
            "Colors"

        Text ->
            "Text and Fonts"

        Pages ->
            "Error Pages"

        Animations ->
            "Animations"


categoryForId : Category -> String
categoryForId category =
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

        Modals ->
            "modals"

        Colors ->
            "colors"

        Text ->
            "text-and-fonts"

        Pages ->
            "error-pages"

        Animations ->
            "animations"


view : Bool -> ModuleExample msg -> Html msg
view showFocusLink { name, content } =
    Html.div
        [ -- this class makes the axe accessibility checking output easier to parse
          String.replace "." "-" name
            |> (++) "module-example__"
            |> Attributes.class
        ]
        [ Html.div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , justifyContent flexStart
                , padding (px 4)
                , backgroundColor glacier
                ]
            ]
            [ Html.styled Html.h2
                [ color gray20
                , fontFamilies [ qt "Source Code Pro", "Consolas", "Courier", "monospace" ]
                , fontSize (px 20)
                , marginTop zero
                , marginBottom zero
                ]
                []
                [ Html.text name ]
            , String.replace "." "-" name
                |> (++) "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                |> viewLink "view docs"
            , if showFocusLink then
                viewLink "see only this" ("#doodad/" ++ name)

              else
                Html.text ""
            ]
        , Html.div [ Attributes.css [ padding2 (px 20) zero ] ] content
        ]


viewLink : String -> String -> Html msg
viewLink text href =
    Html.a
        [ Attributes.href href
        , Attributes.css [ Css.display Css.block, marginLeft (px 20) ]
        ]
        [ Html.text text
        ]
