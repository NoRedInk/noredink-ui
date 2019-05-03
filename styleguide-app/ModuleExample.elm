module ModuleExample exposing
    ( Category(..)
    , ModuleExample
    , ModuleMessages
    , categoryForDisplay
    , categoryFromString
    , view
    )

import Css exposing (..)
import Html.Styled as Html exposing (Html, img)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)


type alias ModuleExample msg =
    { filename : String
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


view : Bool -> ModuleExample msg -> Html msg
view showFocusLink { filename, content } =
    Html.div
        []
        [ Html.styled Html.div
            [ display block
            , backgroundColor glacier
            , padding (px 20)
            , marginTop (px 20)
            ]
            []
            [ Html.styled Html.h2
                [ color gray20
                , fontFamilies [ qt "Source Code Pro", "Consolas", "Courier", "monospace" ]
                , fontSize (px 20)
                ]
                []
                [ Html.text filename
                , Html.text " "
                , if showFocusLink then
                    Html.a
                        [ Attributes.href <| "#doodad/" ++ filename ]
                        [ Html.text "(see only this)" ]

                  else
                    Html.text ""
                ]
            ]
        , Html.styled Html.div
            [ padding2 (px 20) zero ]
            []
            content
        ]
