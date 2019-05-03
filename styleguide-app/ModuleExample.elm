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
    = Layout
    | Inputs
    | Buttons
    | Icons
    | Behaviors
    | Messaging
    | Modals
    | Colors
    | Text
    | DynamicSymbols
    | Pages
    | QuestionTypes


{-| Used for route changes
-}
categoryFromString : String -> Result String Category
categoryFromString string =
    case string of
        "Layout" ->
            Ok Layout

        "Inputs" ->
            Ok Inputs

        "Behaviors" ->
            Ok Behaviors

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

        "DynamicSymbols" ->
            Ok DynamicSymbols

        "Pages" ->
            Ok Pages

        "QuestionTypes" ->
            Ok QuestionTypes

        _ ->
            Err "Invalid String"


categoryForDisplay : Category -> String
categoryForDisplay category =
    case category of
        Layout ->
            "Layout"

        Inputs ->
            "Inputs"

        Behaviors ->
            "Behaviors"

        Buttons ->
            "Buttons"

        Icons ->
            "Icons"

        Messaging ->
            "Messaging"

        Modals ->
            "Modals"

        Colors ->
            "Colors"

        Text ->
            "Text"

        DynamicSymbols ->
            "Dynamic Symbols"

        Pages ->
            "Pages"

        QuestionTypes ->
            "Question types / Quiz UI"


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
