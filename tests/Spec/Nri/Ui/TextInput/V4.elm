module Spec.Nri.Ui.TextInput.V4 exposing (all)

import Html.Styled
import Nri.Ui.TextInput.V4 as TextInput
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (id, tag)


all : Test
all =
    let
        basic =
            { label = "label"
            , isInError = False
            , onInput = identity
            , onBlur = Nothing
            , placeholder = "placeholder"
            , value = "value"
            , autofocus = False
            , showLabel = False
            , type_ = TextInput.text
            }
    in
    describe "Nri.Ui.TextInput.V4"
        [ test "it uses the same DOM id that generateId produces" <|
            \() ->
                TextInput.view
                    { basic | label = "myLabel" }
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "input"
                        , id (TextInput.generateId "myLabel")
                        ]
        ]
