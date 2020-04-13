module Spec.Nri.Ui.TextInput.V6 exposing (all)

import Html.Styled
import Nri.Ui.TextInput.V6 as TextInput
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (id, tag)


all : Test
all =
    describe "Nri.Ui.TextInput.V6"
        [ test "it uses the same DOM id that generateId produces" <|
            \() ->
                TextInput.view
                    { label = "myLabel"
                    , isInError = False
                    , onInput = identity
                    , onBlur = Nothing
                    , placeholder = "placeholder"
                    , value = "value"
                    , autofocus = False
                    , showLabel = False
                    , type_ = TextInput.text
                    }
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "input"
                        , id (TextInput.generateId "myLabel")
                        ]
        ]
