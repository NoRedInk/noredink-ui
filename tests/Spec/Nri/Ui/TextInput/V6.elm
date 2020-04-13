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
                TextInput.view "myLabel"
                    (TextInput.text identity)
                    [ TextInput.hiddenLabel
                    , TextInput.placeholder "placeholder"
                    ]
                    "value"
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "input"
                        , id (TextInput.generateId "myLabel")
                        ]
        ]
