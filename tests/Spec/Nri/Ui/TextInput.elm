module Spec.Nri.Ui.TextInput exposing (spec)

import Html.Styled
import Nri.Ui.TextInput.V7 as TextInput
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.TextInput.V7"
        [ test "it uses the same DOM id that generateId produces" <|
            \() ->
                TextInput.view "myLabel"
                    [ TextInput.text identity ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "input"
                        , id (TextInput.generateId "myLabel")
                        ]
        ]
