module Spec.Nri.Ui.TextInput exposing (spec)

import Html.Attributes as Attr
import Html.Styled
import Iso8601
import Nri.Ui.TextInput.V8 as TextInput
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.TextInput.V8"
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
        , describe "datetime"
            [ test "it displays no value when no value is provided" <|
                \() ->
                    TextInput.view "myLabel"
                        [ TextInput.datetime identity, TextInput.value Nothing ]
                        |> Html.Styled.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ tag "input"
                            , attribute (Attr.value "")
                            ]
            , test "it displays the value when a value is provided" <|
                \() ->
                    TextInput.view "myLabel"
                        [ TextInput.datetime identity, TextInput.value (Iso8601.toTime "2018-01-01T00:00:00.000Z" |> Result.toMaybe) ]
                        |> Html.Styled.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ tag "input"
                            , attribute (Attr.value "2018-01-01T00:00:00.000")
                            ]
            ]
        , describe "date"
            [ test "it displays no value when no value is provided" <|
                \() ->
                    TextInput.view "myLabel"
                        [ TextInput.date identity, TextInput.value Nothing ]
                        |> Html.Styled.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ tag "input"
                            , attribute (Attr.value "")
                            ]
            , test "it displays the value when a value is provided" <|
                \() ->
                    TextInput.view "myLabel"
                        [ TextInput.date identity, TextInput.value (Iso8601.toTime "2018-01-01" |> Result.toMaybe) ]
                        |> Html.Styled.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ tag "input"
                            , attribute (Attr.value "2018-01-01")
                            ]
            ]
        ]
