module Spec.Nri.Ui.Select exposing (spec)

import Expect exposing (Expectation)
import Html
import Html.Attributes as Attr
import Html.Styled
import Nri.Ui.Select.V5
import Nri.Ui.Select.V6
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "view"
        [ describe "V5"
            (viewSuiteV5
                (\config ->
                    { choices = config.choices
                    , current = config.current
                    , id = Nothing
                    , valueToString = identity
                    }
                        |> Nri.Ui.Select.V5.view
                        |> Html.Styled.toUnstyled
                )
            )
        , describe "V6"
            (viewSuiteV6
                (\config ->
                    { choices = config.choices
                    , current = config.current
                    , id = Nothing
                    , valueToString = identity
                    , defaultDisplayText = config.defaultDisplayText
                    }
                        |> Nri.Ui.Select.V6.view
                        |> Html.Styled.toUnstyled
                )
            )
        ]


viewSuiteV5 :
    ({ choices : List { label : String, value : String }, current : String } -> Html.Html msg)
    -> List Test
viewSuiteV5 view =
    [ test "shows all options" <|
        \() ->
            viewTestV5
                view
                "Tacos"
                [ "Tacos", "Burritos", "Enchiladas" ]
                |> Query.find [ tag "select" ]
                |> Query.findAll [ tag "option" ]
                |> Query.count (Expect.equal 3)
    , test "selects the current option" <|
        \() ->
            viewTestV5
                view
                "Burritos"
                [ "Tacos", "Burritos", "Enchiladas" ]
                |> Query.find
                    [ tag "option"
                    , attribute <| Attr.selected True
                    ]
                |> Query.has [ text "Burritos" ]
    ]


viewTestV5 :
    ({ choices : List { label : a, value : a }, current : b } -> Html.Html msg)
    -> b
    -> List a
    -> Query.Single msg
viewTestV5 view selected items =
    Query.fromHtml
        (Html.div []
            [ view
                { choices = List.map (\x -> { label = x, value = x }) items
                , current = selected
                }
            ]
        )


viewSuiteV6 :
    ({ choices : List { label : String, value : String }, current : Maybe String, defaultDisplayText : Maybe String } -> Html.Html msg)
    -> List Test
viewSuiteV6 view =
    [ describe "without a default option"
        [ test "shows all options" <|
            \() ->
                viewTestV6
                    view
                    Nothing
                    Nothing
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find [ tag "select" ]
                    |> Query.findAll [ tag "option" ]
                    |> Query.count (Expect.equal 3)
        , test "selects the first option if nothing is selected and there's no default" <|
            \() ->
                viewTestV6
                    view
                    Nothing
                    Nothing
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find
                        [ tag "option"
                        , attribute <| Attr.selected True
                        ]
                    |> Query.has [ text "Tacos" ]
        , test "selects the current option" <|
            \() ->
                viewTestV6
                    view
                    Nothing
                    (Just "Burritos")
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find
                        [ tag "option"
                        , attribute <| Attr.selected True
                        ]
                    |> Query.has [ text "Burritos" ]
        ]
    , describe "with a default option"
        [ test "shows all options" <|
            \() ->
                viewTestV6
                    view
                    (Just "Tasty tortilla'd foods")
                    Nothing
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find [ tag "select" ]
                    |> Query.findAll [ tag "option" ]
                    |> Query.count (Expect.equal 4)
        , test "selects the disabled default option if nothing is currently selected" <|
            \() ->
                viewTestV6
                    view
                    (Just "Tasty tortilla'd foods")
                    Nothing
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find
                        [ tag "option"
                        , attribute <| Attr.selected True
                        , attribute <| Attr.disabled True
                        ]
                    |> Query.has [ text "Tasty tortilla'd foods" ]
        , test "selects the current option" <|
            \() ->
                viewTestV6
                    view
                    (Just "Tasty tortilla'd foods")
                    (Just "Burritos")
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find
                        [ tag "option"
                        , attribute <| Attr.selected True
                        ]
                    |> Query.has [ text "Burritos" ]
        ]
    ]


viewTestV6 :
    ({ choices : List { label : a, value : a }, current : Maybe b, defaultDisplayText : Maybe String } -> Html.Html msg)
    -> Maybe String
    -> Maybe b
    -> List a
    -> Query.Single msg
viewTestV6 view defaultDisplayText selected items =
    Query.fromHtml
        (Html.div []
            [ view
                { choices = List.map (\x -> { label = x, value = x }) items
                , current = selected
                , defaultDisplayText = defaultDisplayText
                }
            ]
        )
