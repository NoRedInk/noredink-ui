module Spec.Nri.Ui.Select exposing (spec)

import Expect exposing (Expectation)
import Html
import Html.Attributes as Attr
import Html.Styled
import Nri.Ui.Select.V3
import Nri.Ui.Select.V5
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "view"
        [ describe "V1" (viewSuite Nri.Ui.Select.V1.view)
        , describe "V2" (viewSuite Nri.Ui.Select.V2.view)
        , describe "V3" (viewSuite (Nri.Ui.Select.V3.view >> Html.Styled.toUnstyled))
        , describe "V5"
            (viewSuite
                (\config ->
                    { choices = config.choices, current = config.current, id = Nothing, valueToString = identity }
                        |> Nri.Ui.Select.V5.view
                        |> Html.Styled.toUnstyled
                )
            )
        ]


viewSuite :
    ({ choices : List { label : String, value : String }, current : String } -> Html.Html msg)
    -> List Test
viewSuite view =
    [ test "shows all options" <|
        \() ->
            viewTest
                view
                "Tacos"
                [ "Tacos", "Burritos", "Enchiladas" ]
                |> Query.find [ tag "select" ]
                |> Query.findAll [ tag "option" ]
                |> Query.count (Expect.equal 3)
    , test "selects the current option" <|
        \() ->
            viewTest
                view
                "Burritos"
                [ "Tacos", "Burritos", "Enchiladas" ]
                |> Query.find
                    [ tag "option"
                    , attribute <| Attr.selected True
                    ]
                |> Query.has [ text "Burritos" ]
    ]


viewTest :
    ({ choices : List { label : a, value : a }, current : b } -> Html.Html msg)
    -> b
    -> List a
    -> Query.Single msg
viewTest view selected items =
    Query.fromHtml
        (Html.div []
            [ view
                { choices = List.map (\x -> { label = x, value = x }) items
                , current = selected
                }
            ]
        )
