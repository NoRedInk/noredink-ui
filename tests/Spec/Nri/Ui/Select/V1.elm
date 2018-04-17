module Spec.Nri.Ui.Select.V1 exposing (spec)

import Expect exposing (Expectation)
import Html
import Html.Attributes as Attr
import Nri.Ui.Select.V1
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


view : String -> List String -> Query.Single String
view selected items =
    Nri.Ui.Select.V1.view
        { choices = items |> List.map (\x -> { label = x, value = x })
        , current = selected
        }
        |> List.singleton
        |> -- needed so that Query can find the root node
           Html.div []
        |> Query.fromHtml


spec : Test
spec =
    describe "view"
        [ test "shows all options" <|
            \() ->
                view "Tacos"
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find [ tag "select" ]
                    |> Query.findAll [ tag "option" ]
                    |> Query.count (Expect.equal 3)
        , test "selects the current option" <|
            \() ->
                view "Burritos"
                    [ "Tacos", "Burritos", "Enchiladas" ]
                    |> Query.find
                        [ tag "option"
                        , attribute <| Attr.selected True
                        ]
                    |> Query.has [ text "Burritos" ]
        ]
