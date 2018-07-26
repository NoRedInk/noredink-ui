module Spec.Nri.Ui.DisclosureIndicator.V1 exposing (spec)

import Html exposing (div)
import Html.Attributes exposing (alt)
import Nri.Ui.AssetPath exposing (Asset(Asset))
import Nri.Ui.DisclosureIndicator.V1 as DisclosureIndicator
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.DisclosureIndicator.V1"
        [ test "has accessibility text for closed state" <|
            \() ->
                DisclosureIndicator.view assets
                    { isOpen = False
                    , label = "carrots"
                    }
                    |> (\x -> div [] [ x ])
                    |> Query.fromHtml
                    |> Query.find [ tag "img" ]
                    |> Query.has [ attribute (alt "show carrots") ]
        , test "has accessibility text for open state" <|
            \() ->
                DisclosureIndicator.view assets
                    { isOpen = True
                    , label = "carrots"
                    }
                    |> (\x -> div [] [ x ])
                    |> Query.fromHtml
                    |> Query.find [ tag "img" ]
                    |> Query.has [ attribute (alt "hide carrots") ]
        ]


assets =
    { icons_arrowDownBlue_svg = Asset "arrowDownBlue reference"
    }
