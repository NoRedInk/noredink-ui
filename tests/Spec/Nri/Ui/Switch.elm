module Spec.Nri.Ui.Switch exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import Nri.Ui.Switch.V3 as Switch
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Switch.V2"
        [ describe "'switch' role" hasCorrectRole
        , describe "has aria-disabled=true when disabled" hasCorrectAriaDisabled
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role 'switch'" <|
        \() ->
            Switch.view { id = "switch", label = "Switch" }
                []
                |> List.singleton
                |> div []
                |> toUnstyled
                |> Query.fromHtml
                |> Query.find [ Selector.id "switch" ]
                |> Query.has [ Selector.attribute Role.switch ]
    ]


hasCorrectAriaDisabled : List Test
hasCorrectAriaDisabled =
    [ test "has 'aria-disabled=true' when disabled" <|
        \() ->
            Switch.view { id = "switch", label = "Switch" }
                [ Switch.disabled True ]
                |> List.singleton
                |> div []
                |> toUnstyled
                |> Query.fromHtml
                |> Query.find [ Selector.id "switch" ]
                |> Query.has [ Selector.attribute (Aria.disabled True) ]
    ]
