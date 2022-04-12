module Spec.Nri.Ui.Switch exposing (spec)

import Accessibility.Widget as Widget
import Html.Styled as HtmlStyled
import Nri.Ui.Switch.V1 as Switch
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Switch.V1"
        [ test "it works" <|
            \() ->
                program
                    [ Switch.label (HtmlStyled.text "Switch")
                    , Switch.onSwitch identity
                    , Switch.id "switch"
                    ]
                    True
                    -- switch starts with aria-checked=true and displays the label
                    |> ensureViewHas
                        [ Selector.attribute (Widget.checked (Just True))
                        , Selector.text "Switch"
                        ]
                    -- user can click the first switch
                    |> check "switch" "Switch" False
                    -- the switch now has aria-checked=false
                    |> ensureViewHas
                        [ Selector.attribute (Widget.checked (Just False))
                        , Selector.text "Switch"
                        ]
                    |> done
        ]


program : List (Switch.Attribute Bool) -> Bool -> ProgramTest Bool Bool ()
program attributes init =
    ProgramTest.createSandbox
        { init = init
        , update = \msg model -> msg
        , view = \isOpen -> Switch.view attributes isOpen |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()
