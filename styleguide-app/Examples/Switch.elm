module Examples.Switch exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Switch.V2 as Switch


{-| -}
type alias State =
    Bool


{-| -}
type Msg
    = Switch Bool


example : Example State Msg
example =
    { name = "Switch"
    , version = 1
    , state = True
    , update = \(Switch new) _ -> ( new, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Switch.view
            [ Switch.label "Toggle On"
            , Switch.custom [ Key.tabbable False ]
            ]
            False
        , Switch.view
            [ Switch.label "Toggle Off"
            , Switch.custom [ Key.tabbable False ]
            ]
            True
        ]
    , view =
        \ellieLinkConfig interactiveIsOn ->
            [ Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Interactive" ]
            , Switch.view
                [ Switch.onSwitch Switch
                , Switch.id "switch-interactive"
                , Switch.label
                    (if interactiveIsOn then
                        "On"

                     else
                        "Off"
                    )
                ]
                interactiveIsOn
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Disabled (On)" ]
            , Switch.view
                [ Switch.disabled
                , Switch.id "switch-disabled-on"
                , Switch.label "Permanently on"
                ]
                True
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Disabled (Off)" ]
            , Switch.view
                [ Switch.disabled
                , Switch.id "switch-disabled-off"
                , Switch.label "Permanently off"
                ]
                False
            ]
    , categories = [ Category.Inputs ]
    , keyboardSupport = [{- TODO -}]
    }
