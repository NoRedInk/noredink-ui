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
    , version = 2
    , state = True
    , update = \(Switch new) _ -> ( new, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Switch.view { label = "Toggle Off", id = "preview-switch-a" }
            [ Switch.selected False
            , Switch.custom [ Key.tabbable False ]
            ]
        , Switch.view { label = "Toggle On", id = "preview-switch-b" }
            [ Switch.selected True
            , Switch.custom [ Key.tabbable False ]
            ]
        ]
    , view =
        \ellieLinkConfig interactiveIsOn ->
            [ Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Interactive" ]
            , Switch.view
                { id = "switch-interactive"
                , label = "Show pandas in results"
                }
                [ Switch.onSwitch Switch
                , Switch.selected interactiveIsOn
                ]
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Disabled (On)" ]
            , Switch.view
                { id = "switch-disabled-on"
                , label = "Permanently on"
                }
                [ Switch.disabled
                , Switch.selected True
                ]
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Disabled (Off)" ]
            , Switch.view
                { id = "switch-disabled-off"
                , label = "Permanently off"
                }
                [ Switch.disabled
                , Switch.selected False
                ]
            ]
    , categories = [ Category.Inputs ]
    , keyboardSupport = [{- TODO -}]
    }
