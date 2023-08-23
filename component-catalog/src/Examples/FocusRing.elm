module Examples.FocusRing exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Table.V7 as Table


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "FocusRing"
    , version = 1
    , categories = [ Text, Atoms ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        [ div
            [ css
                [ Css.width (Css.pct 100)
                , Css.height (Css.px 20)
                , Css.batch FocusRing.styles
                , Css.marginBottom (Css.px 30)
                ]
            ]
            []
        , div
            [ css
                [ Css.width (Css.pct 100)
                , Css.height (Css.px 20)
                , Css.batch FocusRing.tightStyles
                ]
            ]
            []
        ]
    , about = []
    , view =
        \_ _ ->
            [ Heading.h2 [ Heading.plaintext "Examples" ]
            ]
    }
