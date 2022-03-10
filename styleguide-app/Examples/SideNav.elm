module Examples.SideNav exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.SideNav.V2 as SideNav


{-| -}
example : Example State Msg
example =
    { name = "SideNav"
    , version = 2
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport = []
    , preview = [ viewPreview ]
    , view =
        \state ->
            []
    }


viewPreview : Html msg
viewPreview =
    div
        [ css
            [ Css.height (Css.px 80)
            , Css.backgroundColor Colors.white
            , Css.padding (Css.px 8)
            , Css.displayFlex
            ]
        ]
        [ div
            [ css
                [ Css.flexGrow (Css.int 1)
                , Css.backgroundColor Colors.gray96
                , Css.borderRadius (Css.px 2)
                ]
            ]
            [ div
                [ css
                    [ Css.height (Css.px 8)
                    , Css.backgroundColor Colors.glacier
                    , Css.borderRadius (Css.px 2)
                    , Css.margin2 (Css.px 8) (Css.px 4)
                    ]
                ]
                []
            ]
        , div [ css [ Css.flexGrow (Css.int 2) ] ] []
        ]


{-| -}
type alias State =
    {}


{-| -}
init : State
init =
    {}


{-| -}
type alias Msg =
    ()


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        () ->
            ( state, Cmd.none )
