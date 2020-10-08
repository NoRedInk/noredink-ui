module Examples.Pennant exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Pennant"
    , version = 2
    , categories = [ Icons ]
    , atomicDesignType = Atom
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ Html.div [ css [ Css.displayFlex, Css.width (Css.px 200) ] ]
                [ Pennant.premiumFlag
                    |> Svg.withHeight (Css.px 60)
                    |> Svg.toHtml
                , Pennant.disabledPremiumFlag
                    |> Svg.withHeight (Css.px 60)
                    |> Svg.toHtml
                , Pennant.expiredPremiumFlag
                    |> Svg.withHeight (Css.px 60)
                    |> Svg.toHtml
                ]
            ]
    }
