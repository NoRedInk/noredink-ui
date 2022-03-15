module Examples.Pennant exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.Pennant.V2 as Pennant


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
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        IconExamples.preview
            [ Pennant.premiumFlag
            , Pennant.expiredPremiumFlag
            , Pennant.disabledPremiumFlag
            ]
    , view =
        \_ ->
            [ IconExamples.viewWithCustomStyles "Premium Pennants"
                [ ( "premiumFlag"
                  , Pennant.premiumFlag
                  , [ Css.width (Css.px 80) ]
                  )
                , ( "expiredPremiumFlag"
                  , Pennant.expiredPremiumFlag
                  , [ Css.width (Css.px 80) ]
                  )
                , ( "disabledPremiumFlag"
                  , Pennant.disabledPremiumFlag
                  , [ Css.width (Css.px 80) ]
                  )
                ]
            ]
    }
