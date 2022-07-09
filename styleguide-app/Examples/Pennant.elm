module Examples.Pennant exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
    exposing
        ( IconExampleGroupWithCustomStyles
        , viewByGroupWithCustomStyles
        )
import Nri.Ui.Pennant.V2 as Pennant


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { name = "Pennant"
    , version = 2
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = IconExamples.init { label = "Premium", name = "premiumFlag", icon = Pennant.premiumFlag }
    , update = IconExamples.update
    , subscriptions = \_ -> Sub.none
    , preview =
        IconExamples.preview
            [ Pennant.premiumFlag
            , Pennant.expiredPremiumFlag
            , Pennant.disabledPremiumFlag
            ]
    , view = \ellieLinkConfig settings -> viewByGroupWithCustomStyles settings all
    }


all : List IconExampleGroupWithCustomStyles
all =
    [ ( "Premium Pennants"
      , [ ( "premiumFlag"
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
      )
    ]
