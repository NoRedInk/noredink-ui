module Examples.Pennant exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Css
import Example exposing (Example)
import IconExamples exposing (Group)
import Nri.Ui.Pennant.V3 as Pennant


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { moduleName = "Pennant"
    , version = 3
    , label = "Premium"
    , name = "premiumFlag"
    , icon = Pennant.contentPremiumFlag
    , renderSvgCode = \name -> "Pennant." ++ name
    , preview =
        IconExamples.preview
            [ Pennant.contentPremiumFlag
            , Pennant.activePremiumFlag
            , Pennant.inactivePremiumFlag
            , Pennant.expiredPremiumFlag
            , Pennant.giftPremiumFlag
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Premium Pennants"
      , [ ( "contentPremiumFlag"
          , Pennant.contentPremiumFlag
          , [ Css.width (Css.px 80) ]
          )
        , ( "activePremiumFlag"
          , Pennant.activePremiumFlag
          , [ Css.width (Css.px 80) ]
          )
        , ( "inactivePremiumFlag"
          , Pennant.inactivePremiumFlag
          , [ Css.width (Css.px 80) ]
          )
        , ( "expiredPremiumFlag"
          , Pennant.expiredPremiumFlag
          , [ Css.width (Css.px 80) ]
          )
        , ( "giftPremiumFlag"
          , Pennant.giftPremiumFlag
          , [ Css.width (Css.px 80) ]
          )
        ]
      )
    ]
