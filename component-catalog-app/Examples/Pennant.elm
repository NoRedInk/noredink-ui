module Examples.Pennant exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
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
    { moduleName = "Pennant"
    , version = 2
    , label = "Premium"
    , name = "premiumFlag"
    , icon = Pennant.premiumFlag
    , renderSvgCode = \name -> "Pennant." ++ name
    , preview =
        IconExamples.preview
            [ Pennant.premiumFlag
            , Pennant.expiredPremiumFlag
            , Pennant.disabledPremiumFlag
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
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
