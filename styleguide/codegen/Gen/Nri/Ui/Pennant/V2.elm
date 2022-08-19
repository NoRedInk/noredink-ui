module Gen.Nri.Ui.Pennant.V2 exposing (disabledPremiumFlag, expiredPremiumFlag, moduleName_, premiumFlag, values_)

{-| 
@docs moduleName_, premiumFlag, disabledPremiumFlag, expiredPremiumFlag, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Pennant", "V2" ]


{-| premiumFlag: Nri.Ui.Svg.V1.Svg -}
premiumFlag : Elm.Expression
premiumFlag =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
        , name = "premiumFlag"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
        }


{-| disabledPremiumFlag: Nri.Ui.Svg.V1.Svg -}
disabledPremiumFlag : Elm.Expression
disabledPremiumFlag =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
        , name = "disabledPremiumFlag"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
        }


{-| expiredPremiumFlag: Nri.Ui.Svg.V1.Svg -}
expiredPremiumFlag : Elm.Expression
expiredPremiumFlag =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
        , name = "expiredPremiumFlag"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
        }


values_ :
    { premiumFlag : Elm.Expression
    , disabledPremiumFlag : Elm.Expression
    , expiredPremiumFlag : Elm.Expression
    }
values_ =
    { premiumFlag =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
            , name = "premiumFlag"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
            }
    , disabledPremiumFlag =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
            , name = "disabledPremiumFlag"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
            }
    , expiredPremiumFlag =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Pennant", "V2" ]
            , name = "expiredPremiumFlag"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
            }
    }


