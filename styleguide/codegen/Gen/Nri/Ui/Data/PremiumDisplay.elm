module Gen.Nri.Ui.Data.PremiumDisplay exposing (annotation_, caseOf_, make_, moduleName_)

{-| 
@docs moduleName_, annotation_, make_, caseOf_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Data", "PremiumDisplay" ]


annotation_ : { premiumDisplay : Type.Annotation }
annotation_ =
    { premiumDisplay =
        Type.namedWith
            [ "Nri", "Ui", "Data", "PremiumDisplay" ]
            "PremiumDisplay"
            []
    }


make_ :
    { free : Elm.Expression
    , premiumLocked : Elm.Expression
    , premiumUnlocked : Elm.Expression
    }
make_ =
    { free =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Data", "PremiumDisplay" ]
            , name = "Free"
            , annotation = Just (Type.namedWith [] "PremiumDisplay" [])
            }
    , premiumLocked =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Data", "PremiumDisplay" ]
            , name = "PremiumLocked"
            , annotation = Just (Type.namedWith [] "PremiumDisplay" [])
            }
    , premiumUnlocked =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Data", "PremiumDisplay" ]
            , name = "PremiumUnlocked"
            , annotation = Just (Type.namedWith [] "PremiumDisplay" [])
            }
    }


caseOf_ :
    { premiumDisplay :
        Elm.Expression
        -> { premiumDisplayTags_0_0
            | free : Elm.Expression
            , premiumLocked : Elm.Expression
            , premiumUnlocked : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { premiumDisplay =
        \premiumDisplayExpression premiumDisplayTags ->
            Elm.Case.custom
                premiumDisplayExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                    "PremiumDisplay"
                    []
                )
                [ Elm.Case.branch0 "Free" premiumDisplayTags.free
                , Elm.Case.branch0
                    "PremiumLocked"
                    premiumDisplayTags.premiumLocked
                , Elm.Case.branch0
                    "PremiumUnlocked"
                    premiumDisplayTags.premiumUnlocked
                ]
    }


