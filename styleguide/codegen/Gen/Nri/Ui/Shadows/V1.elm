module Gen.Nri.Ui.Shadows.V1 exposing (high, low, medium, moduleName_, values_)

{-| 
@docs moduleName_, low, medium, high, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Shadows", "V1" ]


{-| Low: for standard containers and similar elements like large messages

low: Css.Style
-}
low : Elm.Expression
low =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
        , name = "low"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| Medium: for larger, more prominent containers like Container.Pillow and marketing site cards

medium: Css.Style
-}
medium : Elm.Expression
medium =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
        , name = "medium"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| High: for “floating” elements like tooltips, popovers, and modals

high: Css.Style
-}
high : Elm.Expression
high =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
        , name = "high"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


values_ :
    { low : Elm.Expression, medium : Elm.Expression, high : Elm.Expression }
values_ =
    { low =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
            , name = "low"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , medium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
            , name = "medium"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , high =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Shadows", "V1" ]
            , name = "high"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    }


