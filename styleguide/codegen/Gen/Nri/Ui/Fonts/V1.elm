module Gen.Nri.Ui.Fonts.V1 exposing (baseFont, moduleName_, quizFont, ugFont, values_)

{-| 
@docs moduleName_, baseFont, quizFont, ugFont, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Fonts", "V1" ]


{-| Font for instructions, headers, and pretty much everything else

baseFont: Css.Style
-}
baseFont : Elm.Expression
baseFont =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
        , name = "baseFont"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| Font for question sentences, or most interactable or graded fields

quizFont: Css.Style
-}
quizFont : Elm.Expression
quizFont =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
        , name = "quizFont"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| Font for displaying user-generated content.

ugFont: Css.Style
-}
ugFont : Elm.Expression
ugFont =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
        , name = "ugFont"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


values_ :
    { baseFont : Elm.Expression
    , quizFont : Elm.Expression
    , ugFont : Elm.Expression
    }
values_ =
    { baseFont =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
            , name = "baseFont"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , quizFont =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
            , name = "quizFont"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , ugFont =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Fonts", "V1" ]
            , name = "ugFont"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    }


