module Gen.Nri.Ui.Effects.V1 exposing (moduleName_, selectionShadow, values_)

{-| 
@docs moduleName_, selectionShadow, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Effects", "V1" ]


{-| Draw a 2 px thick ochre border around the element to indicate it is
selected.

This uses a CSS box shadow to draw what looks like a border. Box shadows are
perfect for this because they don't affect the elements positioning in any way.
This means we can be sure switching the selection shadow on and off is not
going to make the element jump.

selectionShadow: List Css.Style
-}
selectionShadow : Elm.Expression
selectionShadow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Effects", "V1" ]
        , name = "selectionShadow"
        , annotation = Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
        }


values_ : { selectionShadow : Elm.Expression }
values_ =
    { selectionShadow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Effects", "V1" ]
            , name = "selectionShadow"
            , annotation =
                Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
            }
    }


