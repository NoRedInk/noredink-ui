module Gen.Accessibility.Styled.Style exposing (invisible, invisibleStyle, moduleName_, values_)

{-| 
@docs moduleName_, invisibleStyle, invisible, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Style" ]


{-| Makes content invisible without making it inaccessible.

    label [ css invisibleStyle ] [ text "Screen readers can still read me!" ]

invisibleStyle: Css.Style
-}
invisibleStyle : Elm.Expression
invisibleStyle =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Style" ]
        , name = "invisibleStyle"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| Makes content invisible without making it inaccessible.

    label invisible [ text "Screen readers can still read me!" ]

invisible: List (Html.Styled.Attribute msg)
-}
invisible : Elm.Expression
invisible =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Style" ]
        , name = "invisible"
        , annotation =
            Just
                (Type.list
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
                )
        }


values_ : { invisibleStyle : Elm.Expression, invisible : Elm.Expression }
values_ =
    { invisibleStyle =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Style" ]
            , name = "invisibleStyle"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , invisible =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Style" ]
            , name = "invisible"
            , annotation =
                Just
                    (Type.list
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


