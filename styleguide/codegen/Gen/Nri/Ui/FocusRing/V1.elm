module Gen.Nri.Ui.FocusRing.V1 exposing (boxShadows, call_, customClass, forKeyboardUsers, forMouseUsers, innerColor, insetBoxShadow, moduleName_, outerBoxShadow, outerColor, styles, tightStyles, values_)

{-| 
@docs moduleName_, forKeyboardUsers, forMouseUsers, styles, tightStyles, boxShadows, outerBoxShadow, insetBoxShadow, customClass, outerColor, innerColor, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "FocusRing", "V1" ]


{-| When :focus-visible, add the two-tone focus ring.

Hides default focus ring from elements that are tagged as having a custom focus ring.

forKeyboardUsers: List Css.Global.Snippet
-}
forKeyboardUsers : Elm.Expression
forKeyboardUsers =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "forKeyboardUsers"
        , annotation =
            Just (Type.list (Type.namedWith [ "Css", "Global" ] "Snippet" []))
        }


{-| forMouseUsers: List Css.Global.Snippet -}
forMouseUsers : Elm.Expression
forMouseUsers =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "forMouseUsers"
        , annotation =
            Just (Type.list (Type.namedWith [ "Css", "Global" ] "Snippet" []))
        }


{-| A two-tone focus ring that will be visually apparent for any background/element combination.

NOTE: use `boxShadows` instead if your focusable element:

  - already has a box shadow
  - has an explicit border radius set

styles: List Css.Style
-}
styles : Elm.Expression
styles =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "styles"
        , annotation = Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
        }


{-| Prefer `styles` over tightStyles, except in cases where line spacing/font size will otherwise cause obscured content.

tightStyles: List Css.Style
-}
tightStyles : Elm.Expression
tightStyles =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "tightStyles"
        , annotation = Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
        }


{-| focus
        [ FocusRing.boxShadows [ "inset 0 3px 0 0 " ++ ColorsExtra.toCssString glacier ]
        , outline none
        ]

boxShadows: List String -> Css.Style
-}
boxShadows : List String -> Elm.Expression
boxShadows boxShadowsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "boxShadows"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list (List.map Elm.string boxShadowsArg) ]


{-| In special cases, we don't use a two-tone focus ring.

Be very sure this is what you need before using this!

outerBoxShadow: Css.Style
-}
outerBoxShadow : Elm.Expression
outerBoxShadow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "outerBoxShadow"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| In special cases, we don't use a two-tone focus ring, and an outset focus ring would be obscured.

Be very sure this is what you need before using this!

insetBoxShadow: Css.Style
-}
insetBoxShadow : Elm.Expression
insetBoxShadow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "insetBoxShadow"
        , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
        }


{-| Add this class to remove global focus styles. Only do this
if you'll be adding the two-tone focus ring styles another way.

customClass: String
-}
customClass : Elm.Expression
customClass =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "customClass"
        , annotation = Just Type.string
        }


{-| outerColor: Css.Color -}
outerColor : Elm.Expression
outerColor =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "outerColor"
        , annotation = Just (Type.namedWith [ "Css" ] "Color" [])
        }


{-| innerColor: Css.Color -}
innerColor : Elm.Expression
innerColor =
    Elm.value
        { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
        , name = "innerColor"
        , annotation = Just (Type.namedWith [ "Css" ] "Color" [])
        }


call_ : { boxShadows : Elm.Expression -> Elm.Expression }
call_ =
    { boxShadows =
        \boxShadowsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
                    , name = "boxShadows"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ boxShadowsArg ]
    }


values_ :
    { forKeyboardUsers : Elm.Expression
    , forMouseUsers : Elm.Expression
    , styles : Elm.Expression
    , tightStyles : Elm.Expression
    , boxShadows : Elm.Expression
    , outerBoxShadow : Elm.Expression
    , insetBoxShadow : Elm.Expression
    , customClass : Elm.Expression
    , outerColor : Elm.Expression
    , innerColor : Elm.Expression
    }
values_ =
    { forKeyboardUsers =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "forKeyboardUsers"
            , annotation =
                Just
                    (Type.list (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , forMouseUsers =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "forMouseUsers"
            , annotation =
                Just
                    (Type.list (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , styles =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "styles"
            , annotation =
                Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
            }
    , tightStyles =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "tightStyles"
            , annotation =
                Just (Type.list (Type.namedWith [ "Css" ] "Style" []))
            }
    , boxShadows =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "boxShadows"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , outerBoxShadow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "outerBoxShadow"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , insetBoxShadow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "insetBoxShadow"
            , annotation = Just (Type.namedWith [ "Css" ] "Style" [])
            }
    , customClass =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "customClass"
            , annotation = Just Type.string
            }
    , outerColor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "outerColor"
            , annotation = Just (Type.namedWith [ "Css" ] "Color" [])
            }
    , innerColor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusRing", "V1" ]
            , name = "innerColor"
            , annotation = Just (Type.namedWith [ "Css" ] "Color" [])
            }
    }


