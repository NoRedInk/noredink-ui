module Gen.Nri.Ui.Svg.V1 exposing (annotation_, call_, init, moduleName_, toHtml, values_, withColor, withCss, withCustom, withHeight, withLabel, withWidth)

{-| 
@docs moduleName_, withColor, withLabel, withWidth, withHeight, withCss, withCustom, init, toHtml, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Svg", "V1" ]


{-| withColor: Css.Color -> Nri.Ui.Svg.V1.Svg -> Nri.Ui.Svg.V1.Svg -}
withColor : Elm.Expression -> Elm.Expression -> Elm.Expression
withColor withColorArg withColorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ withColorArg, withColorArg0 ]


{-| Add a title to the svg. Note that when the label is _not_ present, the icon will be entirely hidden from screenreader users.

Read [Carie Fisher's "Accessible Svgs"](https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/) article to learn more about accessible svgs.

Go through the [WCAG images tutorial](https://www.w3.org/WAI/tutorials/images/) to learn more about identifying when images are functional or decorative or something else.

withLabel: String -> Nri.Ui.Svg.V1.Svg -> Nri.Ui.Svg.V1.Svg
-}
withLabel : String -> Elm.Expression -> Elm.Expression
withLabel withLabelArg withLabelArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.string withLabelArg, withLabelArg0 ]


{-| withWidth: Css.Px -> Nri.Ui.Svg.V1.Svg -> Nri.Ui.Svg.V1.Svg -}
withWidth : Elm.Expression -> Elm.Expression -> Elm.Expression
withWidth withWidthArg withWidthArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ withWidthArg, withWidthArg0 ]


{-| withHeight: Css.Px -> Nri.Ui.Svg.V1.Svg -> Nri.Ui.Svg.V1.Svg -}
withHeight : Elm.Expression -> Elm.Expression -> Elm.Expression
withHeight withHeightArg withHeightArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ withHeightArg, withHeightArg0 ]


{-| Css for the SVG's container.

withCss: List Css.Style -> Nri.Ui.Svg.V1.Svg -> Nri.Ui.Svg.V1.Svg
-}
withCss : List Elm.Expression -> Elm.Expression -> Elm.Expression
withCss withCssArg withCssArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.list withCssArg, withCssArg0 ]


{-| withCustom: 
    List (Svg.Styled.Attribute Basics.Never)
    -> Nri.Ui.Svg.V1.Svg
    -> Nri.Ui.Svg.V1.Svg
-}
withCustom : List Elm.Expression -> Elm.Expression -> Elm.Expression
withCustom withCustomArg withCustomArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withCustom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.list withCustomArg, withCustomArg0 ]


{-| Pass through the viewbox as the first argument and the contents of the svg node as the second argument.

init: String -> List (Svg.Styled.Svg Basics.Never) -> Nri.Ui.Svg.V1.Svg
-}
init : String -> List Elm.Expression -> Elm.Expression
init initArg initArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.string initArg, Elm.list initArg0 ]


{-| render an svg.

toHtml: Nri.Ui.Svg.V1.Svg -> Svg.Styled.Svg msg
-}
toHtml : Elm.Expression -> Elm.Expression
toHtml toHtmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "toHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ toHtmlArg ]


annotation_ : { svg : Type.Annotation }
annotation_ =
    { svg = Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] }


call_ :
    { withColor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withLabel : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withWidth : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withHeight : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withCss : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withCustom : Elm.Expression -> Elm.Expression -> Elm.Expression
    , init : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toHtml : Elm.Expression -> Elm.Expression
    }
call_ =
    { withColor =
        \withColorArg withColorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Color" []
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withColorArg, withColorArg0 ]
    , withLabel =
        \withLabelArg withLabelArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withLabel"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withLabelArg, withLabelArg0 ]
    , withWidth =
        \withWidthArg withWidthArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" []
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withWidthArg, withWidthArg0 ]
    , withHeight =
        \withHeightArg withHeightArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" []
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withHeightArg, withHeightArg0 ]
    , withCss =
        \withCssArg withCssArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withCssArg, withCssArg0 ]
    , withCustom =
        \withCustomArg withCustomArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "withCustom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ withCustomArg, withCustomArg0 ]
    , init =
        \initArg initArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ initArg, initArg0 ]
    , toHtml =
        \toHtmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
                    , name = "toHtml"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toHtmlArg ]
    }


values_ :
    { withColor : Elm.Expression
    , withLabel : Elm.Expression
    , withWidth : Elm.Expression
    , withHeight : Elm.Expression
    , withCss : Elm.Expression
    , withCustom : Elm.Expression
    , init : Elm.Expression
    , toHtml : Elm.Expression
    }
values_ =
    { withColor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , withLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , withWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , withHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" []
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , withCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , withCustom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "withCustom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , init =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , toHtml =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Svg", "V1" ]
            , name = "toHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


