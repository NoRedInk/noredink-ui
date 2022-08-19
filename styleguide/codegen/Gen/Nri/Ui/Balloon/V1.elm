module Gen.Nri.Ui.Balloon.V1 exposing (annotation_, balloon, call_, green, moduleName_, navy, onBottom, onLeft, onRight, onTop, orange, paddingPx, purple, values_, white, widthPct, widthPx)

{-| 
@docs moduleName_, balloon, green, purple, orange, white, navy, onBottom, onLeft, onRight, onTop, widthPx, widthPct, paddingPx, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Balloon", "V1" ]


{-| Green balloon without an arrow by default.

     __________
    |         |
    |_________|

balloon: List Nri.Ui.Balloon.V1.Attribute -> Html.Styled.Html msg -> Html.Styled.Html msg
-}
balloon : List Elm.Expression -> Elm.Expression -> Elm.Expression
balloon balloonArg balloonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "balloon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Balloon", "V1" ]
                                "Attribute"
                                []
                            )
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list balloonArg, balloonArg0 ]


{-| Green theme (This is the default theme.)

green: Nri.Ui.Balloon.V1.Attribute
-}
green : Elm.Expression
green =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "green"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Purple theme

purple: Nri.Ui.Balloon.V1.Attribute
-}
purple : Elm.Expression
purple =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "purple"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Orange theme

orange: Nri.Ui.Balloon.V1.Attribute
-}
orange : Elm.Expression
orange =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "orange"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| White theme

white: Nri.Ui.Balloon.V1.Attribute
-}
white : Elm.Expression
white =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "white"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Navy theme

navy: Nri.Ui.Balloon.V1.Attribute
-}
navy : Elm.Expression
navy =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "navy"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Balloon with the arrow on the top.

     ___/\_____
    |         |
    |_________|

onBottom: Nri.Ui.Balloon.V1.Attribute
-}
onBottom : Elm.Expression
onBottom =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "onBottom"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Balloon with the arrow on the right.

      __________
     |         |
     |          >
     |_________|

onLeft: Nri.Ui.Balloon.V1.Attribute
-}
onLeft : Elm.Expression
onLeft =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "onLeft"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Balloon with the arrow on the left.

      __________
     |         |
    <          |
     |_________|

onRight: Nri.Ui.Balloon.V1.Attribute
-}
onRight : Elm.Expression
onRight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "onRight"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Balloon with the arrow on the bottom.

     __________
    |         |
    |___  ____|
        \/

onTop: Nri.Ui.Balloon.V1.Attribute
-}
onTop : Elm.Expression
onTop =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
        , name = "onTop"
        , annotation =
            Just
                (Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" [])
        }


{-| Width of the balloon in pixels.

widthPx: Float -> Nri.Ui.Balloon.V1.Attribute
-}
widthPx : Float -> Elm.Expression
widthPx widthPxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "widthPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.float widthPxArg ]


{-| Warning: using a percentage-based width may change the positioning of the element
in unexpected ways.

widthPct: Float -> Nri.Ui.Balloon.V1.Attribute
-}
widthPct : Float -> Elm.Expression
widthPct widthPctArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "widthPct"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.float widthPctArg ]


{-| Padding of the balloon in pixels.

paddingPx: Float -> Nri.Ui.Balloon.V1.Attribute
-}
paddingPx : Float -> Elm.Expression
paddingPx paddingPxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "paddingPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.float paddingPxArg ]


annotation_ : { attribute : Type.Annotation }
annotation_ =
    { attribute = Type.namedWith [ "Nri", "Ui", "Balloon", "V1" ] "Attribute" []
    }


call_ :
    { balloon : Elm.Expression -> Elm.Expression -> Elm.Expression
    , widthPx : Elm.Expression -> Elm.Expression
    , widthPct : Elm.Expression -> Elm.Expression
    , paddingPx : Elm.Expression -> Elm.Expression
    }
call_ =
    { balloon =
        \balloonArg balloonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
                    , name = "balloon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Balloon", "V1" ]
                                        "Attribute"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ balloonArg, balloonArg0 ]
    , widthPx =
        \widthPxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
                    , name = "widthPx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Balloon", "V1" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ widthPxArg ]
    , widthPct =
        \widthPctArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
                    , name = "widthPct"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Balloon", "V1" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ widthPctArg ]
    , paddingPx =
        \paddingPxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
                    , name = "paddingPx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Balloon", "V1" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ paddingPxArg ]
    }


values_ :
    { balloon : Elm.Expression
    , green : Elm.Expression
    , purple : Elm.Expression
    , orange : Elm.Expression
    , white : Elm.Expression
    , navy : Elm.Expression
    , onBottom : Elm.Expression
    , onLeft : Elm.Expression
    , onRight : Elm.Expression
    , onTop : Elm.Expression
    , widthPx : Elm.Expression
    , widthPct : Elm.Expression
    , paddingPx : Elm.Expression
    }
values_ =
    { balloon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "balloon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Balloon", "V1" ]
                                "Attribute"
                                []
                            )
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , green =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "green"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , purple =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "purple"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , orange =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "orange"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , white =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "white"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , navy =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "navy"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , onBottom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "onBottom"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , onLeft =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "onLeft"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , onRight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "onRight"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , onTop =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "onTop"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Balloon", "V1" ]
                        "Attribute"
                        []
                    )
            }
    , widthPx =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "widthPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
    , widthPct =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "widthPct"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
    , paddingPx =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Balloon", "V1" ]
            , name = "paddingPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Balloon", "V1" ]
                            "Attribute"
                            []
                        )
                    )
            }
    }


