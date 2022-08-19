module Gen.Css.Animations exposing (all, annotation_, backgroundColor, backgroundSize, backgroundSize2, border, border2, border3, borderBottom, borderBottom2, borderBottom3, call_, custom, keyframes, moduleName_, opacity, property, transform, values_)

{-| 
@docs moduleName_, keyframes, opacity, transform, all, backgroundSize, backgroundSize2, border, property, backgroundColor, border2, border3, borderBottom, borderBottom2, borderBottom3, custom, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Css", "Animations" ]


{-| Specify a list of ( percentage, properties) tuples to use for
animation keyframes.

Pass the result to [`Css.animationName`](Css#animationName)!

`keyframes []` is equivalent to `none`.

keyframes: List ( Int, List Css.Animations.Property ) -> Css.Animations.Keyframes {}
-}
keyframes : List Elm.Expression -> Elm.Expression
keyframes keyframesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "keyframes"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.int
                                (Type.list
                                    (Type.namedWith
                                        [ "Css", "Animations" ]
                                        "Property"
                                        []
                                    )
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Css", "Animations" ]
                            "Keyframes"
                            [ Type.record [] ]
                        )
                    )
            }
        )
        [ Elm.list keyframesArg ]


{-| opacity: 
    { compatible | value : String, number : Css.Structure.Compatible }
    -> Css.Animations.Property
-}
opacity :
    { compatible | value : String, number : Elm.Expression } -> Elm.Expression
opacity opacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "number"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "value" (Elm.string opacityArg.value)
            , Tuple.pair "number" opacityArg.number
            ]
        ]


{-| transform: 
    List { compatible | value : String, transform : Css.Structure.Compatible }
    -> Css.Animations.Property
-}
transform :
    List { compatible | value : String, transform : Elm.Expression }
    -> Elm.Expression
transform transformArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.extensible
                                "compatible"
                                [ ( "value", Type.string )
                                , ( "transform"
                                  , Type.namedWith
                                        [ "Css", "Structure" ]
                                        "Compatible"
                                        []
                                  )
                                ]
                            )
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.list
            (List.map
                (\unpack ->
                    Elm.record
                        [ Tuple.pair "value" (Elm.string unpack.value)
                        , Tuple.pair "transform" unpack.transform
                        ]
                )
                transformArg
            )
        ]


{-| all: 
    { compatible | value : String, all : Css.Structure.Compatible }
    -> Css.Animations.Property
-}
all : { compatible | value : String, all : Elm.Expression } -> Elm.Expression
all allArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "all"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "value" (Elm.string allArg.value)
            , Tuple.pair "all" allArg.all
            ]
        ]


{-| backgroundSize: Css.Internal.LengthOrAutoOrCoverOrContain compatible -> Css.Animations.Property -}
backgroundSize : Elm.Expression -> Elm.Expression
backgroundSize backgroundSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ backgroundSizeArg ]


{-| backgroundSize2: 
    Css.Internal.LengthOrAutoOrCoverOrContain compatible
    -> Css.Internal.LengthOrAutoOrCoverOrContain compatible
    -> Css.Animations.Property
-}
backgroundSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
backgroundSize2 backgroundSize2Arg backgroundSize2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ backgroundSize2Arg, backgroundSize2Arg0 ]


{-| border: Css.Internal.Length compatible units -> Css.Animations.Property -}
border : Elm.Expression -> Elm.Expression
border borderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ borderArg ]


{-| Define a custom animatable property.

    css [ animationName (keyframes [ ( 5, [ property "backdrop-filter" "blur(3px)" ] ) ]) ]

...outputs

    @keyframes _cf0d1b {
        5% {
            backdrop-filter:blur(3px);
        }
    }

    ...

    animation-name: _cf0d1b

property: String -> String -> Css.Animations.Property
-}
property : String -> String -> Elm.Expression
property propertyArg propertyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.string propertyArg, Elm.string propertyArg0 ]


{-| backgroundColor: 
    { compatible | value : String, color : Css.Structure.Compatible }
    -> Css.Animations.Property
-}
backgroundColor :
    { compatible | value : String, color : Elm.Expression } -> Elm.Expression
backgroundColor backgroundColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "color"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "value" (Elm.string backgroundColorArg.value)
            , Tuple.pair "color" backgroundColorArg.color
            ]
        ]


{-| border2: 
    Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Animations.Property
-}
border2 : Elm.Expression -> Elm.Expression -> Elm.Expression
border2 border2Arg border2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ border2Arg, border2Arg0 ]


{-| border3: 
    Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Animations.Property
-}
border3 : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
border3 border3Arg border3Arg0 border3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ border3Arg, border3Arg0, border3Arg1 ]


{-| borderBottom: Css.Internal.Length compatible units -> Css.Animations.Property -}
borderBottom : Elm.Expression -> Elm.Expression
borderBottom borderBottomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ borderBottomArg ]


{-| borderBottom2: 
    Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Animations.Property
-}
borderBottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
borderBottom2 borderBottom2Arg borderBottom2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ borderBottom2Arg, borderBottom2Arg0 ]


{-| borderBottom3: 
    Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Internal.Length compatible units
    -> Css.Animations.Property
-}
borderBottom3 :
    Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
borderBottom3 borderBottom3Arg borderBottom3Arg0 borderBottom3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ borderBottom3Arg, borderBottom3Arg0, borderBottom3Arg1 ]


{-| Create a custom animatable property with the given name and value. You can use this to define vendor-prefixed or experimental properties.

    custom "-moz-outline-radius" "25px"

**WARNING:** Some properties are not exposed in this module because they
are not animatable! This means browsers will refuse to animate them even if you pass them to this function.

MDN has [a list of the animatable properties](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_animated_properties).

custom: String -> String -> Css.Animations.Property
-}
custom : String -> String -> Elm.Expression
custom customArg customArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
        )
        [ Elm.string customArg, Elm.string customArg0 ]


annotation_ :
    { property : Type.Annotation
    , keyframes : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { property =
        Type.alias
            moduleName_
            "Property"
            []
            (Type.namedWith [ "Css", "Internal" ] "AnimationProperty" [])
    , keyframes =
        \keyframesArg0 ->
            Type.alias
                moduleName_
                "Keyframes"
                [ keyframesArg0 ]
                (Type.extensible
                    "compatible"
                    [ ( "keyframes"
                      , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                      )
                    , ( "none"
                      , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                      )
                    , ( "value", Type.string )
                    ]
                )
    }


call_ :
    { keyframes : Elm.Expression -> Elm.Expression
    , opacity : Elm.Expression -> Elm.Expression
    , transform : Elm.Expression -> Elm.Expression
    , all : Elm.Expression -> Elm.Expression
    , backgroundSize : Elm.Expression -> Elm.Expression
    , backgroundSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , border : Elm.Expression -> Elm.Expression
    , property : Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundColor : Elm.Expression -> Elm.Expression
    , border2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , border3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottom : Elm.Expression -> Elm.Expression
    , borderBottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottom3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { keyframes =
        \keyframesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "keyframes"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.int
                                        (Type.list
                                            (Type.namedWith
                                                [ "Css", "Animations" ]
                                                "Property"
                                                []
                                            )
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Keyframes"
                                    [ Type.record [] ]
                                )
                            )
                    }
                )
                [ keyframesArg ]
    , opacity =
        \opacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "opacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.extensible
                                    "compatible"
                                    [ ( "value", Type.string )
                                    , ( "number"
                                      , Type.namedWith
                                            [ "Css", "Structure" ]
                                            "Compatible"
                                            []
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ opacityArg ]
    , transform =
        \transformArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "transform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.extensible
                                        "compatible"
                                        [ ( "value", Type.string )
                                        , ( "transform"
                                          , Type.namedWith
                                                [ "Css", "Structure" ]
                                                "Compatible"
                                                []
                                          )
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ transformArg ]
    , all =
        \allArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "all"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.extensible
                                    "compatible"
                                    [ ( "value", Type.string )
                                    , ( "all"
                                      , Type.namedWith
                                            [ "Css", "Structure" ]
                                            "Compatible"
                                            []
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ allArg ]
    , backgroundSize =
        \backgroundSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "backgroundSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "LengthOrAutoOrCoverOrContain"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ backgroundSizeArg ]
    , backgroundSize2 =
        \backgroundSize2Arg backgroundSize2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "backgroundSize2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "LengthOrAutoOrCoverOrContain"
                                    [ Type.var "compatible" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "LengthOrAutoOrCoverOrContain"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ backgroundSize2Arg, backgroundSize2Arg0 ]
    , border =
        \borderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "border"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ borderArg ]
    , property =
        \propertyArg propertyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "property"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ propertyArg, propertyArg0 ]
    , backgroundColor =
        \backgroundColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "backgroundColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.extensible
                                    "compatible"
                                    [ ( "value", Type.string )
                                    , ( "color"
                                      , Type.namedWith
                                            [ "Css", "Structure" ]
                                            "Compatible"
                                            []
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ backgroundColorArg ]
    , border2 =
        \border2Arg border2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "border2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ border2Arg, border2Arg0 ]
    , border3 =
        \border3Arg border3Arg0 border3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "border3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ border3Arg, border3Arg0, border3Arg1 ]
    , borderBottom =
        \borderBottomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "borderBottom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomArg ]
    , borderBottom2 =
        \borderBottom2Arg borderBottom2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "borderBottom2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ borderBottom2Arg, borderBottom2Arg0 ]
    , borderBottom3 =
        \borderBottom3Arg borderBottom3Arg0 borderBottom3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "borderBottom3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                , Type.namedWith
                                    [ "Css", "Internal" ]
                                    "Length"
                                    [ Type.var "compatible", Type.var "units" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ borderBottom3Arg, borderBottom3Arg0, borderBottom3Arg1 ]
    , custom =
        \customArg customArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Animations" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Css", "Animations" ]
                                    "Property"
                                    []
                                )
                            )
                    }
                )
                [ customArg, customArg0 ]
    }


values_ :
    { keyframes : Elm.Expression
    , opacity : Elm.Expression
    , transform : Elm.Expression
    , all : Elm.Expression
    , backgroundSize : Elm.Expression
    , backgroundSize2 : Elm.Expression
    , border : Elm.Expression
    , property : Elm.Expression
    , backgroundColor : Elm.Expression
    , border2 : Elm.Expression
    , border3 : Elm.Expression
    , borderBottom : Elm.Expression
    , borderBottom2 : Elm.Expression
    , borderBottom3 : Elm.Expression
    , custom : Elm.Expression
    }
values_ =
    { keyframes =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "keyframes"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.int
                                (Type.list
                                    (Type.namedWith
                                        [ "Css", "Animations" ]
                                        "Property"
                                        []
                                    )
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Css", "Animations" ]
                            "Keyframes"
                            [ Type.record [] ]
                        )
                    )
            }
    , opacity =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "number"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , transform =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.extensible
                                "compatible"
                                [ ( "value", Type.string )
                                , ( "transform"
                                  , Type.namedWith
                                        [ "Css", "Structure" ]
                                        "Compatible"
                                        []
                                  )
                                ]
                            )
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , all =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "all"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , backgroundSize =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , backgroundSize2 =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "LengthOrAutoOrCoverOrContain"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , border =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , property =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , backgroundColor =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "backgroundColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "compatible"
                            [ ( "value", Type.string )
                            , ( "color"
                              , Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Compatible"
                                    []
                              )
                            ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , border2 =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , border3 =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "border3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , borderBottom =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , borderBottom2 =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , borderBottom3 =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "borderBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        , Type.namedWith
                            [ "Css", "Internal" ]
                            "Length"
                            [ Type.var "compatible", Type.var "units" ]
                        ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Css", "Animations" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css", "Animations" ] "Property" [])
                    )
            }
    }


