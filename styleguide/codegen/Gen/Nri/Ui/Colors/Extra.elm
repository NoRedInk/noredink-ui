module Gen.Nri.Ui.Colors.Extra exposing (call_, fromCssColor, moduleName_, toCssColor, toCssString, values_, withAlpha)

{-| 
@docs moduleName_, toCssColor, fromCssColor, withAlpha, toCssString, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Colors", "Extra" ]


{-| toCssColor: SolidColor.SolidColor -> Css.Color -}
toCssColor : Elm.Expression -> Elm.Expression
toCssColor toCssColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "toCssColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "SolidColor" ] "SolidColor" [] ]
                        (Type.namedWith [ "Css" ] "Color" [])
                    )
            }
        )
        [ toCssColorArg ]


{-| fromCssColor: Css.Color -> SolidColor.SolidColor -}
fromCssColor : Elm.Expression -> Elm.Expression
fromCssColor fromCssColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "fromCssColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" [] ]
                        (Type.namedWith [ "SolidColor" ] "SolidColor" [])
                    )
            }
        )
        [ fromCssColorArg ]


{-| Add an alpha property to a Css.Color
grassland -- "{ value = "#56bf74", color = Compatible, red = 86, green = 191, blue = 116, alpha = 1, warnings = [] } : Css.Color"
withAlpha 0.5 grassland -- "{ value = "rgba(86, 191, 116, 0.5)", color = Compatible, warnings = [], red = 86, green = 191, blue = 116, alpha = 0.5 } : Css.Color"

withAlpha: Float -> Css.Color -> Css.Color
-}
withAlpha : Float -> Elm.Expression -> Elm.Expression
withAlpha withAlphaArg withAlphaArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "withAlpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.namedWith [ "Css" ] "Color" [] ]
                        (Type.namedWith [ "Css" ] "Color" [])
                    )
            }
        )
        [ Elm.float withAlphaArg, withAlphaArg0 ]


{-| toCssString: Css.Color -> String -}
toCssString : Elm.Expression -> Elm.Expression
toCssString toCssStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "toCssString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" [] ]
                        Type.string
                    )
            }
        )
        [ toCssStringArg ]


call_ :
    { toCssColor : Elm.Expression -> Elm.Expression
    , fromCssColor : Elm.Expression -> Elm.Expression
    , withAlpha : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toCssString : Elm.Expression -> Elm.Expression
    }
call_ =
    { toCssColor =
        \toCssColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
                    , name = "toCssColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "SolidColor" ]
                                    "SolidColor"
                                    []
                                ]
                                (Type.namedWith [ "Css" ] "Color" [])
                            )
                    }
                )
                [ toCssColorArg ]
    , fromCssColor =
        \fromCssColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
                    , name = "fromCssColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Color" [] ]
                                (Type.namedWith [ "SolidColor" ] "SolidColor" []
                                )
                            )
                    }
                )
                [ fromCssColorArg ]
    , withAlpha =
        \withAlphaArg withAlphaArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
                    , name = "withAlpha"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.namedWith [ "Css" ] "Color" []
                                ]
                                (Type.namedWith [ "Css" ] "Color" [])
                            )
                    }
                )
                [ withAlphaArg, withAlphaArg0 ]
    , toCssString =
        \toCssStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
                    , name = "toCssString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Color" [] ]
                                Type.string
                            )
                    }
                )
                [ toCssStringArg ]
    }


values_ :
    { toCssColor : Elm.Expression
    , fromCssColor : Elm.Expression
    , withAlpha : Elm.Expression
    , toCssString : Elm.Expression
    }
values_ =
    { toCssColor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "toCssColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "SolidColor" ] "SolidColor" [] ]
                        (Type.namedWith [ "Css" ] "Color" [])
                    )
            }
    , fromCssColor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "fromCssColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" [] ]
                        (Type.namedWith [ "SolidColor" ] "SolidColor" [])
                    )
            }
    , withAlpha =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "withAlpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.namedWith [ "Css" ] "Color" [] ]
                        (Type.namedWith [ "Css" ] "Color" [])
                    )
            }
    , toCssString =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Colors", "Extra" ]
            , name = "toCssString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Color" [] ]
                        Type.string
                    )
            }
    }


