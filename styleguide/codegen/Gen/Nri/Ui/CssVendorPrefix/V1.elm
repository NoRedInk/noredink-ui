module Gen.Nri.Ui.CssVendorPrefix.V1 exposing (call_, complexProperty, moduleName_, property, value, values_)

{-| 
@docs moduleName_, property, value, complexProperty, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "CssVendorPrefix", "V1" ]


{-| Same as Css.property but vendor prefixed.

property: String -> String -> Css.Style
-}
property : String -> String -> Elm.Expression
property propertyArg propertyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.string propertyArg, Elm.string propertyArg0 ]


{-| Same as Css.property but vendor prefixed.

value: String -> String -> Css.Style
-}
value : String -> String -> Elm.Expression
value valueArg valueArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.string valueArg, Elm.string valueArg0 ]


{-| Used to build more complex Css styles

complexProperty: (String -> Css.Style) -> Css.Style
-}
complexProperty : (Elm.Expression -> Elm.Expression) -> Elm.Expression
complexProperty complexPropertyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "complexProperty"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.string ]
                            (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.functionReduced "complexPropertyUnpack" complexPropertyArg ]


call_ :
    { property : Elm.Expression -> Elm.Expression -> Elm.Expression
    , value : Elm.Expression -> Elm.Expression -> Elm.Expression
    , complexProperty : Elm.Expression -> Elm.Expression
    }
call_ =
    { property =
        \propertyArg propertyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
                    , name = "property"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ propertyArg, propertyArg0 ]
    , value =
        \valueArg valueArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
                    , name = "value"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ valueArg, valueArg0 ]
    , complexProperty =
        \complexPropertyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
                    , name = "complexProperty"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.string ]
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ complexPropertyArg ]
    }


values_ :
    { property : Elm.Expression
    , value : Elm.Expression
    , complexProperty : Elm.Expression
    }
values_ =
    { property =
        Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , value =
        Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , complexProperty =
        Elm.value
            { importFrom = [ "Nri", "Ui", "CssVendorPrefix", "V1" ]
            , name = "complexProperty"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.string ]
                            (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    }


