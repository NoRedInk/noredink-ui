module Gen.Nri.Ui.Html.V3 exposing (call_, defaultOptions, moduleName_, onKeyUp, values_, viewIf, viewJust)

{-| 
@docs moduleName_, viewJust, viewIf, onKeyUp, defaultOptions, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Html", "V3" ]


{-| View value of if `Maybe` is a `Just`, otherwise show nothing.

viewJust: (a -> Html.Styled.Html msg) -> Maybe a -> Html.Styled.Html msg
-}
viewJust :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
viewJust viewJustArg viewJustArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "viewJust"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.maybe (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "viewJustUnpack" viewJustArg, viewJustArg0 ]


{-| viewIf: (() -> Html.Styled.Html msg) -> Bool -> Html.Styled.Html msg -}
viewIf : (Elm.Expression -> Elm.Expression) -> Bool -> Elm.Expression
viewIf viewIfArg viewIfArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "viewIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.bool
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "viewIfUnpack" viewIfArg, Elm.bool viewIfArg0 ]


{-| Convert a keycode into a message on keyup

onKeyUp: 
    { preventDefault : Bool, stopPropagation : Bool }
    -> (Int -> Maybe a)
    -> Html.Styled.Attribute a
-}
onKeyUp :
    { preventDefault : Bool, stopPropagation : Bool }
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
onKeyUp onKeyUpArg onKeyUpArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "onKeyUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "preventDefault", Type.bool )
                            , ( "stopPropagation", Type.bool )
                            ]
                        , Type.function [ Type.int ] (Type.maybe (Type.var "a"))
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "preventDefault" (Elm.bool onKeyUpArg.preventDefault)
            , Tuple.pair "stopPropagation" (Elm.bool onKeyUpArg.stopPropagation)
            ]
        , Elm.functionReduced "onKeyUpUnpack" onKeyUpArg0
        ]


{-| defaultOptions: { preventDefault : Bool, stopPropagation : Bool } -}
defaultOptions : Elm.Expression
defaultOptions =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Html", "V3" ]
        , name = "defaultOptions"
        , annotation =
            Just
                (Type.record
                    [ ( "preventDefault", Type.bool )
                    , ( "stopPropagation", Type.bool )
                    ]
                )
        }


call_ :
    { viewJust : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewIf : Elm.Expression -> Elm.Expression -> Elm.Expression
    , onKeyUp : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { viewJust =
        \viewJustArg viewJustArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "V3" ]
                    , name = "viewJust"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                , Type.maybe (Type.var "a")
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewJustArg, viewJustArg0 ]
    , viewIf =
        \viewIfArg viewIfArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "V3" ]
                    , name = "viewIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.unit ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                , Type.bool
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewIfArg, viewIfArg0 ]
    , onKeyUp =
        \onKeyUpArg onKeyUpArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "V3" ]
                    , name = "onKeyUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "preventDefault", Type.bool )
                                    , ( "stopPropagation", Type.bool )
                                    ]
                                , Type.function
                                    [ Type.int ]
                                    (Type.maybe (Type.var "a"))
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ onKeyUpArg, onKeyUpArg0 ]
    }


values_ :
    { viewJust : Elm.Expression
    , viewIf : Elm.Expression
    , onKeyUp : Elm.Expression
    , defaultOptions : Elm.Expression
    }
values_ =
    { viewJust =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "viewJust"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.maybe (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewIf =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "viewIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.bool
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onKeyUp =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "onKeyUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "preventDefault", Type.bool )
                            , ( "stopPropagation", Type.bool )
                            ]
                        , Type.function [ Type.int ] (Type.maybe (Type.var "a"))
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "a" ]
                        )
                    )
            }
    , defaultOptions =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "V3" ]
            , name = "defaultOptions"
            , annotation =
                Just
                    (Type.record
                        [ ( "preventDefault", Type.bool )
                        , ( "stopPropagation", Type.bool )
                        ]
                    )
            }
    }


