module Gen.Nri.Ui.Confetti.V2 exposing (annotation_, burst, call_, init, moduleName_, subscriptions, update, updatePageWidth, values_, view)

{-| 
@docs moduleName_, init, view, burst, update, updatePageWidth, subscriptions, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Confetti", "V2" ]


{-| `center` An argument to Particle.withLocation that determines the horizontal center of viewport where you would like confetti to rain.

init: Float -> Nri.Ui.Confetti.V2.Model
-}
init : Float -> Elm.Expression
init initArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
        )
        [ Elm.float initArg ]


{-| view: Nri.Ui.Confetti.V2.Model -> Html.Styled.Html msg -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewArg ]


{-| `burst` BURSTS CONFETTI!!!

burst: Nri.Ui.Confetti.V2.Model -> Nri.Ui.Confetti.V2.Model
-}
burst : Elm.Expression -> Elm.Expression
burst burstArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "burst"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
        )
        [ burstArg ]


{-| update: 
    Particle.System.Msg Nri.Ui.Confetti.V2.Confetti
    -> Nri.Ui.Confetti.V2.Model
    -> Nri.Ui.Confetti.V2.Model
-}
update : Elm.Expression -> Elm.Expression -> Elm.Expression
update updateArg updateArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Particle", "System" ]
                            "Msg"
                            [ Type.namedWith
                                [ "Nri", "Ui", "Confetti", "V2" ]
                                "Confetti"
                                []
                            ]
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
        )
        [ updateArg, updateArg0 ]


{-| You will need to watch for page resize events and update the confetti model
with the new width. If you don't, your confetti will be off-center.

Why is this not part of subscriptions? Your application may already be listening
for browser resize events -- we don't want to double-up listeners unnecessarily.

updatePageWidth: Int -> Nri.Ui.Confetti.V2.Model -> Nri.Ui.Confetti.V2.Model
-}
updatePageWidth : Int -> Elm.Expression -> Elm.Expression
updatePageWidth updatePageWidthArg updatePageWidthArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "updatePageWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
        )
        [ Elm.int updatePageWidthArg, updatePageWidthArg0 ]


{-| subscriptions: 
    (Nri.Ui.Confetti.V2.Msg -> msg)
    -> Nri.Ui.Confetti.V2.Model
    -> Platform.Sub.Sub msg
-}
subscriptions :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
subscriptions subscriptionsArg subscriptionsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Nri", "Ui", "Confetti", "V2" ]
                                "Msg"
                                []
                            ]
                            (Type.var "msg")
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "subscriptionsUnpack" subscriptionsArg
        , subscriptionsArg0
        ]


annotation_ : { model : Type.Annotation, msg : Type.Annotation }
annotation_ =
    { model = Type.namedWith [ "Nri", "Ui", "Confetti", "V2" ] "Model" []
    , msg =
        Type.alias
            moduleName_
            "Msg"
            []
            (Type.namedWith
                [ "Particle", "System" ]
                "Msg"
                [ Type.namedWith [ "Nri", "Ui", "Confetti", "V2" ] "Confetti" []
                ]
            )
    }


call_ :
    { init : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , burst : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , updatePageWidth : Elm.Expression -> Elm.Expression -> Elm.Expression
    , subscriptions : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { init =
        \initArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                )
                            )
                    }
                )
                [ initArg ]
    , view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , burst =
        \burstArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "burst"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                )
                            )
                    }
                )
                [ burstArg ]
    , update =
        \updateArg updateArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "update"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Particle", "System" ]
                                    "Msg"
                                    [ Type.namedWith
                                        [ "Nri", "Ui", "Confetti", "V2" ]
                                        "Confetti"
                                        []
                                    ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                )
                            )
                    }
                )
                [ updateArg, updateArg0 ]
    , updatePageWidth =
        \updatePageWidthArg updatePageWidthArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "updatePageWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                )
                            )
                    }
                )
                [ updatePageWidthArg, updatePageWidthArg0 ]
    , subscriptions =
        \subscriptionsArg subscriptionsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
                    , name = "subscriptions"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith
                                        [ "Nri", "Ui", "Confetti", "V2" ]
                                        "Msg"
                                        []
                                    ]
                                    (Type.var "msg")
                                , Type.namedWith
                                    [ "Nri", "Ui", "Confetti", "V2" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ subscriptionsArg, subscriptionsArg0 ]
    }


values_ :
    { init : Elm.Expression
    , view : Elm.Expression
    , burst : Elm.Expression
    , update : Elm.Expression
    , updatePageWidth : Elm.Expression
    , subscriptions : Elm.Expression
    }
values_ =
    { init =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , burst =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "burst"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
    , update =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Particle", "System" ]
                            "Msg"
                            [ Type.namedWith
                                [ "Nri", "Ui", "Confetti", "V2" ]
                                "Confetti"
                                []
                            ]
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
    , updatePageWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "updatePageWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        )
                    )
            }
    , subscriptions =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Confetti", "V2" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Nri", "Ui", "Confetti", "V2" ]
                                "Msg"
                                []
                            ]
                            (Type.var "msg")
                        , Type.namedWith
                            [ "Nri", "Ui", "Confetti", "V2" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    }


