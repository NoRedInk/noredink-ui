module Gen.Nri.Ui.WhenFocusLeaves.V1 exposing (call_, moduleName_, toAttribute, toDecoder, values_)

{-| 
@docs moduleName_, toAttribute, toDecoder, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]


{-| Attach this attribute to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

NOTE: When needing to listen to multiple keys toDecoder should be used instead of toAttribute.

toAttribute: 
    { firstId : String
    , lastId : String
    , tabBackAction : msg
    , tabForwardAction : msg
    }
    -> Accessibility.Styled.Attribute msg
-}
toAttribute :
    { firstId : String
    , lastId : String
    , tabBackAction : Elm.Expression
    , tabForwardAction : Elm.Expression
    }
    -> Elm.Expression
toAttribute toAttributeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
            , name = "toAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "firstId", Type.string )
                            , ( "lastId", Type.string )
                            , ( "tabBackAction", Type.var "msg" )
                            , ( "tabForwardAction", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "firstId" (Elm.string toAttributeArg.firstId)
            , Tuple.pair "lastId" (Elm.string toAttributeArg.lastId)
            , Tuple.pair "tabBackAction" toAttributeArg.tabBackAction
            , Tuple.pair "tabForwardAction" toAttributeArg.tabForwardAction
            ]
        ]


{-| Use this decoder to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

NOTE: When needing to listen to multiple keys toDecoder should be used instead of toAttribute.

    import Accessibility.Styled.Key as Key

    Key.onKeyDown
        [ Key.escape CloseModal
        , toDecoder config
        ]

toDecoder: 
    { firstId : String
    , lastId : String
    , tabBackAction : msg
    , tabForwardAction : msg
    }
    -> Json.Decode.Decoder msg
-}
toDecoder :
    { firstId : String
    , lastId : String
    , tabBackAction : Elm.Expression
    , tabForwardAction : Elm.Expression
    }
    -> Elm.Expression
toDecoder toDecoderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
            , name = "toDecoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "firstId", Type.string )
                            , ( "lastId", Type.string )
                            , ( "tabBackAction", Type.var "msg" )
                            , ( "tabForwardAction", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "firstId" (Elm.string toDecoderArg.firstId)
            , Tuple.pair "lastId" (Elm.string toDecoderArg.lastId)
            , Tuple.pair "tabBackAction" toDecoderArg.tabBackAction
            , Tuple.pair "tabForwardAction" toDecoderArg.tabForwardAction
            ]
        ]


call_ :
    { toAttribute : Elm.Expression -> Elm.Expression
    , toDecoder : Elm.Expression -> Elm.Expression
    }
call_ =
    { toAttribute =
        \toAttributeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
                    , name = "toAttribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "firstId", Type.string )
                                    , ( "lastId", Type.string )
                                    , ( "tabBackAction", Type.var "msg" )
                                    , ( "tabForwardAction", Type.var "msg" )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toAttributeArg ]
    , toDecoder =
        \toDecoderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
                    , name = "toDecoder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "firstId", Type.string )
                                    , ( "lastId", Type.string )
                                    , ( "tabBackAction", Type.var "msg" )
                                    , ( "tabForwardAction", Type.var "msg" )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toDecoderArg ]
    }


values_ : { toAttribute : Elm.Expression, toDecoder : Elm.Expression }
values_ =
    { toAttribute =
        Elm.value
            { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
            , name = "toAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "firstId", Type.string )
                            , ( "lastId", Type.string )
                            , ( "tabBackAction", Type.var "msg" )
                            , ( "tabForwardAction", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , toDecoder =
        Elm.value
            { importFrom = [ "Nri", "Ui", "WhenFocusLeaves", "V1" ]
            , name = "toDecoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "firstId", Type.string )
                            , ( "lastId", Type.string )
                            , ( "tabBackAction", Type.var "msg" )
                            , ( "tabForwardAction", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


