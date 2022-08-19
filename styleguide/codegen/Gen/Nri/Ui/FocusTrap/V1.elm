module Gen.Nri.Ui.FocusTrap.V1 exposing (annotation_, call_, make_, moduleName_, toAttribute, values_)

{-| 
@docs moduleName_, toAttribute, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "FocusTrap", "V1" ]


{-| Attach this attribute to add a focus trap to an HTML element.

toAttribute: Nri.Ui.FocusTrap.V1.FocusTrap msg -> Accessibility.Styled.Attribute msg
-}
toAttribute : Elm.Expression -> Elm.Expression
toAttribute toAttributeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "FocusTrap", "V1" ]
            , name = "toAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "FocusTrap", "V1" ]
                            "FocusTrap"
                            [ Type.var "msg" ]
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


annotation_ : { focusTrap : Type.Annotation -> Type.Annotation }
annotation_ =
    { focusTrap =
        \focusTrapArg0 ->
            Type.alias
                moduleName_
                "FocusTrap"
                [ focusTrapArg0 ]
                (Type.record
                    [ ( "firstId", Type.string )
                    , ( "lastId", Type.string )
                    , ( "focus"
                      , Type.function [ Type.string ] (Type.var "msg")
                      )
                    ]
                )
    }


make_ :
    { focusTrap :
        { firstId : Elm.Expression
        , lastId : Elm.Expression
        , focus : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { focusTrap =
        \focusTrap_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "FocusTrap", "V1" ]
                    "FocusTrap"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "firstId", Type.string )
                        , ( "lastId", Type.string )
                        , ( "focus"
                          , Type.function [ Type.string ] (Type.var "msg")
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "firstId" focusTrap_args.firstId
                    , Tuple.pair "lastId" focusTrap_args.lastId
                    , Tuple.pair "focus" focusTrap_args.focus
                    ]
                )
    }


call_ : { toAttribute : Elm.Expression -> Elm.Expression }
call_ =
    { toAttribute =
        \toAttributeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "FocusTrap", "V1" ]
                    , name = "toAttribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "FocusTrap", "V1" ]
                                    "FocusTrap"
                                    [ Type.var "msg" ]
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
    }


values_ : { toAttribute : Elm.Expression }
values_ =
    { toAttribute =
        Elm.value
            { importFrom = [ "Nri", "Ui", "FocusTrap", "V1" ]
            , name = "toAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "FocusTrap", "V1" ]
                            "FocusTrap"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


