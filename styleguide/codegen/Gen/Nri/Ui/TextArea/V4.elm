module Gen.Nri.Ui.TextArea.V4 exposing (annotation_, call_, caseOf_, contentCreation, generateId, make_, moduleName_, values_, view, writing)

{-| 
@docs moduleName_, view, writing, contentCreation, generateId, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "TextArea", "V4" ]


{-| view: Nri.Ui.TextArea.V4.Model msg -> Html.Styled.Html msg -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
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
        [ viewArg ]


{-| Used for Writing Cycles

writing: Nri.Ui.TextArea.V4.Model msg -> Html.Styled.Html msg
-}
writing : Elm.Expression -> Elm.Expression
writing writingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "writing"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
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
        [ writingArg ]


{-| Used for Content Creation

contentCreation: Nri.Ui.TextArea.V4.Model msg -> Html.Styled.Html msg
-}
contentCreation : Elm.Expression -> Elm.Expression
contentCreation contentCreationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "contentCreation"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
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
        [ contentCreationArg ]


{-| generateId: String -> String -}
generateId : String -> Elm.Expression
generateId generateIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string generateIdArg ]


annotation_ :
    { height : Type.Annotation
    , heightBehavior : Type.Annotation
    , model : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { height = Type.namedWith [ "Nri", "Ui", "TextArea", "V4" ] "Height" []
    , heightBehavior =
        Type.namedWith [ "Nri", "Ui", "TextArea", "V4" ] "HeightBehavior" []
    , model =
        \modelArg0 ->
            Type.alias
                moduleName_
                "Model"
                [ modelArg0 ]
                (Type.record
                    [ ( "value", Type.string )
                    , ( "autofocus", Type.bool )
                    , ( "onInput"
                      , Type.function [ Type.string ] (Type.var "msg")
                      )
                    , ( "onBlur", Type.maybe (Type.var "msg") )
                    , ( "isInError", Type.bool )
                    , ( "height"
                      , Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "HeightBehavior"
                            []
                      )
                    , ( "placeholder", Type.string )
                    , ( "label", Type.string )
                    , ( "showLabel", Type.bool )
                    ]
                )
    }


make_ :
    { defaultHeight : Elm.Expression
    , singleLine : Elm.Expression
    , fixed : Elm.Expression
    , autoResize : Elm.Expression -> Elm.Expression
    , model :
        { value : Elm.Expression
        , autofocus : Elm.Expression
        , onInput : Elm.Expression
        , onBlur : Elm.Expression
        , isInError : Elm.Expression
        , height : Elm.Expression
        , placeholder : Elm.Expression
        , label : Elm.Expression
        , showLabel : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { defaultHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "DefaultHeight"
            , annotation = Just (Type.namedWith [] "Height" [])
            }
    , singleLine =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "SingleLine"
            , annotation = Just (Type.namedWith [] "Height" [])
            }
    , fixed =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "Fixed"
            , annotation = Just (Type.namedWith [] "HeightBehavior" [])
            }
    , autoResize =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
                    , name = "AutoResize"
                    , annotation = Just (Type.namedWith [] "HeightBehavior" [])
                    }
                )
                [ ar0 ]
    , model =
        \model_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "TextArea", "V4" ]
                    "Model"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "autofocus", Type.bool )
                        , ( "onInput"
                          , Type.function [ Type.string ] (Type.var "msg")
                          )
                        , ( "onBlur", Type.maybe (Type.var "msg") )
                        , ( "isInError", Type.bool )
                        , ( "height"
                          , Type.namedWith
                                [ "Nri", "Ui", "TextArea", "V4" ]
                                "HeightBehavior"
                                []
                          )
                        , ( "placeholder", Type.string )
                        , ( "label", Type.string )
                        , ( "showLabel", Type.bool )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" model_args.value
                    , Tuple.pair "autofocus" model_args.autofocus
                    , Tuple.pair "onInput" model_args.onInput
                    , Tuple.pair "onBlur" model_args.onBlur
                    , Tuple.pair "isInError" model_args.isInError
                    , Tuple.pair "height" model_args.height
                    , Tuple.pair "placeholder" model_args.placeholder
                    , Tuple.pair "label" model_args.label
                    , Tuple.pair "showLabel" model_args.showLabel
                    ]
                )
    }


caseOf_ :
    { height :
        Elm.Expression
        -> { heightTags_0_0
            | defaultHeight : Elm.Expression
            , singleLine : Elm.Expression
        }
        -> Elm.Expression
    , heightBehavior :
        Elm.Expression
        -> { heightBehaviorTags_1_0
            | fixed : Elm.Expression
            , autoResize : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { height =
        \heightExpression heightTags ->
            Elm.Case.custom
                heightExpression
                (Type.namedWith [ "Nri", "Ui", "TextArea", "V4" ] "Height" [])
                [ Elm.Case.branch0 "DefaultHeight" heightTags.defaultHeight
                , Elm.Case.branch0 "SingleLine" heightTags.singleLine
                ]
    , heightBehavior =
        \heightBehaviorExpression heightBehaviorTags ->
            Elm.Case.custom
                heightBehaviorExpression
                (Type.namedWith
                    [ "Nri", "Ui", "TextArea", "V4" ]
                    "HeightBehavior"
                    []
                )
                [ Elm.Case.branch0 "Fixed" heightBehaviorTags.fixed
                , Elm.Case.branch1
                    "AutoResize"
                    ( "nri.Ui.TextArea.V4.Height"
                    , Type.namedWith
                        [ "Nri", "Ui", "TextArea", "V4" ]
                        "Height"
                        []
                    )
                    heightBehaviorTags.autoResize
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , writing : Elm.Expression -> Elm.Expression
    , contentCreation : Elm.Expression -> Elm.Expression
    , generateId : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "TextArea", "V4" ]
                                    "Model"
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
                [ viewArg ]
    , writing =
        \writingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
                    , name = "writing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "TextArea", "V4" ]
                                    "Model"
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
                [ writingArg ]
    , contentCreation =
        \contentCreationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
                    , name = "contentCreation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "TextArea", "V4" ]
                                    "Model"
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
                [ contentCreationArg ]
    , generateId =
        \generateIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
                    , name = "generateId"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ generateIdArg ]
    }


values_ :
    { view : Elm.Expression
    , writing : Elm.Expression
    , contentCreation : Elm.Expression
    , generateId : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , writing =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "writing"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , contentCreation =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "contentCreation"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "TextArea", "V4" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , generateId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextArea", "V4" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    }


