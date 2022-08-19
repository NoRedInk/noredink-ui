module Gen.Nri.Ui.Switch.V2 exposing (annotation_, call_, containerCss, custom, disabled, labelCss, moduleName_, nriDescription, onSwitch, selected, testId, values_, view)

{-| 
@docs moduleName_, view, selected, containerCss, labelCss, custom, nriDescription, testId, onSwitch, disabled, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Switch", "V2" ]


{-| Render a switch. The boolean here indicates whether the switch is on
or not.

view: 
    { label : String, id : String }
    -> List (Nri.Ui.Switch.V2.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
view : { label : String, id : String } -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string ), ( "id", Type.string ) ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Switch", "V2" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "label" (Elm.string viewArg.label)
            , Tuple.pair "id" (Elm.string viewArg.id)
            ]
        , Elm.list viewArg0
        ]


{-| What is the status of the Switch, selected or not?

selected: Bool -> Nri.Ui.Switch.V2.Attribute msg
-}
selected : Bool -> Elm.Expression
selected selectedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool selectedArg ]


{-| Adds CSS to the Switch container.

containerCss: List Css.Style -> Nri.Ui.Switch.V2.Attribute msg
-}
containerCss : List Elm.Expression -> Elm.Expression
containerCss containerCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list containerCssArg ]


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element, since it contains the icon SVG as well.

labelCss: List Css.Style -> Nri.Ui.Switch.V2.Attribute msg
-}
labelCss : List Elm.Expression -> Elm.Expression
labelCss labelCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "labelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelCssArg ]


{-| Pass custom attributes through to be attached to the underlying input.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use `containerCss` or `labelCss`.

custom: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Nri.Ui.Switch.V2.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.Switch.V2.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.Switch.V2.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| Specify what happens when the switch is toggled.

onSwitch: (Bool -> msg) -> Nri.Ui.Switch.V2.Attribute msg
-}
onSwitch : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onSwitch onSwitchArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "onSwitch"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "onSwitchUnpack" onSwitchArg ]


{-| Explicitly specify that you want this switch to be disabled. If you don't
specify `onSwitch`, this is the default, but it's provided so you don't have
to resort to `filterMap` or similar to build a clean list of attributes.

disabled: Bool -> Nri.Ui.Switch.V2.Attribute msg
-}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool disabledArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Switch", "V2" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , selected : Elm.Expression -> Elm.Expression
    , containerCss : Elm.Expression -> Elm.Expression
    , labelCss : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , onSwitch : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "id", Type.string )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Switch", "V2" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , selected =
        \selectedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "selected"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ selectedArg ]
    , containerCss =
        \containerCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "containerCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ containerCssArg ]
    , labelCss =
        \labelCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "labelCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelCssArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , onSwitch =
        \onSwitchArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "onSwitch"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.bool ] (Type.var "msg") ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onSwitchArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
                    , name = "disabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Switch", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disabledArg ]
    }


values_ :
    { view : Elm.Expression
    , selected : Elm.Expression
    , containerCss : Elm.Expression
    , labelCss : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , onSwitch : Elm.Expression
    , disabled : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string ), ( "id", Type.string ) ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Switch", "V2" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , selected =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , containerCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "labelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onSwitch =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "onSwitch"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Switch", "V2" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Switch", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


