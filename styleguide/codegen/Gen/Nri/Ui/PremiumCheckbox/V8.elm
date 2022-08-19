module Gen.Nri.Ui.PremiumCheckbox.V8 exposing (annotation_, call_, disabled, enabled, id, moduleName_, onLockedClick, partiallySelected, premium, selected, setCheckboxContainerCss, setCheckboxDisabledLabelCss, setCheckboxEnabledLabelCss, values_, view)

{-| 
@docs moduleName_, view, selected, partiallySelected, premium, onLockedClick, disabled, enabled, id, setCheckboxContainerCss, setCheckboxEnabledLabelCss, setCheckboxDisabledLabelCss, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]


{-| view: 
    { label : String, onChange : Bool -> msg }
    -> List (Nri.Ui.PremiumCheckbox.V8.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
view :
    { label : String, onChange : Elm.Expression -> Elm.Expression }
    -> List Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onChange"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
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
            , Tuple.pair
                "onChange"
                (Elm.functionReduced "viewUnpack" viewArg.onChange)
            ]
        , Elm.list viewArg0
        ]


{-| selected: Bool -> Nri.Ui.PremiumCheckbox.V8.Attribute msg -}
selected : Bool -> Elm.Expression
selected selectedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool selectedArg ]


{-| partiallySelected: Nri.Ui.PremiumCheckbox.V8.Attribute msg -}
partiallySelected : Elm.Expression
partiallySelected =
    Elm.value
        { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
        , name = "partiallySelected"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Lock Premium content if the user does not have Premium.

premium: 
    Nri.Ui.Data.PremiumDisplay.PremiumDisplay
    -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
premium : Elm.Expression -> Elm.Expression
premium premiumArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "premium"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                            "PremiumDisplay"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ premiumArg ]


{-| Show Premium pennant on Premium content.

When a locked premium checkbox is clicked, the msg that's passed in will fire.

onLockedClick: msg -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
onLockedClick : Elm.Expression -> Elm.Expression
onLockedClick onLockedClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "onLockedClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onLockedClickArg ]


{-| This disables the input

disabled: Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| This enables the input, this is the default behavior

enabled: Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
enabled : Elm.Expression
enabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
        , name = "enabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set a custom ID for this checkbox and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one checkbox with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!

id: String -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| Set custom CSS for the checkbox container

setCheckboxContainerCss: List Css.Style -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
setCheckboxContainerCss : List Elm.Expression -> Elm.Expression
setCheckboxContainerCss setCheckboxContainerCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxContainerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list setCheckboxContainerCssArg ]


{-| Set custom CSS for the enabled checkbox label

setCheckboxEnabledLabelCss: List Css.Style -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
setCheckboxEnabledLabelCss : List Elm.Expression -> Elm.Expression
setCheckboxEnabledLabelCss setCheckboxEnabledLabelCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxEnabledLabelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list setCheckboxEnabledLabelCssArg ]


{-| Set custom CSS for the disabled checkbox label

setCheckboxDisabledLabelCss: List Css.Style -> Nri.Ui.PremiumCheckbox.V8.Attribute msg
-}
setCheckboxDisabledLabelCss : List Elm.Expression -> Elm.Expression
setCheckboxDisabledLabelCss setCheckboxDisabledLabelCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxDisabledLabelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list setCheckboxDisabledLabelCssArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , selected : Elm.Expression -> Elm.Expression
    , premium : Elm.Expression -> Elm.Expression
    , onLockedClick : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , setCheckboxContainerCss : Elm.Expression -> Elm.Expression
    , setCheckboxEnabledLabelCss : Elm.Expression -> Elm.Expression
    , setCheckboxDisabledLabelCss : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "onChange"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.var "msg")
                                      )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
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
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "selected"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ selectedArg ]
    , premium =
        \premiumArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "premium"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                                    "PremiumDisplay"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ premiumArg ]
    , onLockedClick =
        \onLockedClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "onLockedClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onLockedClickArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , setCheckboxContainerCss =
        \setCheckboxContainerCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "setCheckboxContainerCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ setCheckboxContainerCssArg ]
    , setCheckboxEnabledLabelCss =
        \setCheckboxEnabledLabelCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "setCheckboxEnabledLabelCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ setCheckboxEnabledLabelCssArg ]
    , setCheckboxDisabledLabelCss =
        \setCheckboxDisabledLabelCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                    , name = "setCheckboxDisabledLabelCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ setCheckboxDisabledLabelCssArg ]
    }


values_ :
    { view : Elm.Expression
    , selected : Elm.Expression
    , partiallySelected : Elm.Expression
    , premium : Elm.Expression
    , onLockedClick : Elm.Expression
    , disabled : Elm.Expression
    , enabled : Elm.Expression
    , id : Elm.Expression
    , setCheckboxContainerCss : Elm.Expression
    , setCheckboxEnabledLabelCss : Elm.Expression
    , setCheckboxDisabledLabelCss : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onChange"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
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
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , partiallySelected =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "partiallySelected"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , premium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "premium"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                            "PremiumDisplay"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onLockedClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "onLockedClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , enabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "enabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , setCheckboxContainerCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxContainerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , setCheckboxEnabledLabelCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxEnabledLabelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , setCheckboxDisabledLabelCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
            , name = "setCheckboxDisabledLabelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "PremiumCheckbox", "V8" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


