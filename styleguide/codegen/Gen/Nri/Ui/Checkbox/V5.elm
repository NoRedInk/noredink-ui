module Gen.Nri.Ui.Checkbox.V5 exposing (annotation_, call_, caseOf_, checkboxLockOnInside, make_, moduleName_, selectedFromBool, values_, view, viewIcon, viewWithLabel)

{-| 
@docs moduleName_, view, viewWithLabel, selectedFromBool, viewIcon, checkboxLockOnInside, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Checkbox", "V5" ]


{-| Shows a checkbox (the label is only used for accessibility hints)

view: Nri.Ui.Checkbox.V5.Model msg -> Accessibility.Styled.Html msg
-}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewArg ]


{-| Shows a checkbox and its label text

viewWithLabel: Nri.Ui.Checkbox.V5.Model msg -> Accessibility.Styled.Html msg
-}
viewWithLabel : Elm.Expression -> Elm.Expression
viewWithLabel viewWithLabelArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "viewWithLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewWithLabelArg ]


{-| If your selectedness is always selected or not selected,
you will likely store that state as a `Bool` in your model.
`selectedFromBool` lets you easily convert that into an `IsSelected` value
for use with `Nri.Ui.Checkbox`.

selectedFromBool: Bool -> Nri.Ui.Checkbox.V5.IsSelected
-}
selectedFromBool : Bool -> Elm.Expression
selectedFromBool selectedFromBoolArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "selectedFromBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "IsSelected"
                            []
                        )
                    )
            }
        )
        [ Elm.bool selectedFromBoolArg ]


{-| viewIcon: List Css.Style -> Nri.Ui.Svg.V1.Svg -> Accessibility.Styled.Html msg -}
viewIcon : List Elm.Expression -> Elm.Expression -> Elm.Expression
viewIcon viewIconArg viewIconArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "viewIcon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list viewIconArg, viewIconArg0 ]


{-| checkboxLockOnInside: String -> Nri.Ui.Svg.V1.Svg -}
checkboxLockOnInside : String -> Elm.Expression
checkboxLockOnInside checkboxLockOnInsideArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "checkboxLockOnInside"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.string checkboxLockOnInsideArg ]


annotation_ :
    { model : Type.Annotation -> Type.Annotation
    , theme : Type.Annotation
    , isSelected : Type.Annotation
    }
annotation_ =
    { model =
        \modelArg0 ->
            Type.alias
                moduleName_
                "Model"
                [ modelArg0 ]
                (Type.record
                    [ ( "identifier", Type.string )
                    , ( "label", Type.string )
                    , ( "setterMsg"
                      , Type.function [ Type.bool ] (Type.var "msg")
                      )
                    , ( "selected"
                      , Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "IsSelected"
                            []
                      )
                    , ( "disabled", Type.bool )
                    , ( "theme"
                      , Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "Theme"
                            []
                      )
                    ]
                )
    , theme = Type.namedWith [ "Nri", "Ui", "Checkbox", "V5" ] "Theme" []
    , isSelected =
        Type.namedWith [ "Nri", "Ui", "Checkbox", "V5" ] "IsSelected" []
    }


make_ :
    { model :
        { identifier : Elm.Expression
        , label : Elm.Expression
        , setterMsg : Elm.Expression
        , selected : Elm.Expression
        , disabled : Elm.Expression
        , theme : Elm.Expression
        }
        -> Elm.Expression
    , square : Elm.Expression
    , locked : Elm.Expression
    , selected : Elm.Expression
    , notSelected : Elm.Expression
    , partiallySelected : Elm.Expression
    }
make_ =
    { model =
        \model_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Checkbox", "V5" ]
                    "Model"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "identifier", Type.string )
                        , ( "label", Type.string )
                        , ( "setterMsg"
                          , Type.function [ Type.bool ] (Type.var "msg")
                          )
                        , ( "selected"
                          , Type.namedWith
                                [ "Nri", "Ui", "Checkbox", "V5" ]
                                "IsSelected"
                                []
                          )
                        , ( "disabled", Type.bool )
                        , ( "theme"
                          , Type.namedWith
                                [ "Nri", "Ui", "Checkbox", "V5" ]
                                "Theme"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "identifier" model_args.identifier
                    , Tuple.pair "label" model_args.label
                    , Tuple.pair "setterMsg" model_args.setterMsg
                    , Tuple.pair "selected" model_args.selected
                    , Tuple.pair "disabled" model_args.disabled
                    , Tuple.pair "theme" model_args.theme
                    ]
                )
    , square =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "Square"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    , locked =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "Locked"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    , selected =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "Selected"
            , annotation = Just (Type.namedWith [] "IsSelected" [])
            }
    , notSelected =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "NotSelected"
            , annotation = Just (Type.namedWith [] "IsSelected" [])
            }
    , partiallySelected =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "PartiallySelected"
            , annotation = Just (Type.namedWith [] "IsSelected" [])
            }
    }


caseOf_ :
    { theme :
        Elm.Expression
        -> { themeTags_0_0 | square : Elm.Expression, locked : Elm.Expression }
        -> Elm.Expression
    , isSelected :
        Elm.Expression
        -> { isSelectedTags_1_0
            | selected : Elm.Expression
            , notSelected : Elm.Expression
            , partiallySelected : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { theme =
        \themeExpression themeTags ->
            Elm.Case.custom
                themeExpression
                (Type.namedWith [ "Nri", "Ui", "Checkbox", "V5" ] "Theme" [])
                [ Elm.Case.branch0 "Square" themeTags.square
                , Elm.Case.branch0 "Locked" themeTags.locked
                ]
    , isSelected =
        \isSelectedExpression isSelectedTags ->
            Elm.Case.custom
                isSelectedExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Checkbox", "V5" ]
                    "IsSelected"
                    []
                )
                [ Elm.Case.branch0 "Selected" isSelectedTags.selected
                , Elm.Case.branch0 "NotSelected" isSelectedTags.notSelected
                , Elm.Case.branch0
                    "PartiallySelected"
                    isSelectedTags.partiallySelected
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , viewWithLabel : Elm.Expression -> Elm.Expression
    , selectedFromBool : Elm.Expression -> Elm.Expression
    , viewIcon : Elm.Expression -> Elm.Expression -> Elm.Expression
    , checkboxLockOnInside : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Checkbox", "V5" ]
                                    "Model"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , viewWithLabel =
        \viewWithLabelArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
                    , name = "viewWithLabel"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Checkbox", "V5" ]
                                    "Model"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewWithLabelArg ]
    , selectedFromBool =
        \selectedFromBoolArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
                    , name = "selectedFromBool"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Checkbox", "V5" ]
                                    "IsSelected"
                                    []
                                )
                            )
                    }
                )
                [ selectedFromBoolArg ]
    , viewIcon =
        \viewIconArg viewIconArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
                    , name = "viewIcon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewIconArg, viewIconArg0 ]
    , checkboxLockOnInside =
        \checkboxLockOnInsideArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
                    , name = "checkboxLockOnInside"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ checkboxLockOnInsideArg ]
    }


values_ :
    { view : Elm.Expression
    , viewWithLabel : Elm.Expression
    , selectedFromBool : Elm.Expression
    , viewIcon : Elm.Expression
    , checkboxLockOnInside : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewWithLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "viewWithLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "Model"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , selectedFromBool =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "selectedFromBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Checkbox", "V5" ]
                            "IsSelected"
                            []
                        )
                    )
            }
    , viewIcon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "viewIcon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , checkboxLockOnInside =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Checkbox", "V5" ]
            , name = "checkboxLockOnInside"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    }


