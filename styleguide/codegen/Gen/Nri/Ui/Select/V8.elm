module Gen.Nri.Ui.Select.V8 exposing (annotation_, call_, choices, containerCss, custom, defaultDisplayText, disabled, errorIf, errorMessage, generateId, groupedChoices, guidance, hiddenLabel, id, loading, make_, moduleName_, noMargin, nriDescription, testId, value, values_, view, visibleLabel)

{-| 
@docs moduleName_, view, generateId, choices, groupedChoices, value, defaultDisplayText, hiddenLabel, visibleLabel, disabled, loading, errorIf, errorMessage, guidance, custom, nriDescription, id, testId, containerCss, noMargin, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Select", "V8" ]


{-| view: String -> List (Nri.Ui.Select.V8.Attribute a) -> Accessibility.Styled.Html a -}
view : String -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "Attribute"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.string viewArg, Elm.list viewArg0 ]


{-| Pass in the label to generate the default DOM element id used by a `Select.view` with the given label.

generateId: String -> String
-}
generateId : String -> Elm.Expression
generateId generateIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string generateIdArg ]


{-| choices: 
    (value -> String)
    -> List (Nri.Ui.Select.V8.Choice value)
    -> Nri.Ui.Select.V8.Attribute value
-}
choices :
    (Elm.Expression -> Elm.Expression) -> List Elm.Expression -> Elm.Expression
choices choicesArg choicesArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "choices"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "Choice"
                                [ Type.var "value" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "choicesUnpack" choicesArg, Elm.list choicesArg0 ]


{-| groupedChoices: 
    (value -> String)
    -> List (Nri.Ui.Select.V8.ChoicesGroup value)
    -> Nri.Ui.Select.V8.Attribute value
-}
groupedChoices :
    (Elm.Expression -> Elm.Expression) -> List Elm.Expression -> Elm.Expression
groupedChoices groupedChoicesArg groupedChoicesArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "groupedChoices"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "ChoicesGroup"
                                [ Type.var "value" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "groupedChoicesUnpack" groupedChoicesArg
        , Elm.list groupedChoicesArg0
        ]


{-| value: Maybe value -> Nri.Ui.Select.V8.Attribute value -}
value : Elm.Expression -> Elm.Expression
value valueArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe (Type.var "value") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ valueArg ]


{-| defaultDisplayText: String -> Nri.Ui.Select.V8.Attribute value -}
defaultDisplayText : String -> Elm.Expression
defaultDisplayText defaultDisplayTextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "defaultDisplayText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.string defaultDisplayTextArg ]


{-| Hides the visible label. (There will still be an invisible label for screen readers.)

hiddenLabel: Nri.Ui.Select.V8.Attribute value
-}
hiddenLabel : Elm.Expression
hiddenLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Select", "V8" ]
        , name = "hiddenLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Select", "V8" ]
                    "Attribute"
                    [ Type.var "value" ]
                )
        }


{-| Default behavior.

visibleLabel: Nri.Ui.Select.V8.Attribute value
-}
visibleLabel : Elm.Expression
visibleLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Select", "V8" ]
        , name = "visibleLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Select", "V8" ]
                    "Attribute"
                    [ Type.var "value" ]
                )
        }


{-| Disables the input

disabled: Nri.Ui.Select.V8.Attribute value
-}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Select", "V8" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Select", "V8" ]
                    "Attribute"
                    [ Type.var "value" ]
                )
        }


{-| Use this while the form the input is a part of is being submitted.

loading: Nri.Ui.Select.V8.Attribute value
-}
loading : Elm.Expression
loading =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Select", "V8" ]
        , name = "loading"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Select", "V8" ]
                    "Attribute"
                    [ Type.var "value" ]
                )
        }


{-| Sets whether or not the field will be highlighted as having a validation error.

If you have an error message to display, use `errorMessage` instead.

errorIf: Bool -> Nri.Ui.Select.V8.Attribute value
-}
errorIf : Bool -> Elm.Expression
errorIf errorIfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.bool errorIfArg ]


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.

errorMessage: Maybe String -> Nri.Ui.Select.V8.Attribute value
-}
errorMessage : Elm.Expression -> Elm.Expression
errorMessage errorMessageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ errorMessageArg ]


{-| A guidance message shows below the input, unless an error message is showing instead.

guidance: String -> Nri.Ui.Select.V8.Attribute value
-}
guidance : String -> Elm.Expression
guidance guidanceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.string guidanceArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Nri.Ui.Select.V8.Attribute value
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
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
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.Select.V8.Attribute value -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!

id: String -> Nri.Ui.Select.V8.Attribute value
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| testId: String -> Nri.Ui.Select.V8.Attribute value -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| Adds CSS to the element containing the input.

containerCss: List Css.Style -> Nri.Ui.Select.V8.Attribute value
-}
containerCss : List Elm.Expression -> Elm.Expression
containerCss containerCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.list containerCssArg ]


{-| Remove default spacing from the Input.

noMargin: Bool -> Nri.Ui.Select.V8.Attribute value
-}
noMargin : Bool -> Elm.Expression
noMargin noMarginArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "noMargin"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
        )
        [ Elm.bool noMarginArg ]


annotation_ :
    { choice : Type.Annotation -> Type.Annotation
    , choicesGroup : Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { choice =
        \choiceArg0 ->
            Type.alias
                moduleName_
                "Choice"
                [ choiceArg0 ]
                (Type.record
                    [ ( "label", Type.string ), ( "value", Type.var "value" ) ]
                )
    , choicesGroup =
        \choicesGroupArg0 ->
            Type.alias
                moduleName_
                "ChoicesGroup"
                [ choicesGroupArg0 ]
                (Type.record
                    [ ( "label", Type.string )
                    , ( "choices"
                      , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "Choice"
                                [ Type.var "value" ]
                            )
                      )
                    ]
                )
    , attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Select", "V8" ]
                "Attribute"
                [ attributeArg0 ]
    }


make_ :
    { choice :
        { label : Elm.Expression, value : Elm.Expression } -> Elm.Expression
    , choicesGroup :
        { label : Elm.Expression, choices : Elm.Expression } -> Elm.Expression
    }
make_ =
    { choice =
        \choice_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Select", "V8" ]
                    "Choice"
                    [ Type.var "value" ]
                    (Type.record
                        [ ( "label", Type.string )
                        , ( "value", Type.var "value" )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "label" choice_args.label
                    , Tuple.pair "value" choice_args.value
                    ]
                )
    , choicesGroup =
        \choicesGroup_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Select", "V8" ]
                    "ChoicesGroup"
                    [ Type.var "value" ]
                    (Type.record
                        [ ( "label", Type.string )
                        , ( "choices"
                          , Type.list
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Choice"
                                    [ Type.var "value" ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "label" choicesGroup_args.label
                    , Tuple.pair "choices" choicesGroup_args.choices
                    ]
                )
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , generateId : Elm.Expression -> Elm.Expression
    , choices : Elm.Expression -> Elm.Expression -> Elm.Expression
    , groupedChoices : Elm.Expression -> Elm.Expression -> Elm.Expression
    , value : Elm.Expression -> Elm.Expression
    , defaultDisplayText : Elm.Expression -> Elm.Expression
    , errorIf : Elm.Expression -> Elm.Expression
    , errorMessage : Elm.Expression -> Elm.Expression
    , guidance : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , containerCss : Elm.Expression -> Elm.Expression
    , noMargin : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Select", "V8" ]
                                        "Attribute"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , generateId =
        \generateIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "generateId"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ generateIdArg ]
    , choices =
        \choicesArg choicesArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "choices"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "value" ] Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Select", "V8" ]
                                        "Choice"
                                        [ Type.var "value" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ choicesArg, choicesArg0 ]
    , groupedChoices =
        \groupedChoicesArg groupedChoicesArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "groupedChoices"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "value" ] Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Select", "V8" ]
                                        "ChoicesGroup"
                                        [ Type.var "value" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ groupedChoicesArg, groupedChoicesArg0 ]
    , value =
        \valueArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "value"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe (Type.var "value") ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ valueArg ]
    , defaultDisplayText =
        \defaultDisplayTextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "defaultDisplayText"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ defaultDisplayTextArg ]
    , errorIf =
        \errorIfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "errorIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ errorIfArg ]
    , errorMessage =
        \errorMessageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "errorMessage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ errorMessageArg ]
    , guidance =
        \guidanceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "guidance"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ guidanceArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
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
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , containerCss =
        \containerCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "containerCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ containerCssArg ]
    , noMargin =
        \noMarginArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Select", "V8" ]
                    , name = "noMargin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Select", "V8" ]
                                    "Attribute"
                                    [ Type.var "value" ]
                                )
                            )
                    }
                )
                [ noMarginArg ]
    }


values_ :
    { view : Elm.Expression
    , generateId : Elm.Expression
    , choices : Elm.Expression
    , groupedChoices : Elm.Expression
    , value : Elm.Expression
    , defaultDisplayText : Elm.Expression
    , hiddenLabel : Elm.Expression
    , visibleLabel : Elm.Expression
    , disabled : Elm.Expression
    , loading : Elm.Expression
    , errorIf : Elm.Expression
    , errorMessage : Elm.Expression
    , guidance : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , id : Elm.Expression
    , testId : Elm.Expression
    , containerCss : Elm.Expression
    , noMargin : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "Attribute"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "a" ]
                        )
                    )
            }
    , generateId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    , choices =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "choices"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "Choice"
                                [ Type.var "value" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , groupedChoices =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "groupedChoices"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Select", "V8" ]
                                "ChoicesGroup"
                                [ Type.var "value" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , value =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe (Type.var "value") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , defaultDisplayText =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "defaultDisplayText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , hiddenLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "hiddenLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Select", "V8" ]
                        "Attribute"
                        [ Type.var "value" ]
                    )
            }
    , visibleLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "visibleLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Select", "V8" ]
                        "Attribute"
                        [ Type.var "value" ]
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Select", "V8" ]
                        "Attribute"
                        [ Type.var "value" ]
                    )
            }
    , loading =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "loading"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Select", "V8" ]
                        "Attribute"
                        [ Type.var "value" ]
                    )
            }
    , errorIf =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , errorMessage =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , guidance =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
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
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , containerCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    , noMargin =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Select", "V8" ]
            , name = "noMargin"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Select", "V8" ]
                            "Attribute"
                            [ Type.var "value" ]
                        )
                    )
            }
    }


