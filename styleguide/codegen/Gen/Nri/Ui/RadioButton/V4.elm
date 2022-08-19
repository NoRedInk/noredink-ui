module Gen.Nri.Ui.RadioButton.V4 exposing (annotation_, call_, containerCss, custom, disabled, disclosure, enabled, errorIf, errorMessage, guidance, hiddenLabel, id, labelCss, moduleName_, nriDescription, onLockedClick, onSelect, premium, testId, values_, view, visibleLabel)

{-| 
@docs moduleName_, view, premium, onLockedClick, disclosure, onSelect, hiddenLabel, visibleLabel, containerCss, labelCss, custom, nriDescription, id, testId, disabled, enabled, errorIf, errorMessage, guidance, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "RadioButton", "V4" ]


{-| View a single radio button.

view: 
    { label : String
    , name : String
    , value : value
    , valueToString : value -> String
    , selectedValue : Maybe value
    }
    -> List (Nri.Ui.RadioButton.V4.Attribute value msg)
    -> Accessibility.Styled.Html msg
-}
view :
    { label : String
    , name : String
    , value : Elm.Expression
    , valueToString : Elm.Expression -> Elm.Expression
    , selectedValue : Elm.Expression
    }
    -> List Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "name", Type.string )
                            , ( "value", Type.var "value" )
                            , ( "valueToString"
                              , Type.function [ Type.var "value" ] Type.string
                              )
                            , ( "selectedValue", Type.maybe (Type.var "value") )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "RadioButton", "V4" ]
                                "Attribute"
                                [ Type.var "value", Type.var "msg" ]
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
            , Tuple.pair "name" (Elm.string viewArg.name)
            , Tuple.pair "value" viewArg.value
            , Tuple.pair
                "valueToString"
                (Elm.functionReduced "viewUnpack" viewArg.valueToString)
            , Tuple.pair "selectedValue" viewArg.selectedValue
            ]
        , Elm.list viewArg0
        ]


{-| Lock Premium content if the user does not have Premium.

premium: 
    Nri.Ui.Data.PremiumDisplay.PremiumDisplay
    -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
premium : Elm.Expression -> Elm.Expression
premium premiumArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
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
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ premiumArg ]


{-| Makes the Premium pennant clickable.

When the pennant is clicked, the msg that's passed in will fire.

onLockedClick: msg -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
onLockedClick : Elm.Expression -> Elm.Expression
onLockedClick onLockedClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "onLockedClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ onLockedClickArg ]


{-| Content that shows when this RadioButton is selected

disclosure: 
    List (Accessibility.Styled.Html msg)
    -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
disclosure : List Elm.Expression -> Elm.Expression
disclosure disclosureArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list disclosureArg ]


{-| Fire a message parameterized by the value type when selecting a radio option

onSelect: (value -> msg) -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
onSelect : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onSelect onSelectArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "onSelect"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "onSelectUnpack" onSelectArg ]


{-| Hides the visible label. (There will still be an invisible label for screen readers.)

hiddenLabel: Nri.Ui.RadioButton.V4.Attribute value msg
-}
hiddenLabel : Elm.Expression
hiddenLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
        , name = "hiddenLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "RadioButton", "V4" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Shows the visible label. This is the default behavior

visibleLabel: Nri.Ui.RadioButton.V4.Attribute value msg
-}
visibleLabel : Elm.Expression
visibleLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
        , name = "visibleLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "RadioButton", "V4" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Adds CSS to the element containing the input.

containerCss: List Css.Style -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
containerCss : List Elm.Expression -> Elm.Expression
containerCss containerCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list containerCssArg ]


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element, since it contains the icon SVG as well.

labelCss: List Css.Style -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
labelCss : List Elm.Expression -> Elm.Expression
labelCss labelCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "labelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelCssArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Html.Styled.Attribute Basics.Never)
    -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.RadioButton.V4.Attribute value msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one radio input with the same label on
the page. You might also use this helper if you're manually managing focus.

id: String -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| testId: String -> Nri.Ui.RadioButton.V4.Attribute value msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| This disables the input

disabled: Nri.Ui.RadioButton.V4.Attribute value msg
-}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "RadioButton", "V4" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| This enables the input, this is the default behavior

enabled: Nri.Ui.RadioButton.V4.Attribute value msg
-}
enabled : Elm.Expression
enabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
        , name = "enabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "RadioButton", "V4" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Sets whether or not the field will be highlighted as having a validation error.

errorIf: Bool -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
errorIf : Bool -> Elm.Expression
errorIf errorIfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool errorIfArg ]


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.

errorMessage: Maybe String -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
errorMessage : Elm.Expression -> Elm.Expression
errorMessage errorMessageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ errorMessageArg ]


{-| A guidance message shows below the input, unless an error message is showing instead.

guidance: String -> Nri.Ui.RadioButton.V4.Attribute value msg
-}
guidance : String -> Elm.Expression
guidance guidanceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string guidanceArg ]


annotation_ :
    { attribute : Type.Annotation -> Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 attributeArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "RadioButton", "V4" ]
                "Attribute"
                [ attributeArg0, attributeArg1 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , premium : Elm.Expression -> Elm.Expression
    , onLockedClick : Elm.Expression -> Elm.Expression
    , disclosure : Elm.Expression -> Elm.Expression
    , onSelect : Elm.Expression -> Elm.Expression
    , containerCss : Elm.Expression -> Elm.Expression
    , labelCss : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , errorIf : Elm.Expression -> Elm.Expression
    , errorMessage : Elm.Expression -> Elm.Expression
    , guidance : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "name", Type.string )
                                    , ( "value", Type.var "value" )
                                    , ( "valueToString"
                                      , Type.function
                                            [ Type.var "value" ]
                                            Type.string
                                      )
                                    , ( "selectedValue"
                                      , Type.maybe (Type.var "value")
                                      )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "RadioButton", "V4" ]
                                        "Attribute"
                                        [ Type.var "value", Type.var "msg" ]
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
    , premium =
        \premiumArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
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
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ premiumArg ]
    , onLockedClick =
        \onLockedClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "onLockedClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onLockedClickArg ]
    , disclosure =
        \disclosureArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "disclosure"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disclosureArg ]
    , onSelect =
        \onSelectArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "onSelect"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "value" ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onSelectArg ]
    , containerCss =
        \containerCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "containerCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ containerCssArg ]
    , labelCss =
        \labelCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "labelCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelCssArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , errorIf =
        \errorIfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "errorIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ errorIfArg ]
    , errorMessage =
        \errorMessageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "errorMessage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ errorMessageArg ]
    , guidance =
        \guidanceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
                    , name = "guidance"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "RadioButton", "V4" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ guidanceArg ]
    }


values_ :
    { view : Elm.Expression
    , premium : Elm.Expression
    , onLockedClick : Elm.Expression
    , disclosure : Elm.Expression
    , onSelect : Elm.Expression
    , hiddenLabel : Elm.Expression
    , visibleLabel : Elm.Expression
    , containerCss : Elm.Expression
    , labelCss : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , id : Elm.Expression
    , testId : Elm.Expression
    , disabled : Elm.Expression
    , enabled : Elm.Expression
    , errorIf : Elm.Expression
    , errorMessage : Elm.Expression
    , guidance : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "name", Type.string )
                            , ( "value", Type.var "value" )
                            , ( "valueToString"
                              , Type.function [ Type.var "value" ] Type.string
                              )
                            , ( "selectedValue", Type.maybe (Type.var "value") )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "RadioButton", "V4" ]
                                "Attribute"
                                [ Type.var "value", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , premium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
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
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , onLockedClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "onLockedClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , disclosure =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , onSelect =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "onSelect"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "value" ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , hiddenLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "hiddenLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "RadioButton", "V4" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , visibleLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "visibleLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "RadioButton", "V4" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , containerCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , labelCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "labelCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "RadioButton", "V4" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , enabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "enabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "RadioButton", "V4" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , errorIf =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , errorMessage =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , guidance =
        Elm.value
            { importFrom = [ "Nri", "Ui", "RadioButton", "V4" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "RadioButton", "V4" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    }


