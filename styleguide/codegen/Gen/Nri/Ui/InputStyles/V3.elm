module Gen.Nri.Ui.InputStyles.V3 exposing (annotation_, call_, caseOf_, defaultMarginTop, errorClass, focusedErrorInputBoxShadow, focusedInputBoxShadow, input, inputClass, inputLineHeight, inputPaddingVertical, label, make_, moduleName_, textAreaHeight, values_, writingLineHeight, writingMinHeight, writingPadding, writingPaddingTop)

{-| 
@docs moduleName_, label, input, inputPaddingVertical, inputLineHeight, textAreaHeight, writingLineHeight, writingPadding, writingPaddingTop, writingMinHeight, defaultMarginTop, focusedInputBoxShadow, focusedErrorInputBoxShadow, errorClass, inputClass, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "InputStyles", "V3" ]


{-| label: Nri.Ui.InputStyles.V3.Theme -> Bool -> Css.Style -}
label : Elm.Expression -> Bool -> Elm.Expression
label labelArg labelArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "InputStyles", "V3" ]
                            "Theme"
                            []
                        , Type.bool
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ labelArg, Elm.bool labelArg0 ]


{-| In order to use these styles in an input module, you will need to add the class "override-sass-styles". This is because sass styles in the monolith have higher precendence than the class styles here.

input: Nri.Ui.InputStyles.V3.Theme -> Css.Style
-}
input : Elm.Expression -> Elm.Expression
input inputArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "InputStyles", "V3" ]
                            "Theme"
                            []
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ inputArg ]


{-| inputPaddingVertical: Css.Px -}
inputPaddingVertical : Elm.Expression
inputPaddingVertical =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "inputPaddingVertical"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| inputLineHeight: Css.Px -}
inputLineHeight : Elm.Expression
inputLineHeight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "inputLineHeight"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| textAreaHeight: Css.Px -}
textAreaHeight : Elm.Expression
textAreaHeight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "textAreaHeight"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| writingLineHeight: Css.Px -}
writingLineHeight : Elm.Expression
writingLineHeight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "writingLineHeight"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| writingPadding: Css.Px -}
writingPadding : Elm.Expression
writingPadding =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "writingPadding"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| writingPaddingTop: Css.Px -}
writingPaddingTop : Elm.Expression
writingPaddingTop =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "writingPaddingTop"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| writingMinHeight: Css.Px -}
writingMinHeight : Elm.Expression
writingMinHeight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "writingMinHeight"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| defaultMarginTop: Float -}
defaultMarginTop : Elm.Expression
defaultMarginTop =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "defaultMarginTop"
        , annotation = Just Type.float
        }


{-| focusedInputBoxShadow: String -}
focusedInputBoxShadow : Elm.Expression
focusedInputBoxShadow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "focusedInputBoxShadow"
        , annotation = Just Type.string
        }


{-| focusedErrorInputBoxShadow: String -}
focusedErrorInputBoxShadow : Elm.Expression
focusedErrorInputBoxShadow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "focusedErrorInputBoxShadow"
        , annotation = Just Type.string
        }


{-| errorClass: String -}
errorClass : Elm.Expression
errorClass =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "errorClass"
        , annotation = Just Type.string
        }


{-| inputClass: String -}
inputClass : Elm.Expression
inputClass =
    Elm.value
        { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
        , name = "inputClass"
        , annotation = Just Type.string
        }


annotation_ : { theme : Type.Annotation }
annotation_ =
    { theme = Type.namedWith [ "Nri", "Ui", "InputStyles", "V3" ] "Theme" [] }


make_ :
    { contentCreation : Elm.Expression
    , standard : Elm.Expression
    , userGenerated : Elm.Expression
    , writing : Elm.Expression
    }
make_ =
    { contentCreation =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "ContentCreation"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    , standard =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "Standard"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    , userGenerated =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "UserGenerated"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    , writing =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "Writing"
            , annotation = Just (Type.namedWith [] "Theme" [])
            }
    }


caseOf_ :
    { theme :
        Elm.Expression
        -> { themeTags_0_0
            | contentCreation : Elm.Expression
            , standard : Elm.Expression
            , userGenerated : Elm.Expression
            , writing : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { theme =
        \themeExpression themeTags ->
            Elm.Case.custom
                themeExpression
                (Type.namedWith [ "Nri", "Ui", "InputStyles", "V3" ] "Theme" [])
                [ Elm.Case.branch0 "ContentCreation" themeTags.contentCreation
                , Elm.Case.branch0 "Standard" themeTags.standard
                , Elm.Case.branch0 "UserGenerated" themeTags.userGenerated
                , Elm.Case.branch0 "Writing" themeTags.writing
                ]
    }


call_ :
    { label : Elm.Expression -> Elm.Expression -> Elm.Expression
    , input : Elm.Expression -> Elm.Expression
    }
call_ =
    { label =
        \labelArg labelArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
                    , name = "label"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "InputStyles", "V3" ]
                                    "Theme"
                                    []
                                , Type.bool
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ labelArg, labelArg0 ]
    , input =
        \inputArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
                    , name = "input"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "InputStyles", "V3" ]
                                    "Theme"
                                    []
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ inputArg ]
    }


values_ :
    { label : Elm.Expression
    , input : Elm.Expression
    , inputPaddingVertical : Elm.Expression
    , inputLineHeight : Elm.Expression
    , textAreaHeight : Elm.Expression
    , writingLineHeight : Elm.Expression
    , writingPadding : Elm.Expression
    , writingPaddingTop : Elm.Expression
    , writingMinHeight : Elm.Expression
    , defaultMarginTop : Elm.Expression
    , focusedInputBoxShadow : Elm.Expression
    , focusedErrorInputBoxShadow : Elm.Expression
    , errorClass : Elm.Expression
    , inputClass : Elm.Expression
    }
values_ =
    { label =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "InputStyles", "V3" ]
                            "Theme"
                            []
                        , Type.bool
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , input =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "InputStyles", "V3" ]
                            "Theme"
                            []
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , inputPaddingVertical =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "inputPaddingVertical"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , inputLineHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "inputLineHeight"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , textAreaHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "textAreaHeight"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , writingLineHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "writingLineHeight"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , writingPadding =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "writingPadding"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , writingPaddingTop =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "writingPaddingTop"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , writingMinHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "writingMinHeight"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , defaultMarginTop =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "defaultMarginTop"
            , annotation = Just Type.float
            }
    , focusedInputBoxShadow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "focusedInputBoxShadow"
            , annotation = Just Type.string
            }
    , focusedErrorInputBoxShadow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "focusedErrorInputBoxShadow"
            , annotation = Just Type.string
            }
    , errorClass =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "errorClass"
            , annotation = Just Type.string
            }
    , inputClass =
        Elm.value
            { importFrom = [ "Nri", "Ui", "InputStyles", "V3" ]
            , name = "inputClass"
            , annotation = Just Type.string
            }
    }


