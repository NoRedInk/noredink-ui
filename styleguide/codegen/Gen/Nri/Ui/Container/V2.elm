module Gen.Nri.Ui.Container.V2 exposing (annotation_, buttony, call_, css, custom, default, disabled, gray, html, id, invalid, markdown, mobileCss, moduleName_, notMobileCss, paddingPx, pillow, plaintext, quizEngineMobileCss, testId, values_, view)

{-| 
@docs moduleName_, view, custom, testId, id, css, notMobileCss, mobileCss, quizEngineMobileCss, paddingPx, plaintext, markdown, html, gray, default, disabled, invalid, pillow, buttony, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Container", "V2" ]


{-| view: List (Nri.Ui.Container.V2.Attribute msg) -> Html.Styled.Html msg -}
view : List Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Container", "V2" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list viewArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: List (Html.Styled.Attribute msg) -> Nri.Ui.Container.V2.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| testId: String -> Nri.Ui.Container.V2.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.Container.V2.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| css: List Css.Style -> Nri.Ui.Container.V2.Attribute msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Set styles that will only apply if the viewport is wider than NRI's mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.Container.V2.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Set styles that will only apply if the viewport is narrower than NRI's mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.Container.V2.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Set styles that will only apply if the viewport is narrower than NRI's quiz-engine-specific mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.Container.V2.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


{-| Changes the padding inside the container border around the content.

paddingPx: Float -> Nri.Ui.Container.V2.Attribute msg
-}
paddingPx : Float -> Elm.Expression
paddingPx paddingPxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "paddingPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float paddingPxArg ]


{-| Provide a plain-text string.

plaintext: String -> Nri.Ui.Container.V2.Attribute msg
-}
plaintext : String -> Elm.Expression
plaintext plaintextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string plaintextArg ]


{-| Provide a string that will be rendered as markdown.

Note that you may need to remove extra margin added by default
to `p` tags by user agents.

markdown: String -> Nri.Ui.Container.V2.Attribute msg
-}
markdown : String -> Elm.Expression
markdown markdownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markdownArg ]


{-| Provide a list of custom HTML.

html: List (Html.Styled.Html msg) -> Nri.Ui.Container.V2.Attribute msg
-}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| Used when there are a lot of containers.

gray: Nri.Ui.Container.V2.Attribute msg
-}
gray : Elm.Expression
gray =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "gray"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Used for the default container case.

default: Nri.Ui.Container.V2.Attribute msg
-}
default : Elm.Expression
default =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "default"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| disabled: Nri.Ui.Container.V2.Attribute msg -}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| DEPRECATED -- this will be removed in the next version of this component.

invalid: Nri.Ui.Container.V2.Attribute msg
-}
invalid : Elm.Expression
invalid =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "invalid"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Used for containers of interactive elements.

pillow: Nri.Ui.Container.V2.Attribute msg
-}
pillow : Elm.Expression
pillow =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "pillow"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Used for clickable cards

buttony: Nri.Ui.Container.V2.Attribute msg
-}
buttony : Elm.Expression
buttony =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Container", "V2" ]
        , name = "buttony"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Container", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Container", "V2" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    , paddingPx : Elm.Expression -> Elm.Expression
    , plaintext : Elm.Expression -> Elm.Expression
    , markdown : Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Container", "V2" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
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
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , notMobileCss =
        \notMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ notMobileCssArg ]
    , mobileCss =
        \mobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mobileCssArg ]
    , quizEngineMobileCss =
        \quizEngineMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    , paddingPx =
        \paddingPxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "paddingPx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paddingPxArg ]
    , plaintext =
        \plaintextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "plaintext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ plaintextArg ]
    , markdown =
        \markdownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "markdown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markdownArg ]
    , html =
        \htmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Container", "V2" ]
                    , name = "html"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Container", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    }


values_ :
    { view : Elm.Expression
    , custom : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    , paddingPx : Elm.Expression
    , plaintext : Elm.Expression
    , markdown : Elm.Expression
    , html : Elm.Expression
    , gray : Elm.Expression
    , default : Elm.Expression
    , disabled : Elm.Expression
    , invalid : Elm.Expression
    , pillow : Elm.Expression
    , buttony : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Container", "V2" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , paddingPx =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "paddingPx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , plaintext =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markdown =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Container", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , gray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "gray"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , default =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "default"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , invalid =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , pillow =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "pillow"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , buttony =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Container", "V2" ]
            , name = "buttony"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Container", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    }


