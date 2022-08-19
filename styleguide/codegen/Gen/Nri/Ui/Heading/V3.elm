module Gen.Nri.Ui.Heading.V3 exposing (annotation_, call_, css, custom, h1, h2, h3, h4, h5, html, id, markdown, moduleName_, nriDescription, plaintext, small, subhead, testId, top, values_)

{-| 
@docs moduleName_, h1, h2, h3, h4, h5, plaintext, markdown, html, top, subhead, small, custom, css, nriDescription, testId, id, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Heading", "V3" ]


{-| Make a first-level heading (styled like a top-level heading by default.)

h1: List (Nri.Ui.Heading.V3.Attribute msg) -> Html.Styled.Html msg
-}
h1 : List Elm.Expression -> Elm.Expression
h1 h1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
        [ Elm.list h1Arg ]


{-| Make a second-level heading (styled like a tagline by default.)

h2: List (Nri.Ui.Heading.V3.Attribute msg) -> Html.Styled.Html msg
-}
h2 : List Elm.Expression -> Elm.Expression
h2 h2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
        [ Elm.list h2Arg ]


{-| Make a third-level heading (styled like a subhead by default.)

h3: List (Nri.Ui.Heading.V3.Attribute msg) -> Html.Styled.Html msg
-}
h3 : List Elm.Expression -> Elm.Expression
h3 h3Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
        [ Elm.list h3Arg ]


{-| Make a fourth-level heading (styled like a small heading by default.)

h4: List (Nri.Ui.Heading.V3.Attribute msg) -> Html.Styled.Html msg
-}
h4 : List Elm.Expression -> Elm.Expression
h4 h4Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
        [ Elm.list h4Arg ]


{-| Make a fifth-level heading (styled like a small heading by default.)

h5: List (Nri.Ui.Heading.V3.Attribute msg) -> Html.Styled.Html msg
-}
h5 : List Elm.Expression -> Elm.Expression
h5 h5Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
        [ Elm.list h5Arg ]


{-| Provide a plain-text string.

plaintext: String -> Nri.Ui.Heading.V3.Attribute msg
-}
plaintext : String -> Elm.Expression
plaintext plaintextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string plaintextArg ]


{-| Provide a string that will be rendered as markdown.

markdown: String -> Nri.Ui.Heading.V3.Attribute msg
-}
markdown : String -> Elm.Expression
markdown markdownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markdownArg ]


{-| Provide a list of custom HTML.

html: List (Html.Styled.Html msg) -> Nri.Ui.Heading.V3.Attribute msg
-}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| `top` headings are Colors.navy and have:

    font-size: 30px
    font-weight: 700

By default.

top: Nri.Ui.Heading.V3.Attribute msg
-}
top : Elm.Expression
top =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
        , name = "top"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Heading", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| `subhead` headings are Colors.navy and have:

    font-size: 20px
    font-weight: 700

By default.

subhead: Nri.Ui.Heading.V3.Attribute msg
-}
subhead : Elm.Expression
subhead =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
        , name = "subhead"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Heading", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| `small` headings are Colors.gray20 and have:

    font-size: 16px
    font-weight: 700

By default.

`small` heading default styles also make the [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing) slightly narrower, by 0.13px.

small: Nri.Ui.Heading.V3.Attribute msg
-}
small : Elm.Expression
small =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
        , name = "small"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Heading", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set some custom attributes.

Please don't make headers interactive! Use buttons or links instead so that keyboard and screen
reader users can use the site too.

For style customizations, be sure to use the Heading.css helper.

custom: List (Html.Styled.Attribute msg) -> Nri.Ui.Heading.V3.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| Set some custom CSS in this heading. For example, maybe you need to tweak
margins.

css: List Css.Style -> Nri.Ui.Heading.V3.Attribute msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| nriDescription: String -> Nri.Ui.Heading.V3.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.Heading.V3.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.Heading.V3.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Heading", "V3" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { h1 : Elm.Expression -> Elm.Expression
    , h2 : Elm.Expression -> Elm.Expression
    , h3 : Elm.Expression -> Elm.Expression
    , h4 : Elm.Expression -> Elm.Expression
    , h5 : Elm.Expression -> Elm.Expression
    , plaintext : Elm.Expression -> Elm.Expression
    , markdown : Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    }
call_ =
    { h1 =
        \h1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "h1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Heading", "V3" ]
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
                [ h1Arg ]
    , h2 =
        \h2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "h2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Heading", "V3" ]
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
                [ h2Arg ]
    , h3 =
        \h3Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "h3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Heading", "V3" ]
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
                [ h3Arg ]
    , h4 =
        \h4Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "h4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Heading", "V3" ]
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
                [ h4Arg ]
    , h5 =
        \h5Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "h5"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Heading", "V3" ]
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
                [ h5Arg ]
    , plaintext =
        \plaintextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "plaintext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "markdown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                                    [ "Nri", "Ui", "Heading", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                                    [ "Nri", "Ui", "Heading", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Heading", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    }


values_ :
    { h1 : Elm.Expression
    , h2 : Elm.Expression
    , h3 : Elm.Expression
    , h4 : Elm.Expression
    , h5 : Elm.Expression
    , plaintext : Elm.Expression
    , markdown : Elm.Expression
    , html : Elm.Expression
    , top : Elm.Expression
    , subhead : Elm.Expression
    , small : Elm.Expression
    , custom : Elm.Expression
    , css : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    }
values_ =
    { h1 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
    , h2 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
    , h3 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
    , h4 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
    , h5 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Heading", "V3" ]
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
    , plaintext =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markdown =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , top =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "top"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Heading", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , subhead =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "subhead"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Heading", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "small"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Heading", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
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
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Heading", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Heading", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


