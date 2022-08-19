module Gen.Nri.Ui.Text.V6 exposing (annotation_, call_, caption, css, custom, html, id, markdown, mediumBody, mediumBodyGray, moduleName_, noBreak, nriDescription, plaintext, smallBody, smallBodyGray, testId, ugMediumBody, ugSmallBody, values_)

{-| 
@docs moduleName_, caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray, ugMediumBody, ugSmallBody, plaintext, markdown, html, noBreak, css, id, custom, nriDescription, testId, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Text", "V6" ]


{-| This is a little note or caption.

caption: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
caption : List Elm.Expression -> Elm.Expression
caption captionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list captionArg ]


{-| This is some medium body copy.

mediumBody: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
mediumBody : List Elm.Expression -> Elm.Expression
mediumBody mediumBodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "mediumBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list mediumBodyArg ]


{-| `mediumBody`, but with a lighter gray color than the default.

mediumBodyGray: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
mediumBodyGray : List Elm.Expression -> Elm.Expression
mediumBodyGray mediumBodyGrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "mediumBodyGray"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list mediumBodyGrayArg ]


{-| This is some small body copy.

smallBody: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
smallBody : List Elm.Expression -> Elm.Expression
smallBody smallBodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "smallBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list smallBodyArg ]


{-| This is some small body copy but it's gray.

smallBodyGray: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
smallBodyGray : List Elm.Expression -> Elm.Expression
smallBodyGray smallBodyGrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "smallBodyGray"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list smallBodyGrayArg ]


{-| User-generated text.

ugMediumBody: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
ugMediumBody : List Elm.Expression -> Elm.Expression
ugMediumBody ugMediumBodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "ugMediumBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list ugMediumBodyArg ]


{-| User-generated text.

ugSmallBody: List (Nri.Ui.Text.V6.Attribute msg) -> Accessibility.Styled.Html msg
-}
ugSmallBody : List Elm.Expression -> Elm.Expression
ugSmallBody ugSmallBodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "ugSmallBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
        [ Elm.list ugSmallBodyArg ]


{-| Provide a plain-text string.

plaintext: String -> Nri.Ui.Text.V6.Attribute msg
-}
plaintext : String -> Elm.Expression
plaintext plaintextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string plaintextArg ]


{-| Provide a string that will be rendered as markdown.

markdown: String -> Nri.Ui.Text.V6.Attribute msg
-}
markdown : String -> Elm.Expression
markdown markdownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markdownArg ]


{-| Provide a list of custom HTML.

html: List (Accessibility.Styled.Html msg) -> Nri.Ui.Text.V6.Attribute msg
-}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "html"
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
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| Pass True to prevent text from ever wrapping.

The default Text behavior is `noBreak False`, which means content will wrap.

noBreak: Bool -> Nri.Ui.Text.V6.Attribute msg
-}
noBreak : Bool -> Elm.Expression
noBreak noBreakArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "noBreak"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool noBreakArg ]


{-| Add some custom CSS to the text. If you find yourself using this a lot,
please add a stricter attribute to noredink-ui!

css: List Css.Style -> Nri.Ui.Text.V6.Attribute msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| id: String -> Nri.Ui.Text.V6.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Nri.Ui.Text.V6.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
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
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.Text.V6.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.Text.V6.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Text", "V6" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { caption : Elm.Expression -> Elm.Expression
    , mediumBody : Elm.Expression -> Elm.Expression
    , mediumBodyGray : Elm.Expression -> Elm.Expression
    , smallBody : Elm.Expression -> Elm.Expression
    , smallBodyGray : Elm.Expression -> Elm.Expression
    , ugMediumBody : Elm.Expression -> Elm.Expression
    , ugSmallBody : Elm.Expression -> Elm.Expression
    , plaintext : Elm.Expression -> Elm.Expression
    , markdown : Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , noBreak : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    }
call_ =
    { caption =
        \captionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "caption"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ captionArg ]
    , mediumBody =
        \mediumBodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "mediumBody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ mediumBodyArg ]
    , mediumBodyGray =
        \mediumBodyGrayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "mediumBodyGray"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ mediumBodyGrayArg ]
    , smallBody =
        \smallBodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "smallBody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ smallBodyArg ]
    , smallBodyGray =
        \smallBodyGrayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "smallBodyGray"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ smallBodyGrayArg ]
    , ugMediumBody =
        \ugMediumBodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "ugMediumBody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ ugMediumBodyArg ]
    , ugSmallBody =
        \ugSmallBodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "ugSmallBody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Text", "V6" ]
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
                [ ugSmallBodyArg ]
    , plaintext =
        \plaintextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "plaintext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
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
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "markdown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
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
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "html"
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
                                    [ "Nri", "Ui", "Text", "V6" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , noBreak =
        \noBreakArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "noBreak"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ noBreakArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
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
                                    [ "Nri", "Ui", "Text", "V6" ]
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
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
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
                    { importFrom = [ "Nri", "Ui", "Text", "V6" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Text", "V6" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    }


values_ :
    { caption : Elm.Expression
    , mediumBody : Elm.Expression
    , mediumBodyGray : Elm.Expression
    , smallBody : Elm.Expression
    , smallBodyGray : Elm.Expression
    , ugMediumBody : Elm.Expression
    , ugSmallBody : Elm.Expression
    , plaintext : Elm.Expression
    , markdown : Elm.Expression
    , html : Elm.Expression
    , noBreak : Elm.Expression
    , css : Elm.Expression
    , id : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    }
values_ =
    { caption =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , mediumBody =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "mediumBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , mediumBodyGray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "mediumBodyGray"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , smallBody =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "smallBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , smallBodyGray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "smallBodyGray"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , ugMediumBody =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "ugMediumBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , ugSmallBody =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "ugSmallBody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Text", "V6" ]
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
    , plaintext =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markdown =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "html"
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
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , noBreak =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "noBreak"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
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
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "V6" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Text", "V6" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


