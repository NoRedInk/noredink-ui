module Gen.Nri.Ui.Message.V3 exposing (alert, alertDialogRole, alertRole, annotation_, banner, call_, css, custom, customTheme, error, hideIconFor, hideIconForMobile, html, httpError, icon, id, large, markdown, mobileCss, moduleName_, notMobileCss, onDismiss, plaintext, quizEngineMobileCss, somethingWentWrong, success, testId, tiny, tip, values_, view)

{-| 
@docs moduleName_, somethingWentWrong, view, icon, custom, testId, id, hideIconForMobile, hideIconFor, css, notMobileCss, mobileCss, quizEngineMobileCss, tiny, large, banner, plaintext, markdown, html, httpError, tip, error, alert, success, customTheme, alertRole, alertDialogRole, onDismiss, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Message", "V3" ]


{-| Shows an appropriate error message for when something unhandled happened.

    view maybeDetailedErrorMessage =
        viewMaybe Message.somethingWentWrong maybeDetailedErrorMessage

somethingWentWrong: String -> Accessibility.Styled.Html msg
-}
somethingWentWrong : String -> Elm.Expression
somethingWentWrong somethingWentWrongArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "somethingWentWrong"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string somethingWentWrongArg ]


{-| view =
        Message.view
            [ Message.tip
            , Message.markdown "Don't tip too much, or your waitress will **fall over**!"
            ]

view: List (Nri.Ui.Message.V3.Attribute msg) -> Accessibility.Styled.Html msg
-}
view : List Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Message", "V3" ]
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
        [ Elm.list viewArg ]


{-| icon: Nri.Ui.Svg.V1.Svg -> Nri.Ui.Message.V3.Attribute msg -}
icon : Elm.Expression -> Elm.Expression
icon iconArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ iconArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Nri.Ui.Message.V3.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| testId: String -> Nri.Ui.Message.V3.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.Message.V3.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| hideIconForMobile: Nri.Ui.Message.V3.Attribute msg -}
hideIconForMobile : Elm.Expression
hideIconForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "hideIconForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| hideIconFor: Css.Media.MediaQuery -> Nri.Ui.Message.V3.Attribute msg -}
hideIconFor : Elm.Expression -> Elm.Expression
hideIconFor hideIconForArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ hideIconForArg ]


{-| css: List Css.Style -> Nri.Ui.Message.V3.Attribute msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Equivalent to:

    Message.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.Message.V3.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Equivalent to:

    Message.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.Message.V3.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Equivalent to:

    Message.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.Message.V3.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


{-| Shows a tiny alert message. We commonly use these for validation errors and small hints to users.

    Message.view [ Message.tiny ]

This is the default size for a Message.

tiny: Nri.Ui.Message.V3.Attribute msg
-}
tiny : Elm.Expression
tiny =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "tiny"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows a large alert or callout message. We commonly use these for highlighted tips, instructions, or asides in page copy.

    Message.view [ Message.large ]

large: Nri.Ui.Message.V3.Attribute msg
-}
large : Elm.Expression
large =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "large"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows a banner alert message. This is even more prominent than `Message.large`.
We commonly use these for flash messages at the top of pages.

    Message.view [ Message.banner ]

banner: Nri.Ui.Message.V3.Attribute msg
-}
banner : Elm.Expression
banner =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "banner"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Provide a plain-text string.

plaintext: String -> Nri.Ui.Message.V3.Attribute msg
-}
plaintext : String -> Elm.Expression
plaintext plaintextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string plaintextArg ]


{-| Provide a string that will be rendered as markdown.

markdown: String -> Nri.Ui.Message.V3.Attribute msg
-}
markdown : String -> Elm.Expression
markdown markdownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markdownArg ]


{-| Provide a list of custom HTML.

html: List (Accessibility.Styled.Html msg) -> Nri.Ui.Message.V3.Attribute msg
-}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| Provide an HTTP error, which will be translated to user-friendly text.

httpError: Http.Error -> Nri.Ui.Message.V3.Attribute msg
-}
httpError : Elm.Expression -> Elm.Expression
httpError httpErrorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "httpError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Http" ] "Error" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ httpErrorArg ]


{-| This is the default theme for a Message.

tip: Nri.Ui.Message.V3.Attribute msg
-}
tip : Elm.Expression
tip =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "tip"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| error: Nri.Ui.Message.V3.Attribute msg -}
error : Elm.Expression
error =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "error"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| alert: Nri.Ui.Message.V3.Attribute msg -}
alert : Elm.Expression
alert =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "alert"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| success: Nri.Ui.Message.V3.Attribute msg -}
success : Elm.Expression
success =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "success"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| customTheme: 
    { color : Css.Color, backgroundColor : Css.Color }
    -> Nri.Ui.Message.V3.Attribute msg
-}
customTheme :
    { color : Elm.Expression, backgroundColor : Elm.Expression }
    -> Elm.Expression
customTheme customThemeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "customTheme"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "color", Type.namedWith [ "Css" ] "Color" [] )
                            , ( "backgroundColor"
                              , Type.namedWith [ "Css" ] "Color" []
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "color" customThemeArg.color
            , Tuple.pair "backgroundColor" customThemeArg.backgroundColor
            ]
        ]


{-| Use this attribute when a user's immediate attention on the Message is required.

For example, use this attribute when:

>   - An invalid value was entered into a form field
>   - The user's login session is about to expire
>   - The connection to the server was lost, local changes will not be saved

-- Excerpted from [Using the alert role MDN docs](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_alert_role)

alertRole: Nri.Ui.Message.V3.Attribute msg
-}
alertRole : Elm.Expression
alertRole =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "alertRole"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Use this attribute when (1) a user's immediate attention on the Message is required,
(2) the Message contains interactible elements, and (3) you've correctly set up the Message to be
modal (i.e., you've set up tab-wrapping, the body's overflow is hidden, the user
can't interact with elements apart from the Message's contents...)

When you use this role, verify that you are using it correctly using [this
MDN article](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_alertdialog_role).

alertDialogRole: Nri.Ui.Message.V3.Attribute msg
-}
alertDialogRole : Elm.Expression
alertDialogRole =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Message", "V3" ]
        , name = "alertDialogRole"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Message", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Adds a dismiss ("X" icon) to a message which will produce the given `msg` when clicked.

onDismiss: msg -> Nri.Ui.Message.V3.Attribute msg
-}
onDismiss : Elm.Expression -> Elm.Expression
onDismiss onDismissArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "onDismiss"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onDismissArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Message", "V3" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { somethingWentWrong : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , hideIconFor : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    , plaintext : Elm.Expression -> Elm.Expression
    , markdown : Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , httpError : Elm.Expression -> Elm.Expression
    , customTheme : Elm.Expression -> Elm.Expression
    , onDismiss : Elm.Expression -> Elm.Expression
    }
call_ =
    { somethingWentWrong =
        \somethingWentWrongArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "somethingWentWrong"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ somethingWentWrongArg ]
    , view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Message", "V3" ]
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
                [ viewArg ]
    , icon =
        \iconArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "icon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ iconArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , hideIconFor =
        \hideIconForArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "hideIconFor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaQuery"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hideIconForArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    , plaintext =
        \plaintextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "plaintext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "markdown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , httpError =
        \httpErrorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "httpError"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Http" ] "Error" [] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ httpErrorArg ]
    , customTheme =
        \customThemeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "customTheme"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "color"
                                      , Type.namedWith [ "Css" ] "Color" []
                                      )
                                    , ( "backgroundColor"
                                      , Type.namedWith [ "Css" ] "Color" []
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customThemeArg ]
    , onDismiss =
        \onDismissArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Message", "V3" ]
                    , name = "onDismiss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Message", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onDismissArg ]
    }


values_ :
    { somethingWentWrong : Elm.Expression
    , view : Elm.Expression
    , icon : Elm.Expression
    , custom : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , hideIconForMobile : Elm.Expression
    , hideIconFor : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    , tiny : Elm.Expression
    , large : Elm.Expression
    , banner : Elm.Expression
    , plaintext : Elm.Expression
    , markdown : Elm.Expression
    , html : Elm.Expression
    , httpError : Elm.Expression
    , tip : Elm.Expression
    , error : Elm.Expression
    , alert : Elm.Expression
    , success : Elm.Expression
    , customTheme : Elm.Expression
    , alertRole : Elm.Expression
    , alertDialogRole : Elm.Expression
    , onDismiss : Elm.Expression
    }
values_ =
    { somethingWentWrong =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "somethingWentWrong"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Message", "V3" ]
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
    , icon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hideIconForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "hideIconForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hideIconFor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tiny =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "tiny"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , large =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "large"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , banner =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "banner"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , plaintext =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markdown =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "markdown"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
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
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , httpError =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "httpError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Http" ] "Error" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tip =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "tip"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , error =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "error"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alert =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "alert"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , success =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "success"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , customTheme =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "customTheme"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "color", Type.namedWith [ "Css" ] "Color" [] )
                            , ( "backgroundColor"
                              , Type.namedWith [ "Css" ] "Color" []
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alertRole =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "alertRole"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alertDialogRole =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "alertDialogRole"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Message", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onDismiss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Message", "V3" ]
            , name = "onDismiss"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Message", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


