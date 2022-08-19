module Gen.Nri.Ui.ClickableText.V3 exposing (annotation_, button, call_, css, custom, hideIconFor, hideIconForMobile, hideTextFor, hideTextForMobile, href, icon, id, large, link, linkExternal, linkExternalWithTracking, linkSpa, linkWithMethod, linkWithTracking, medium, mobileCss, modal, moduleName_, notMobileCss, nriDescription, onClick, quizEngineMobileCss, small, testId, values_)

{-| 
@docs moduleName_, button, link, small, medium, large, modal, onClick, href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking, icon, custom, nriDescription, testId, id, hideIconForMobile, hideIconFor, hideTextForMobile, hideTextFor, css, notMobileCss, mobileCss, quizEngineMobileCss, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "ClickableText", "V3" ]


{-| Creates a `<button>` element

button: String -> List (Nri.Ui.ClickableText.V3.Attribute msg) -> Html.Styled.Html msg
-}
button : String -> List Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableText", "V3" ]
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
        [ Elm.string buttonArg, Elm.list buttonArg0 ]


{-| Creates a `<a>` element

link: String -> List (Nri.Ui.ClickableText.V3.Attribute msg) -> Html.Styled.Html msg
-}
link : String -> List Elm.Expression -> Elm.Expression
link linkArg linkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableText", "V3" ]
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
        [ Elm.string linkArg, Elm.list linkArg0 ]


{-| small: Nri.Ui.ClickableText.V3.Attribute msg -}
small : Elm.Expression
small =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "small"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| medium: Nri.Ui.ClickableText.V3.Attribute msg -}
medium : Elm.Expression
medium =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "medium"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| large: Nri.Ui.ClickableText.V3.Attribute msg -}
large : Elm.Expression
large =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "large"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| For use in Modal footers (adds `large` and `Css.marginTop (Css.px 15)`)

modal: Nri.Ui.ClickableText.V3.Attribute msg
-}
modal : Elm.Expression
modal =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "modal"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| onClick: msg -> Nri.Ui.ClickableText.V3.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| href: String -> Nri.Ui.ClickableText.V3.Attribute msg -}
href : String -> Elm.Expression
href hrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string hrefArg ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

linkSpa: String -> Nri.Ui.ClickableText.V3.Attribute msg
-}
linkSpa : String -> Elm.Expression
linkSpa linkSpaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkSpaArg ]


{-| linkExternal: String -> Nri.Ui.ClickableText.V3.Attribute msg -}
linkExternal : String -> Elm.Expression
linkExternal linkExternalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkExternalArg ]


{-| linkWithMethod: { method : String, url : String } -> Nri.Ui.ClickableText.V3.Attribute msg -}
linkWithMethod : { method : String, url : String } -> Elm.Expression
linkWithMethod linkWithMethodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkWithMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "method", Type.string )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "method" (Elm.string linkWithMethodArg.method)
            , Tuple.pair "url" (Elm.string linkWithMethodArg.url)
            ]
        ]


{-| linkWithTracking: { track : msg, url : String } -> Nri.Ui.ClickableText.V3.Attribute msg -}
linkWithTracking : { track : Elm.Expression, url : String } -> Elm.Expression
linkWithTracking linkWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "track" linkWithTrackingArg.track
            , Tuple.pair "url" (Elm.string linkWithTrackingArg.url)
            ]
        ]


{-| linkExternalWithTracking: { track : msg, url : String } -> Nri.Ui.ClickableText.V3.Attribute msg -}
linkExternalWithTracking :
    { track : Elm.Expression, url : String } -> Elm.Expression
linkExternalWithTracking linkExternalWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkExternalWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "track" linkExternalWithTrackingArg.track
            , Tuple.pair "url" (Elm.string linkExternalWithTrackingArg.url)
            ]
        ]


{-| icon: Nri.Ui.Svg.V1.Svg -> Nri.Ui.ClickableText.V3.Attribute msg -}
icon : Elm.Expression -> Elm.Expression
icon iconArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ iconArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

custom: List (Html.Styled.Attribute msg) -> Nri.Ui.ClickableText.V3.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
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
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.ClickableText.V3.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.ClickableText.V3.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.ClickableText.V3.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| hideIconForMobile: Nri.Ui.ClickableText.V3.Attribute msg -}
hideIconForMobile : Elm.Expression
hideIconForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "hideIconForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| hideIconFor: Css.Media.MediaQuery -> Nri.Ui.ClickableText.V3.Attribute msg -}
hideIconFor : Elm.Expression -> Elm.Expression
hideIconFor hideIconForArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ hideIconForArg ]


{-| hideTextForMobile: Nri.Ui.ClickableText.V3.Attribute msg -}
hideTextForMobile : Elm.Expression
hideTextForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
        , name = "hideTextForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableText", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| hideTextFor: Css.Media.MediaQuery -> Nri.Ui.ClickableText.V3.Attribute msg -}
hideTextFor : Elm.Expression -> Elm.Expression
hideTextFor hideTextForArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideTextFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ hideTextForArg ]


{-| css: List Css.Style -> Nri.Ui.ClickableText.V3.Attribute msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.ClickableText.V3.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.ClickableText.V3.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.ClickableText.V3.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "ClickableText", "V3" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { button : Elm.Expression -> Elm.Expression -> Elm.Expression
    , link : Elm.Expression -> Elm.Expression -> Elm.Expression
    , onClick : Elm.Expression -> Elm.Expression
    , href : Elm.Expression -> Elm.Expression
    , linkSpa : Elm.Expression -> Elm.Expression
    , linkExternal : Elm.Expression -> Elm.Expression
    , linkWithMethod : Elm.Expression -> Elm.Expression
    , linkWithTracking : Elm.Expression -> Elm.Expression
    , linkExternalWithTracking : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , hideIconFor : Elm.Expression -> Elm.Expression
    , hideTextFor : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    }
call_ =
    { button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "ClickableText", "V3" ]
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
                [ buttonArg, buttonArg0 ]
    , link =
        \linkArg linkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "link"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "ClickableText", "V3" ]
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
                [ linkArg, linkArg0 ]
    , onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onClickArg ]
    , href =
        \hrefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "href"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hrefArg ]
    , linkSpa =
        \linkSpaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "linkSpa"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkSpaArg ]
    , linkExternal =
        \linkExternalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "linkExternal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalArg ]
    , linkWithMethod =
        \linkWithMethodArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "linkWithMethod"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "method", Type.string )
                                    , ( "url", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkWithMethodArg ]
    , linkWithTracking =
        \linkWithTrackingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "linkWithTracking"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "track", Type.var "msg" )
                                    , ( "url", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkWithTrackingArg ]
    , linkExternalWithTracking =
        \linkExternalWithTrackingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "linkExternalWithTracking"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "track", Type.var "msg" )
                                    , ( "url", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalWithTrackingArg ]
    , icon =
        \iconArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
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
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
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
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
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
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hideIconForArg ]
    , hideTextFor =
        \hideTextForArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "hideTextFor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaQuery"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hideTextForArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableText", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    }


values_ :
    { button : Elm.Expression
    , link : Elm.Expression
    , small : Elm.Expression
    , medium : Elm.Expression
    , large : Elm.Expression
    , modal : Elm.Expression
    , onClick : Elm.Expression
    , href : Elm.Expression
    , linkSpa : Elm.Expression
    , linkExternal : Elm.Expression
    , linkWithMethod : Elm.Expression
    , linkWithTracking : Elm.Expression
    , linkExternalWithTracking : Elm.Expression
    , icon : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , hideIconForMobile : Elm.Expression
    , hideIconFor : Elm.Expression
    , hideTextForMobile : Elm.Expression
    , hideTextFor : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    }
values_ =
    { button =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableText", "V3" ]
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
    , link =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableText", "V3" ]
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
    , small =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "small"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , medium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , large =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "large"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , modal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "modal"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , href =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkSpa =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithMethod =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkWithMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "method", Type.string )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternalWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "linkExternalWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , icon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
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
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hideIconForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideIconForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hideIconFor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hideTextForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideTextForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableText", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hideTextFor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "hideTextFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableText", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableText", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


