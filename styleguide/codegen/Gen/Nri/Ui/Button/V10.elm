module Gen.Nri.Ui.Button.V10 exposing (annotation_, boundedWidth, button, call_, css, custom, danger, delete, disabled, enabled, error, exactWidth, fillContainerWidth, hideIconFor, hideIconForMobile, href, icon, id, large, link, linkExternal, linkExternalWithTracking, linkSpa, linkWithMethod, linkWithTracking, loading, medium, mobileCss, modal, moduleName_, notMobileCss, nriDescription, onClick, premium, primary, quizEngineMobileCss, secondary, small, success, tertiary, testId, toggleButton, unboundedWidth, unfulfilled, values_)

{-| 
@docs moduleName_, button, link, onClick, href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking, small, medium, large, modal, exactWidth, boundedWidth, unboundedWidth, fillContainerWidth, primary, secondary, tertiary, danger, premium, enabled, unfulfilled, disabled, error, loading, success, icon, custom, nriDescription, testId, id, hideIconForMobile, hideIconFor, css, notMobileCss, mobileCss, quizEngineMobileCss, delete, toggleButton, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Button", "V10" ]


{-| Button.button "My great button!"
        [ Button.onClick ()
        , Button.enabled
        ]

By default, the button is enabled, Medium sized, with primary colors, and an unbounded width.

button: 
    String
    -> List (Nri.Ui.Button.V10.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
button : String -> List Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Button", "V10" ]
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
        [ Elm.string buttonArg, Elm.list buttonArg0 ]


{-| Button.link "My great link!"
        [ Button.href "My href"
        , Button.secondary
        ]

By default, the link is Medium sized, with primary colors, and an unbounded width.

link: 
    String
    -> List (Nri.Ui.Button.V10.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
link : String -> List Elm.Expression -> Elm.Expression
link linkArg linkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Button", "V10" ]
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
        [ Elm.string linkArg, Elm.list linkArg0 ]


{-| onClick: msg -> Nri.Ui.Button.V10.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| href: String -> Nri.Ui.Button.V10.Attribute msg -}
href : String -> Elm.Expression
href hrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
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

linkSpa: String -> Nri.Ui.Button.V10.Attribute msg
-}
linkSpa : String -> Elm.Expression
linkSpa linkSpaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkSpaArg ]


{-| linkExternal: String -> Nri.Ui.Button.V10.Attribute msg -}
linkExternal : String -> Elm.Expression
linkExternal linkExternalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkExternalArg ]


{-| linkWithMethod: { method : String, url : String } -> Nri.Ui.Button.V10.Attribute msg -}
linkWithMethod : { method : String, url : String } -> Elm.Expression
linkWithMethod linkWithMethodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
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


{-| linkWithTracking: { track : msg, url : String } -> Nri.Ui.Button.V10.Attribute msg -}
linkWithTracking : { track : Elm.Expression, url : String } -> Elm.Expression
linkWithTracking linkWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
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


{-| linkExternalWithTracking: { track : msg, url : String } -> Nri.Ui.Button.V10.Attribute msg -}
linkExternalWithTracking :
    { track : Elm.Expression, url : String } -> Elm.Expression
linkExternalWithTracking linkExternalWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
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


{-| small: Nri.Ui.Button.V10.Attribute msg -}
small : Elm.Expression
small =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "small"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| medium: Nri.Ui.Button.V10.Attribute msg -}
medium : Elm.Expression
medium =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "medium"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| large: Nri.Ui.Button.V10.Attribute msg -}
large : Elm.Expression
large =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "large"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Alias for Button.large

modal: Nri.Ui.Button.V10.Attribute msg
-}
modal : Elm.Expression
modal =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "modal"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Define a size in `px` for the button's total width.

exactWidth: Int -> Nri.Ui.Button.V10.Attribute msg
-}
exactWidth : Int -> Elm.Expression
exactWidth exactWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int exactWidthArg ]


{-| Make a button that is at least `min` large, and which will grow with
its content up to `max`. Both bounds are inclusive (`min <= actual value <=
max`.)

boundedWidth: { min : Int, max : Int } -> Nri.Ui.Button.V10.Attribute msg
-}
boundedWidth : { min : Int, max : Int } -> Elm.Expression
boundedWidth boundedWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "boundedWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "min", Type.int ), ( "max", Type.int ) ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "min" (Elm.int boundedWidthArg.min)
            , Tuple.pair "max" (Elm.int boundedWidthArg.max)
            ]
        ]


{-| Leave the maxiumum width unbounded (there is a minimum width).

unboundedWidth: Nri.Ui.Button.V10.Attribute msg
-}
unboundedWidth : Elm.Expression
unboundedWidth =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "unboundedWidth"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| fillContainerWidth: Nri.Ui.Button.V10.Attribute msg -}
fillContainerWidth : Elm.Expression
fillContainerWidth =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "fillContainerWidth"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| primary: Nri.Ui.Button.V10.Attribute msg -}
primary : Elm.Expression
primary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "primary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| secondary: Nri.Ui.Button.V10.Attribute msg -}
secondary : Elm.Expression
secondary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "secondary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| tertiary: Nri.Ui.Button.V10.Attribute msg -}
tertiary : Elm.Expression
tertiary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "tertiary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| danger: Nri.Ui.Button.V10.Attribute msg -}
danger : Elm.Expression
danger =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "danger"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| premium: Nri.Ui.Button.V10.Attribute msg -}
premium : Elm.Expression
premium =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "premium"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| enabled: Nri.Ui.Button.V10.Attribute msg -}
enabled : Elm.Expression
enabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "enabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows inactive styles.

unfulfilled: Nri.Ui.Button.V10.Attribute msg
-}
unfulfilled : Elm.Expression
unfulfilled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "unfulfilled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows inactive styling.

If a button, this attribute will disable it as you'd expect.

If a link, this attribute will follow the pattern laid out in [Scott O'Hara's disabled links](https://www.scottohara.me/blog/2021/05/28/disabled-links.html) article,
and essentially make the anchor a disabled placeholder.

_Caveat!_

The styleguide example will NOT work correctly because of <https://github.com/elm/browser/issues/34>, which describes a problem where "a tags without href generate a navigation event".

In most cases, if you're not using Browser.application, disabled links should work just fine.

disabled: Nri.Ui.Button.V10.Attribute msg
-}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows error styling. If a button, this attribute will disable it.

error: Nri.Ui.Button.V10.Attribute msg
-}
error : Elm.Expression
error =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "error"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows loading styling. If a button, this attribute will disable it.

loading: Nri.Ui.Button.V10.Attribute msg
-}
loading : Elm.Expression
loading =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "loading"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Shows success styling. If a button, this attribute will disable it.

success: Nri.Ui.Button.V10.Attribute msg
-}
success : Elm.Expression
success =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "success"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| icon: Nri.Ui.Svg.V1.Svg -> Nri.Ui.Button.V10.Attribute msg -}
icon : Elm.Expression -> Elm.Expression
icon iconArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
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

custom: List (Accessibility.Styled.Attribute msg) -> Nri.Ui.Button.V10.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.Button.V10.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.Button.V10.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.Button.V10.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| hideIconForMobile: Nri.Ui.Button.V10.Attribute msg -}
hideIconForMobile : Elm.Expression
hideIconForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Button", "V10" ]
        , name = "hideIconForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Button", "V10" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| hideIconFor: Css.Media.MediaQuery -> Nri.Ui.Button.V10.Attribute msg -}
hideIconFor : Elm.Expression -> Elm.Expression
hideIconFor hideIconForArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ hideIconForArg ]


{-| css: List Css.Style -> Nri.Ui.Button.V10.Attribute msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.Button.V10.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.Button.V10.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.Button.V10.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


{-| DEPRECATED: this should be removed in Button.V11.

delete: { label : String, onClick : msg } -> Accessibility.Styled.Html msg
-}
delete : { label : String, onClick : Elm.Expression } -> Elm.Expression
delete deleteArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "delete"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onClick", Type.var "msg" )
                            ]
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
            [ Tuple.pair "label" (Elm.string deleteArg.label)
            , Tuple.pair "onClick" deleteArg.onClick
            ]
        ]


{-| A button that can be toggled into a pressed state and back again.

toggleButton: 
    { label : String, onSelect : msg, onDeselect : msg, pressed : Bool }
    -> Accessibility.Styled.Html msg
-}
toggleButton :
    { label : String
    , onSelect : Elm.Expression
    , onDeselect : Elm.Expression
    , pressed : Bool
    }
    -> Elm.Expression
toggleButton toggleButtonArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "toggleButton"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onSelect", Type.var "msg" )
                            , ( "onDeselect", Type.var "msg" )
                            , ( "pressed", Type.bool )
                            ]
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
            [ Tuple.pair "label" (Elm.string toggleButtonArg.label)
            , Tuple.pair "onSelect" toggleButtonArg.onSelect
            , Tuple.pair "onDeselect" toggleButtonArg.onDeselect
            , Tuple.pair "pressed" (Elm.bool toggleButtonArg.pressed)
            ]
        ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Button", "V10" ]
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
    , exactWidth : Elm.Expression -> Elm.Expression
    , boundedWidth : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , hideIconFor : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    , delete : Elm.Expression -> Elm.Expression
    , toggleButton : Elm.Expression -> Elm.Expression
    }
call_ =
    { button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Button", "V10" ]
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
                [ buttonArg, buttonArg0 ]
    , link =
        \linkArg linkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "link"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Button", "V10" ]
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
                [ linkArg, linkArg0 ]
    , onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "href"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "linkSpa"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "linkExternal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                                    [ "Nri", "Ui", "Button", "V10" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalWithTrackingArg ]
    , exactWidth =
        \exactWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "exactWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exactWidthArg ]
    , boundedWidth =
        \boundedWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "boundedWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "min", Type.int ), ( "max", Type.int ) ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ boundedWidthArg ]
    , icon =
        \iconArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
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
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Button", "V10" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    , delete =
        \deleteArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "delete"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "onClick", Type.var "msg" )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ deleteArg ]
    , toggleButton =
        \toggleButtonArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Button", "V10" ]
                    , name = "toggleButton"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "onSelect", Type.var "msg" )
                                    , ( "onDeselect", Type.var "msg" )
                                    , ( "pressed", Type.bool )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toggleButtonArg ]
    }


values_ :
    { button : Elm.Expression
    , link : Elm.Expression
    , onClick : Elm.Expression
    , href : Elm.Expression
    , linkSpa : Elm.Expression
    , linkExternal : Elm.Expression
    , linkWithMethod : Elm.Expression
    , linkWithTracking : Elm.Expression
    , linkExternalWithTracking : Elm.Expression
    , small : Elm.Expression
    , medium : Elm.Expression
    , large : Elm.Expression
    , modal : Elm.Expression
    , exactWidth : Elm.Expression
    , boundedWidth : Elm.Expression
    , unboundedWidth : Elm.Expression
    , fillContainerWidth : Elm.Expression
    , primary : Elm.Expression
    , secondary : Elm.Expression
    , tertiary : Elm.Expression
    , danger : Elm.Expression
    , premium : Elm.Expression
    , enabled : Elm.Expression
    , unfulfilled : Elm.Expression
    , disabled : Elm.Expression
    , error : Elm.Expression
    , loading : Elm.Expression
    , success : Elm.Expression
    , icon : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , hideIconForMobile : Elm.Expression
    , hideIconFor : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    , delete : Elm.Expression
    , toggleButton : Elm.Expression
    }
values_ =
    { button =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Button", "V10" ]
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
    , link =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Button", "V10" ]
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
    , onClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , href =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkSpa =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithMethod =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternalWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
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
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "small"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , medium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , large =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "large"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , modal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "modal"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , exactWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , boundedWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "boundedWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "min", Type.int ), ( "max", Type.int ) ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , unboundedWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "unboundedWidth"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , fillContainerWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "fillContainerWidth"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , primary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "primary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , secondary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "secondary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tertiary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "tertiary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , danger =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "danger"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , premium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "premium"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , enabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "enabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , unfulfilled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "unfulfilled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , error =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "error"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , loading =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "loading"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , success =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "success"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , icon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hideIconForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "hideIconForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Button", "V10" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hideIconFor =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "hideIconFor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaQuery" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Button", "V10" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , delete =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "delete"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onClick", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , toggleButton =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Button", "V10" ]
            , name = "toggleButton"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "onSelect", Type.var "msg" )
                            , ( "onDeselect", Type.var "msg" )
                            , ( "pressed", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


