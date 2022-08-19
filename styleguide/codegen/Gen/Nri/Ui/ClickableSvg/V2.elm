module Gen.Nri.Ui.ClickableSvg.V2 exposing (annotation_, button, call_, css, custom, danger, dangerSecondary, disabled, exactHeight, exactSize, exactWidth, href, iconForMobile, id, large, link, linkExternal, linkExternalWithTracking, linkSpa, linkWithMethod, linkWithTracking, medium, mobileCss, moduleName_, notMobileCss, nriDescription, onClick, primary, quizEngineMobileCss, secondary, small, tertiary, testId, values_, withBorder)

{-| 
@docs moduleName_, button, link, onClick, href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking, exactSize, exactWidth, exactHeight, disabled, withBorder, primary, secondary, tertiary, danger, dangerSecondary, custom, nriDescription, testId, id, css, notMobileCss, mobileCss, quizEngineMobileCss, iconForMobile, small, medium, large, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "ClickableSvg", "V2" ]


{-| button: 
    String
    -> Nri.Ui.Svg.V1.Svg
    -> List (Nri.Ui.ClickableSvg.V2.Attribute msg)
    -> Html.Styled.Html msg
-}
button : String -> Elm.Expression -> List Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 buttonArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
        [ Elm.string buttonArg, buttonArg0, Elm.list buttonArg1 ]


{-| link: 
    String
    -> Nri.Ui.Svg.V1.Svg
    -> List (Nri.Ui.ClickableSvg.V2.Attribute msg)
    -> Html.Styled.Html msg
-}
link : String -> Elm.Expression -> List Elm.Expression -> Elm.Expression
link linkArg linkArg0 linkArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
        [ Elm.string linkArg, linkArg0, Elm.list linkArg1 ]


{-| onClick: msg -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| href: String -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
href : String -> Elm.Expression
href hrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
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

linkSpa: String -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
linkSpa : String -> Elm.Expression
linkSpa linkSpaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkSpaArg ]


{-| linkExternal: String -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
linkExternal : String -> Elm.Expression
linkExternal linkExternalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkExternalArg ]


{-| linkWithMethod: { method : String, url : String } -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
linkWithMethod : { method : String, url : String } -> Elm.Expression
linkWithMethod linkWithMethodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
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


{-| linkWithTracking: { track : msg, url : String } -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
linkWithTracking : { track : Elm.Expression, url : String } -> Elm.Expression
linkWithTracking linkWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
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


{-| linkExternalWithTracking: { track : msg, url : String } -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
linkExternalWithTracking :
    { track : Elm.Expression, url : String } -> Elm.Expression
linkExternalWithTracking linkExternalWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
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


{-| Set the size in `px` for the element's width and height.

Equivalent to:

    [ exactWidth inPx
    , exactHeight inPx
    ]

exactSize: Int -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
exactSize : Int -> Elm.Expression
exactSize exactSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int exactSizeArg ]


{-| Define a size in `px` for the element's total width.

exactWidth: Int -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
exactWidth : Int -> Elm.Expression
exactWidth exactWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int exactWidthArg ]


{-| Define a size in `px` for the element's total height.

exactHeight: Int -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
exactHeight : Int -> Elm.Expression
exactHeight exactHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int exactHeightArg ]


{-| disabled: Bool -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool disabledArg ]


{-| Display a border around the icon.

withBorder: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
withBorder : Elm.Expression
withBorder =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "withBorder"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| white/transparent icon on an azure background.

primary: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
primary : Elm.Expression
primary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "primary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| This is the default: a blue icon on a transparent background, or a blue icon
on a white/glacier icon with a blue border.

secondary: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
secondary : Elm.Expression
secondary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "secondary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Used to de-emphasize elements when not hovered.

tertiary: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
tertiary : Elm.Expression
tertiary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "tertiary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| White/transparent icon on a red background.

danger: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
danger : Elm.Expression
danger =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "danger"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Red icon on a white/transparent background.

dangerSecondary: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
dangerSecondary : Elm.Expression
dangerSecondary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "dangerSecondary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

custom: List (Html.Styled.Attribute msg) -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| css: List Css.Style -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Equivalent to:

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Equivalent to:

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Equivalent to:

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.ClickableSvg.V2.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


{-| iconForMobile: Nri.Ui.Svg.V1.Svg -> Nri.Ui.ClickableSvg.V2.Attribute msg -}
iconForMobile : Elm.Expression -> Elm.Expression
iconForMobile iconForMobileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "iconForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ iconForMobileArg ]


{-| This is the default. This attribute will be removed in the next version of ClickableSvg!

small: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
small : Elm.Expression
small =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "small"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| This attribute will be removed in the next version of ClickableSvg!

medium: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
medium : Elm.Expression
medium =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "medium"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| This attribute will be removed in the next version of ClickableSvg!

large: Nri.Ui.ClickableSvg.V2.Attribute msg
-}
large : Elm.Expression
large =
    Elm.value
        { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
        , name = "large"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "ClickableSvg", "V2" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { button :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , link :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , onClick : Elm.Expression -> Elm.Expression
    , href : Elm.Expression -> Elm.Expression
    , linkSpa : Elm.Expression -> Elm.Expression
    , linkExternal : Elm.Expression -> Elm.Expression
    , linkWithMethod : Elm.Expression -> Elm.Expression
    , linkWithTracking : Elm.Expression -> Elm.Expression
    , linkExternalWithTracking : Elm.Expression -> Elm.Expression
    , exactSize : Elm.Expression -> Elm.Expression
    , exactWidth : Elm.Expression -> Elm.Expression
    , exactHeight : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    , iconForMobile : Elm.Expression -> Elm.Expression
    }
call_ =
    { button =
        \buttonArg buttonArg0 buttonArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                [ buttonArg, buttonArg0, buttonArg1 ]
    , link =
        \linkArg linkArg0 linkArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "link"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                [ linkArg, linkArg0, linkArg1 ]
    , onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "href"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "linkSpa"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "linkExternal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalWithTrackingArg ]
    , exactSize =
        \exactSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "exactSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exactSizeArg ]
    , exactWidth =
        \exactWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "exactWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exactWidthArg ]
    , exactHeight =
        \exactHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "exactHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exactHeightArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "disabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disabledArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    , iconForMobile =
        \iconForMobileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
                    , name = "iconForMobile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "ClickableSvg", "V2" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ iconForMobileArg ]
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
    , exactSize : Elm.Expression
    , exactWidth : Elm.Expression
    , exactHeight : Elm.Expression
    , disabled : Elm.Expression
    , withBorder : Elm.Expression
    , primary : Elm.Expression
    , secondary : Elm.Expression
    , tertiary : Elm.Expression
    , danger : Elm.Expression
    , dangerSecondary : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    , iconForMobile : Elm.Expression
    , small : Elm.Expression
    , medium : Elm.Expression
    , large : Elm.Expression
    }
values_ =
    { button =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" []
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
    , onClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , href =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkSpa =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithMethod =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linkExternalWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , exactSize =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , exactWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , exactHeight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "exactHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , withBorder =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "withBorder"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , primary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "primary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , secondary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "secondary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tertiary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "tertiary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , danger =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "danger"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , dangerSecondary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "dangerSecondary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
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
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , iconForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "iconForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "ClickableSvg", "V2" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "small"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , medium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , large =
        Elm.value
            { importFrom = [ "Nri", "Ui", "ClickableSvg", "V2" ]
            , name = "large"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "ClickableSvg", "V2" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    }


