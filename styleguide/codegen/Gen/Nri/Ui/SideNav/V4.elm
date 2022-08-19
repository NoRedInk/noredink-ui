module Gen.Nri.Ui.SideNav.V4 exposing (annotation_, call_, collapsible, css, custom, entry, entryWithChildren, href, html, icon, id, linkExternal, linkExternalWithTracking, linkSpa, linkWithMethod, linkWithTracking, make_, moduleName_, navCss, navId, navLabel, navMobileCss, navNotMobileCss, navQuizEngineMobileCss, nriDescription, onClick, premiumDisplay, primary, secondary, testId, values_, view)

{-| 
@docs moduleName_, view, collapsible, navLabel, navId, navCss, navNotMobileCss, navMobileCss, navQuizEngineMobileCss, entry, entryWithChildren, html, icon, custom, css, nriDescription, testId, id, onClick, href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking, primary, secondary, premiumDisplay, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "SideNav", "V4" ]


{-| view: 
    Nri.Ui.SideNav.V4.Config route msg
    -> List (Nri.Ui.SideNav.V4.NavAttribute msg)
    -> List (Nri.Ui.SideNav.V4.Entry route msg)
    -> Accessibility.Styled.Html msg
-}
view :
    Elm.Expression
    -> List Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 viewArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Config"
                            [ Type.var "route", Type.var "msg" ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "NavAttribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Entry"
                                [ Type.var "route", Type.var "msg" ]
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
        [ viewArg, Elm.list viewArg0, Elm.list viewArg1 ]


{-| collapsible: Nri.Ui.SideNav.V4.CollapsibleConfig msg -> Nri.Ui.SideNav.V4.NavAttribute msg -}
collapsible : Elm.Expression -> Elm.Expression
collapsible collapsibleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "collapsible"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "CollapsibleConfig"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ collapsibleArg ]


{-| Give screenreader users context on what this particular sidenav is for.

If the nav is collapsible, this value will also be used for the sidenav tooltips.

navLabel: String -> Nri.Ui.SideNav.V4.NavAttribute msg
-}
navLabel : String -> Elm.Expression
navLabel navLabelArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string navLabelArg ]


{-| navId: String -> Nri.Ui.SideNav.V4.NavAttribute msg -}
navId : String -> Elm.Expression
navId navIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string navIdArg ]


{-| These styles are included automatically in the nav container:

    [ flexBasis (px 250)
    , flexShrink (num 0)
    , borderRadius (px 8)
    , backgroundColor Colors.gray96
    , padding (px 20)
    , marginRight (px 20)
    ]

navCss: List Css.Style -> Nri.Ui.SideNav.V4.NavAttribute msg
-}
navCss : List Elm.Expression -> Elm.Expression
navCss navCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list navCssArg ]


{-| navNotMobileCss: List Css.Style -> Nri.Ui.SideNav.V4.NavAttribute msg -}
navNotMobileCss : List Elm.Expression -> Elm.Expression
navNotMobileCss navNotMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navNotMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list navNotMobileCssArg ]


{-| navMobileCss: List Css.Style -> Nri.Ui.SideNav.V4.NavAttribute msg -}
navMobileCss : List Elm.Expression -> Elm.Expression
navMobileCss navMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list navMobileCssArg ]


{-| navQuizEngineMobileCss: List Css.Style -> Nri.Ui.SideNav.V4.NavAttribute msg -}
navQuizEngineMobileCss : List Elm.Expression -> Elm.Expression
navQuizEngineMobileCss navQuizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navQuizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list navQuizEngineMobileCssArg ]


{-| entry: 
    String
    -> List (Nri.Ui.SideNav.V4.Attribute route msg)
    -> Nri.Ui.SideNav.V4.Entry route msg
-}
entry : String -> List Elm.Expression -> Elm.Expression
entry entryArg entryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "entry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Attribute"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string entryArg, Elm.list entryArg0 ]


{-| entryWithChildren: 
    String
    -> List (Nri.Ui.SideNav.V4.Attribute route msg)
    -> List (Nri.Ui.SideNav.V4.Entry route msg)
    -> Nri.Ui.SideNav.V4.Entry route msg
-}
entryWithChildren :
    String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
entryWithChildren entryWithChildrenArg entryWithChildrenArg0 entryWithChildrenArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "entryWithChildren"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Attribute"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Entry"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string entryWithChildrenArg
        , Elm.list entryWithChildrenArg0
        , Elm.list entryWithChildrenArg1
        ]


{-| html: List (Accessibility.Styled.Html msg) -> Nri.Ui.SideNav.V4.Entry route msg -}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| icon: Nri.Ui.Svg.V1.Svg -> Nri.Ui.SideNav.V4.Attribute route msg -}
icon : Elm.Expression -> Elm.Expression
icon iconArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ iconArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

custom: List (Html.Styled.Attribute msg) -> Nri.Ui.SideNav.V4.Attribute route msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| css: List Css.Style -> Nri.Ui.SideNav.V4.Attribute route msg -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| nriDescription: String -> Nri.Ui.SideNav.V4.Attribute route msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.SideNav.V4.Attribute route msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| id: String -> Nri.Ui.SideNav.V4.Attribute route msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| onClick: msg -> Nri.Ui.SideNav.V4.Attribute route msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| href: route -> Nri.Ui.SideNav.V4.Attribute route msg -}
href : Elm.Expression -> Elm.Expression
href hrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "route" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ hrefArg ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

linkSpa: route -> Nri.Ui.SideNav.V4.Attribute route msg
-}
linkSpa : Elm.Expression -> Elm.Expression
linkSpa linkSpaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "route" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ linkSpaArg ]


{-| linkExternal: String -> Nri.Ui.SideNav.V4.Attribute route msg -}
linkExternal : String -> Elm.Expression
linkExternal linkExternalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string linkExternalArg ]


{-| linkWithMethod: { method : String, url : route } -> Nri.Ui.SideNav.V4.Attribute route msg -}
linkWithMethod : { method : String, url : Elm.Expression } -> Elm.Expression
linkWithMethod linkWithMethodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkWithMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "method", Type.string )
                            , ( "url", Type.var "route" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "method" (Elm.string linkWithMethodArg.method)
            , Tuple.pair "url" linkWithMethodArg.url
            ]
        ]


{-| linkWithTracking: { track : msg, url : route } -> Nri.Ui.SideNav.V4.Attribute route msg -}
linkWithTracking :
    { track : Elm.Expression, url : Elm.Expression } -> Elm.Expression
linkWithTracking linkWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.var "route" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "track" linkWithTrackingArg.track
            , Tuple.pair "url" linkWithTrackingArg.url
            ]
        ]


{-| linkExternalWithTracking: { track : msg, url : String } -> Nri.Ui.SideNav.V4.Attribute route msg -}
linkExternalWithTracking :
    { track : Elm.Expression, url : String } -> Elm.Expression
linkExternalWithTracking linkExternalWithTrackingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "track" linkExternalWithTrackingArg.track
            , Tuple.pair "url" (Elm.string linkExternalWithTrackingArg.url)
            ]
        ]


{-| primary: Nri.Ui.SideNav.V4.Attribute route msg -}
primary : Elm.Expression
primary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
        , name = "primary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "SideNav", "V4" ]
                    "Attribute"
                    [ Type.var "route", Type.var "msg" ]
                )
        }


{-| secondary: Nri.Ui.SideNav.V4.Attribute route msg -}
secondary : Elm.Expression
secondary =
    Elm.value
        { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
        , name = "secondary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "SideNav", "V4" ]
                    "Attribute"
                    [ Type.var "route", Type.var "msg" ]
                )
        }


{-| premiumDisplay: 
    Nri.Ui.Data.PremiumDisplay.PremiumDisplay
    -> msg
    -> Nri.Ui.SideNav.V4.Attribute route msg
-}
premiumDisplay : Elm.Expression -> Elm.Expression -> Elm.Expression
premiumDisplay premiumDisplayArg premiumDisplayArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "premiumDisplay"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                            "PremiumDisplay"
                            []
                        , Type.var "msg"
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
        )
        [ premiumDisplayArg, premiumDisplayArg0 ]


annotation_ :
    { config : Type.Annotation -> Type.Annotation -> Type.Annotation
    , navAttribute : Type.Annotation -> Type.Annotation
    , entry : Type.Annotation -> Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation -> Type.Annotation
    }
annotation_ =
    { config =
        \configArg0 configArg1 ->
            Type.alias
                moduleName_
                "Config"
                [ configArg0, configArg1 ]
                (Type.record
                    [ ( "isCurrentRoute"
                      , Type.function [ Type.var "route" ] Type.bool
                      )
                    , ( "routeToString"
                      , Type.function [ Type.var "route" ] Type.string
                      )
                    , ( "onSkipNav", Type.var "msg" )
                    ]
                )
    , navAttribute =
        \navAttributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "SideNav", "V4" ]
                "NavAttribute"
                [ navAttributeArg0 ]
    , entry =
        \entryArg0 entryArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "SideNav", "V4" ]
                "Entry"
                [ entryArg0, entryArg1 ]
    , attribute =
        \attributeArg0 attributeArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "SideNav", "V4" ]
                "Attribute"
                [ attributeArg0, attributeArg1 ]
    }


make_ :
    { config :
        { isCurrentRoute : Elm.Expression
        , routeToString : Elm.Expression
        , onSkipNav : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { config =
        \config_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "SideNav", "V4" ]
                    "Config"
                    [ Type.var "route", Type.var "msg" ]
                    (Type.record
                        [ ( "isCurrentRoute"
                          , Type.function [ Type.var "route" ] Type.bool
                          )
                        , ( "routeToString"
                          , Type.function [ Type.var "route" ] Type.string
                          )
                        , ( "onSkipNav", Type.var "msg" )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "isCurrentRoute" config_args.isCurrentRoute
                    , Tuple.pair "routeToString" config_args.routeToString
                    , Tuple.pair "onSkipNav" config_args.onSkipNav
                    ]
                )
    }


call_ :
    { view :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , collapsible : Elm.Expression -> Elm.Expression
    , navLabel : Elm.Expression -> Elm.Expression
    , navId : Elm.Expression -> Elm.Expression
    , navCss : Elm.Expression -> Elm.Expression
    , navNotMobileCss : Elm.Expression -> Elm.Expression
    , navMobileCss : Elm.Expression -> Elm.Expression
    , navQuizEngineMobileCss : Elm.Expression -> Elm.Expression
    , entry : Elm.Expression -> Elm.Expression -> Elm.Expression
    , entryWithChildren :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , onClick : Elm.Expression -> Elm.Expression
    , href : Elm.Expression -> Elm.Expression
    , linkSpa : Elm.Expression -> Elm.Expression
    , linkExternal : Elm.Expression -> Elm.Expression
    , linkWithMethod : Elm.Expression -> Elm.Expression
    , linkWithTracking : Elm.Expression -> Elm.Expression
    , linkExternalWithTracking : Elm.Expression -> Elm.Expression
    , premiumDisplay : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 viewArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Config"
                                    [ Type.var "route", Type.var "msg" ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SideNav", "V4" ]
                                        "NavAttribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SideNav", "V4" ]
                                        "Entry"
                                        [ Type.var "route", Type.var "msg" ]
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
                [ viewArg, viewArg0, viewArg1 ]
    , collapsible =
        \collapsibleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "collapsible"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "CollapsibleConfig"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ collapsibleArg ]
    , navLabel =
        \navLabelArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navLabel"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navLabelArg ]
    , navId =
        \navIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navIdArg ]
    , navCss =
        \navCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navCssArg ]
    , navNotMobileCss =
        \navNotMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navNotMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navNotMobileCssArg ]
    , navMobileCss =
        \navMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navMobileCssArg ]
    , navQuizEngineMobileCss =
        \navQuizEngineMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "navQuizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "NavAttribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navQuizEngineMobileCssArg ]
    , entry =
        \entryArg entryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "entry"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SideNav", "V4" ]
                                        "Attribute"
                                        [ Type.var "route", Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Entry"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ entryArg, entryArg0 ]
    , entryWithChildren =
        \entryWithChildrenArg entryWithChildrenArg0 entryWithChildrenArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "entryWithChildren"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SideNav", "V4" ]
                                        "Attribute"
                                        [ Type.var "route", Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SideNav", "V4" ]
                                        "Entry"
                                        [ Type.var "route", Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Entry"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ entryWithChildrenArg
                , entryWithChildrenArg0
                , entryWithChildrenArg1
                ]
    , html =
        \htmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Entry"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , icon =
        \iconArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ iconArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onClickArg ]
    , href =
        \hrefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "href"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "route" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hrefArg ]
    , linkSpa =
        \linkSpaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "linkSpa"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "route" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkSpaArg ]
    , linkExternal =
        \linkExternalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "linkExternal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalArg ]
    , linkWithMethod =
        \linkWithMethodArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "linkWithMethod"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "method", Type.string )
                                    , ( "url", Type.var "route" )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkWithMethodArg ]
    , linkWithTracking =
        \linkWithTrackingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "linkWithTracking"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "track", Type.var "msg" )
                                    , ( "url", Type.var "route" )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkWithTrackingArg ]
    , linkExternalWithTracking =
        \linkExternalWithTrackingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkExternalWithTrackingArg ]
    , premiumDisplay =
        \premiumDisplayArg premiumDisplayArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
                    , name = "premiumDisplay"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                                    "PremiumDisplay"
                                    []
                                , Type.var "msg"
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SideNav", "V4" ]
                                    "Attribute"
                                    [ Type.var "route", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ premiumDisplayArg, premiumDisplayArg0 ]
    }


values_ :
    { view : Elm.Expression
    , collapsible : Elm.Expression
    , navLabel : Elm.Expression
    , navId : Elm.Expression
    , navCss : Elm.Expression
    , navNotMobileCss : Elm.Expression
    , navMobileCss : Elm.Expression
    , navQuizEngineMobileCss : Elm.Expression
    , entry : Elm.Expression
    , entryWithChildren : Elm.Expression
    , html : Elm.Expression
    , icon : Elm.Expression
    , custom : Elm.Expression
    , css : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , id : Elm.Expression
    , onClick : Elm.Expression
    , href : Elm.Expression
    , linkSpa : Elm.Expression
    , linkExternal : Elm.Expression
    , linkWithMethod : Elm.Expression
    , linkWithTracking : Elm.Expression
    , linkExternalWithTracking : Elm.Expression
    , primary : Elm.Expression
    , secondary : Elm.Expression
    , premiumDisplay : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Config"
                            [ Type.var "route", Type.var "msg" ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "NavAttribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Entry"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , collapsible =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "collapsible"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "CollapsibleConfig"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navLabel"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navNotMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navNotMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , navQuizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "navQuizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "NavAttribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , entry =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "entry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Attribute"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , entryWithChildren =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "entryWithChildren"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Attribute"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SideNav", "V4" ]
                                "Entry"
                                [ Type.var "route", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Entry"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , icon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , onClick =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , href =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "href"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "route" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , linkSpa =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkSpa"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "route" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , linkExternal =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkExternal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , linkWithMethod =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkWithMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "method", Type.string )
                            , ( "url", Type.var "route" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , linkWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "linkWithTracking"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "track", Type.var "msg" )
                            , ( "url", Type.var "route" )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , linkExternalWithTracking =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
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
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    , primary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "primary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "SideNav", "V4" ]
                        "Attribute"
                        [ Type.var "route", Type.var "msg" ]
                    )
            }
    , secondary =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "secondary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "SideNav", "V4" ]
                        "Attribute"
                        [ Type.var "route", Type.var "msg" ]
                    )
            }
    , premiumDisplay =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SideNav", "V4" ]
            , name = "premiumDisplay"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Data", "PremiumDisplay" ]
                            "PremiumDisplay"
                            []
                        , Type.var "msg"
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SideNav", "V4" ]
                            "Attribute"
                            [ Type.var "route", Type.var "msg" ]
                        )
                    )
            }
    }


