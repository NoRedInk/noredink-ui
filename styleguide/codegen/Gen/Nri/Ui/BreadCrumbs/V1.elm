module Gen.Nri.Ui.BreadCrumbs.V1 exposing (after, annotation_, call_, caseOf_, headerId, init, make_, moduleName_, toPageTitle, toPageTitleWithSecondaryBreadCrumbs, values_, view)

{-| 
@docs moduleName_, view, init, after, headerId, toPageTitle, toPageTitleWithSecondaryBreadCrumbs, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "BreadCrumbs", "V1" ]


{-| Usually, the label value will be the string "breadcrumbs".

It's configurable so that if more than one set of BreadCrumbs ever appear on the page, the aria-label for the nav can still be unique.

view: 
    { aTagAttributes : route -> List (Accessibility.Styled.Attribute msg)
    , isCurrentRoute : route -> Bool
    , label : String
    }
    -> Nri.Ui.BreadCrumbs.V1.BreadCrumbs route
    -> Accessibility.Styled.Html msg
-}
view :
    { aTagAttributes : Elm.Expression -> Elm.Expression
    , isCurrentRoute : Elm.Expression -> Elm.Expression
    , label : String
    }
    -> Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "aTagAttributes"
                              , Type.function
                                    [ Type.var "route" ]
                                    (Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "isCurrentRoute"
                              , Type.function [ Type.var "route" ] Type.bool
                              )
                            , ( "label", Type.string )
                            ]
                        , Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
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
            [ Tuple.pair
                "aTagAttributes"
                (Elm.functionReduced "viewUnpack" viewArg.aTagAttributes)
            , Tuple.pair
                "isCurrentRoute"
                (Elm.functionReduced "viewUnpack" viewArg.isCurrentRoute)
            , Tuple.pair "label" (Elm.string viewArg.label)
            ]
        , viewArg0
        ]


{-| init: 
    Nri.Ui.BreadCrumbs.V1.BreadCrumb route
    -> Nri.Ui.BreadCrumbs.V1.BreadCrumbs route
-}
init : Elm.Expression -> Elm.Expression
init initArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumb"
                            [ Type.var "route" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        )
                    )
            }
        )
        [ initArg ]


{-| after: 
    Nri.Ui.BreadCrumbs.V1.BreadCrumbs route
    -> Nri.Ui.BreadCrumbs.V1.BreadCrumb route
    -> Nri.Ui.BreadCrumbs.V1.BreadCrumbs route
-}
after : Elm.Expression -> Elm.Expression -> Elm.Expression
after afterArg afterArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "after"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumb"
                            [ Type.var "route" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        )
                    )
            }
        )
        [ afterArg, afterArg0 ]


{-| headerId: Nri.Ui.BreadCrumbs.V1.BreadCrumbs route -> String -}
headerId : Elm.Expression -> Elm.Expression
headerId headerIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "headerId"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        ]
                        Type.string
                    )
            }
        )
        [ headerIdArg ]


{-| Generate an HTML page title using the breadcrumbs,
in the form "Sub-Category | Category | NoRedInk" for breadCrumbs like:

    Category > Sub - Category

toPageTitle: Nri.Ui.BreadCrumbs.V1.BreadCrumbs a -> String
-}
toPageTitle : Elm.Expression -> Elm.Expression
toPageTitle toPageTitleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "toPageTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
        )
        [ toPageTitleArg ]


{-| toPageTitleWithSecondaryBreadCrumbs: Nri.Ui.BreadCrumbs.V1.BreadCrumbs a -> String -}
toPageTitleWithSecondaryBreadCrumbs : Elm.Expression -> Elm.Expression
toPageTitleWithSecondaryBreadCrumbs toPageTitleWithSecondaryBreadCrumbsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "toPageTitleWithSecondaryBreadCrumbs"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
        )
        [ toPageTitleWithSecondaryBreadCrumbsArg ]


annotation_ :
    { iconStyle : Type.Annotation
    , breadCrumbs : Type.Annotation -> Type.Annotation
    , breadCrumb : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { iconStyle =
        Type.namedWith [ "Nri", "Ui", "BreadCrumbs", "V1" ] "IconStyle" []
    , breadCrumbs =
        \breadCrumbsArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                "BreadCrumbs"
                [ breadCrumbsArg0 ]
    , breadCrumb =
        \breadCrumbArg0 ->
            Type.alias
                moduleName_
                "BreadCrumb"
                [ breadCrumbArg0 ]
                (Type.record
                    [ ( "icon"
                      , Type.maybe
                            (Type.namedWith
                                [ "Nri", "Ui", "Svg", "V1" ]
                                "Svg"
                                []
                            )
                      )
                    , ( "iconStyle"
                      , Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "IconStyle"
                            []
                      )
                    , ( "id", Type.string )
                    , ( "text", Type.string )
                    , ( "route", Type.var "route" )
                    ]
                )
    }


make_ :
    { circled : Elm.Expression
    , default : Elm.Expression
    , breadCrumb :
        { icon : Elm.Expression
        , iconStyle : Elm.Expression
        , id : Elm.Expression
        , text : Elm.Expression
        , route : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { circled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "Circled"
            , annotation = Just (Type.namedWith [] "IconStyle" [])
            }
    , default =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "Default"
            , annotation = Just (Type.namedWith [] "IconStyle" [])
            }
    , breadCrumb =
        \breadCrumb_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    "BreadCrumb"
                    [ Type.var "route" ]
                    (Type.record
                        [ ( "icon"
                          , Type.maybe
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                          )
                        , ( "iconStyle"
                          , Type.namedWith
                                [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                "IconStyle"
                                []
                          )
                        , ( "id", Type.string )
                        , ( "text", Type.string )
                        , ( "route", Type.var "route" )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "icon" breadCrumb_args.icon
                    , Tuple.pair "iconStyle" breadCrumb_args.iconStyle
                    , Tuple.pair "id" breadCrumb_args.id
                    , Tuple.pair "text" breadCrumb_args.text
                    , Tuple.pair "route" breadCrumb_args.route
                    ]
                )
    }


caseOf_ :
    { iconStyle :
        Elm.Expression
        -> { iconStyleTags_0_0
            | circled : Elm.Expression
            , default : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { iconStyle =
        \iconStyleExpression iconStyleTags ->
            Elm.Case.custom
                iconStyleExpression
                (Type.namedWith
                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    "IconStyle"
                    []
                )
                [ Elm.Case.branch0 "Circled" iconStyleTags.circled
                , Elm.Case.branch0 "Default" iconStyleTags.default
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , init : Elm.Expression -> Elm.Expression
    , after : Elm.Expression -> Elm.Expression -> Elm.Expression
    , headerId : Elm.Expression -> Elm.Expression
    , toPageTitle : Elm.Expression -> Elm.Expression
    , toPageTitleWithSecondaryBreadCrumbs : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "aTagAttributes"
                                      , Type.function
                                            [ Type.var "route" ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Accessibility"
                                                    , "Styled"
                                                    ]
                                                    "Attribute"
                                                    [ Type.var "msg" ]
                                                )
                                            )
                                      )
                                    , ( "isCurrentRoute"
                                      , Type.function
                                            [ Type.var "route" ]
                                            Type.bool
                                      )
                                    , ( "label", Type.string )
                                    ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "route" ]
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
    , init =
        \initArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumb"
                                    [ Type.var "route" ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "route" ]
                                )
                            )
                    }
                )
                [ initArg ]
    , after =
        \afterArg afterArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "after"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "route" ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumb"
                                    [ Type.var "route" ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "route" ]
                                )
                            )
                    }
                )
                [ afterArg, afterArg0 ]
    , headerId =
        \headerIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "headerId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "route" ]
                                ]
                                Type.string
                            )
                    }
                )
                [ headerIdArg ]
    , toPageTitle =
        \toPageTitleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "toPageTitle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "a" ]
                                ]
                                Type.string
                            )
                    }
                )
                [ toPageTitleArg ]
    , toPageTitleWithSecondaryBreadCrumbs =
        \toPageTitleWithSecondaryBreadCrumbsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                    , name = "toPageTitleWithSecondaryBreadCrumbs"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                                    "BreadCrumbs"
                                    [ Type.var "a" ]
                                ]
                                Type.string
                            )
                    }
                )
                [ toPageTitleWithSecondaryBreadCrumbsArg ]
    }


values_ :
    { view : Elm.Expression
    , init : Elm.Expression
    , after : Elm.Expression
    , headerId : Elm.Expression
    , toPageTitle : Elm.Expression
    , toPageTitleWithSecondaryBreadCrumbs : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "aTagAttributes"
                              , Type.function
                                    [ Type.var "route" ]
                                    (Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "isCurrentRoute"
                              , Type.function [ Type.var "route" ] Type.bool
                              )
                            , ( "label", Type.string )
                            ]
                        , Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , init =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumb"
                            [ Type.var "route" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        )
                    )
            }
    , after =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "after"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumb"
                            [ Type.var "route" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        )
                    )
            }
    , headerId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "headerId"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "route" ]
                        ]
                        Type.string
                    )
            }
    , toPageTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "toPageTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
    , toPageTitleWithSecondaryBreadCrumbs =
        Elm.value
            { importFrom = [ "Nri", "Ui", "BreadCrumbs", "V1" ]
            , name = "toPageTitleWithSecondaryBreadCrumbs"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "BreadCrumbs", "V1" ]
                            "BreadCrumbs"
                            [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
    }


