module Gen.Nri.Ui.Tabs.V7 exposing (annotation_, build, call_, caseOf_, describedBy, disabled, labelledBy, make_, moduleName_, panelHtml, spaHref, tabHtml, tabString, values_, view, withTooltip)

{-| 
@docs moduleName_, view, build, tabString, tabHtml, withTooltip, disabled, labelledBy, describedBy, panelHtml, spaHref, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Tabs", "V7" ]


{-| view: 
    { title : Maybe String
    , alignment : Nri.Ui.Tabs.V7.Alignment
    , customSpacing : Maybe Float
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , tabs : List (Nri.Ui.Tabs.V7.Tab id msg)
    }
    -> Html.Styled.Html msg
-}
view :
    { title : Elm.Expression
    , alignment : Elm.Expression
    , customSpacing : Elm.Expression
    , focusAndSelect : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression
    , tabs : List Elm.Expression
    }
    -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.maybe Type.string )
                            , ( "alignment"
                              , Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Alignment"
                                    []
                              )
                            , ( "customSpacing", Type.maybe Type.float )
                            , ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "id" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "tabs"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tabs", "V7" ]
                                        "Tab"
                                        [ Type.var "id", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "title" viewArg.title
            , Tuple.pair "alignment" viewArg.alignment
            , Tuple.pair "customSpacing" viewArg.customSpacing
            , Tuple.pair
                "focusAndSelect"
                (Elm.functionReduced "viewUnpack" viewArg.focusAndSelect)
            , Tuple.pair "selected" viewArg.selected
            , Tuple.pair "tabs" (Elm.list viewArg.tabs)
            ]
        ]


{-| build: 
    { id : id, idString : String }
    -> List (Nri.Ui.Tabs.V7.Attribute id msg)
    -> Nri.Ui.Tabs.V7.Tab id msg
-}
build :
    { id : Elm.Expression, idString : String }
    -> List Elm.Expression
    -> Elm.Expression
build buildArg buildArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "build"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "idString", Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tabs", "V7" ]
                                "Attribute"
                                [ Type.var "id", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Tab"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "id" buildArg.id
            , Tuple.pair "idString" (Elm.string buildArg.idString)
            ]
        , Elm.list buildArg0
        ]


{-| tabString: String -> Nri.Ui.Tabs.V7.Attribute id msg -}
tabString : String -> Elm.Expression
tabString tabStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "tabString"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string tabStringArg ]


{-| tabHtml: Html.Styled.Html Basics.Never -> Nri.Ui.Tabs.V7.Attribute id msg -}
tabHtml : Elm.Expression -> Elm.Expression
tabHtml tabHtmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "tabHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ tabHtmlArg ]


{-| Tooltip defaults: `[Tooltip.smallPadding, Tooltip.onBottom, Tooltip.fitToContent]`

withTooltip: List (Nri.Ui.Tooltip.V3.Attribute msg) -> Nri.Ui.Tabs.V7.Attribute id msg
-}
withTooltip : List Elm.Expression -> Elm.Expression
withTooltip withTooltipArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "withTooltip"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list withTooltipArg ]


{-| Makes it so that the tab can't be clicked or focused via keyboard navigation

disabled: Bool -> Nri.Ui.Tabs.V7.Attribute id msg
-}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool disabledArg ]


{-| Sets an overriding labelledBy on the tab for an external tooltip.
This assumes an external tooltip is set and disables any internal tooltip configured.

labelledBy: String -> Nri.Ui.Tabs.V7.Attribute id msg
-}
labelledBy : String -> Elm.Expression
labelledBy labelledByArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "labelledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labelledByArg ]


{-| Like [`labelledBy`](#labelledBy), but it describes the given element
instead of labeling it.

This attribute can be used multiple times if more than one element describes
this tab.

describedBy: String -> Nri.Ui.Tabs.V7.Attribute id msg
-}
describedBy : String -> Elm.Expression
describedBy describedByArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "describedBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string describedByArg ]


{-| panelHtml: Html.Styled.Html msg -> Nri.Ui.Tabs.V7.Attribute id msg -}
panelHtml : Elm.Expression -> Elm.Expression
panelHtml panelHtmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "panelHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ panelHtmlArg ]


{-| spaHref: String -> Nri.Ui.Tabs.V7.Attribute id msg -}
spaHref : String -> Elm.Expression
spaHref spaHrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "spaHref"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string spaHrefArg ]


annotation_ :
    { alignment : Type.Annotation
    , tab : Type.Annotation -> Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation -> Type.Annotation
    }
annotation_ =
    { alignment = Type.namedWith [ "Nri", "Ui", "Tabs", "V7" ] "Alignment" []
    , tab =
        \tabArg0 tabArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "Tabs", "V7" ]
                "Tab"
                [ tabArg0, tabArg1 ]
    , attribute =
        \attributeArg0 attributeArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "Tabs", "V7" ]
                "Attribute"
                [ attributeArg0, attributeArg1 ]
    }


make_ :
    { left : Elm.Expression, center : Elm.Expression, right : Elm.Expression }
make_ =
    { left =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "Left"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , center =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "Center"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , right =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "Right"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    }


caseOf_ :
    { alignment :
        Elm.Expression
        -> { alignmentTags_0_0
            | left : Elm.Expression
            , center : Elm.Expression
            , right : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { alignment =
        \alignmentExpression alignmentTags ->
            Elm.Case.custom
                alignmentExpression
                (Type.namedWith [ "Nri", "Ui", "Tabs", "V7" ] "Alignment" [])
                [ Elm.Case.branch0 "Left" alignmentTags.left
                , Elm.Case.branch0 "Center" alignmentTags.center
                , Elm.Case.branch0 "Right" alignmentTags.right
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , build : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tabString : Elm.Expression -> Elm.Expression
    , tabHtml : Elm.Expression -> Elm.Expression
    , withTooltip : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    , labelledBy : Elm.Expression -> Elm.Expression
    , describedBy : Elm.Expression -> Elm.Expression
    , panelHtml : Elm.Expression -> Elm.Expression
    , spaHref : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "title", Type.maybe Type.string )
                                    , ( "alignment"
                                      , Type.namedWith
                                            [ "Nri", "Ui", "Tabs", "V7" ]
                                            "Alignment"
                                            []
                                      )
                                    , ( "customSpacing", Type.maybe Type.float )
                                    , ( "focusAndSelect"
                                      , Type.function
                                            [ Type.record
                                                [ ( "select", Type.var "id" )
                                                , ( "focus"
                                                  , Type.maybe Type.string
                                                  )
                                                ]
                                            ]
                                            (Type.var "msg")
                                      )
                                    , ( "selected", Type.var "id" )
                                    , ( "tabs"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri", "Ui", "Tabs", "V7" ]
                                                "Tab"
                                                [ Type.var "id"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    ]
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
    , build =
        \buildArg buildArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "build"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "id", Type.var "id" )
                                    , ( "idString", Type.string )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tabs", "V7" ]
                                        "Attribute"
                                        [ Type.var "id", Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Tab"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buildArg, buildArg0 ]
    , tabString =
        \tabStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "tabString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabStringArg ]
    , tabHtml =
        \tabHtmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "tabHtml"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabHtmlArg ]
    , withTooltip =
        \withTooltipArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "withTooltip"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tooltip", "V3" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ withTooltipArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "disabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disabledArg ]
    , labelledBy =
        \labelledByArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "labelledBy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelledByArg ]
    , describedBy =
        \describedByArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "describedBy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ describedByArg ]
    , panelHtml =
        \panelHtmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "panelHtml"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ panelHtmlArg ]
    , spaHref =
        \spaHrefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
                    , name = "spaHref"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Attribute"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spaHrefArg ]
    }


values_ :
    { view : Elm.Expression
    , build : Elm.Expression
    , tabString : Elm.Expression
    , tabHtml : Elm.Expression
    , withTooltip : Elm.Expression
    , disabled : Elm.Expression
    , labelledBy : Elm.Expression
    , describedBy : Elm.Expression
    , panelHtml : Elm.Expression
    , spaHref : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.maybe Type.string )
                            , ( "alignment"
                              , Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V7" ]
                                    "Alignment"
                                    []
                              )
                            , ( "customSpacing", Type.maybe Type.float )
                            , ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "id" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "tabs"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tabs", "V7" ]
                                        "Tab"
                                        [ Type.var "id", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , build =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "build"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "idString", Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tabs", "V7" ]
                                "Attribute"
                                [ Type.var "id", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Tab"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , tabString =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "tabString"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , tabHtml =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "tabHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , withTooltip =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "withTooltip"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , labelledBy =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "labelledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , describedBy =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "describedBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , panelHtml =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "panelHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    , spaHref =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V7" ]
            , name = "spaHref"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tabs", "V7" ]
                            "Attribute"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    }


