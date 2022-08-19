module Gen.Nri.Ui.Accordion.V3 exposing (annotation_, call_, caseOf_, make_, moduleName_, styleAccordion, values_, view)

{-| 
@docs moduleName_, view, styleAccordion, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Accordion", "V3" ]


{-| view: 
    { entries : List (Nri.Ui.Accordion.V3.AccordionEntry msg)
    , focus : String -> msg
    }
    -> Accessibility.Styled.Html msg
-}
view :
    { entries : List Elm.Expression, focus : Elm.Expression -> Elm.Expression }
    -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "entries"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Accordion", "V3" ]
                                        "AccordionEntry"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "focus"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
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
            [ Tuple.pair "entries" (Elm.list viewArg.entries)
            , Tuple.pair
                "focus"
                (Elm.functionReduced "viewUnpack" viewArg.focus)
            ]
        ]


{-| styleAccordion: Nri.Ui.Accordion.V3.StyleOptions -> Accessibility.Styled.Html msg -}
styleAccordion : Elm.Expression -> Elm.Expression
styleAccordion styleAccordionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "styleAccordion"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Accordion", "V3" ]
                            "StyleOptions"
                            []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ styleAccordionArg ]


annotation_ :
    { headerLevel : Type.Annotation
    , accordionEntry : Type.Annotation -> Type.Annotation
    , entry : Type.Annotation -> Type.Annotation
    , styleOptions : Type.Annotation
    }
annotation_ =
    { headerLevel =
        Type.namedWith [ "Nri", "Ui", "Accordion", "V3" ] "HeaderLevel" []
    , accordionEntry =
        \accordionEntryArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Accordion", "V3" ]
                "AccordionEntry"
                [ accordionEntryArg0 ]
    , entry =
        \entryArg0 ->
            Type.alias
                moduleName_
                "Entry"
                [ entryArg0 ]
                (Type.record
                    [ ( "caret"
                      , Type.function
                            [ Type.bool ]
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "content"
                      , Type.function
                            [ Type.unit ]
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "entryClass", Type.string )
                    , ( "headerContent"
                      , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    , ( "headerId", Type.string )
                    , ( "headerLevel"
                      , Type.namedWith
                            [ "Nri", "Ui", "Accordion", "V3" ]
                            "HeaderLevel"
                            []
                      )
                    , ( "isExpanded", Type.bool )
                    , ( "toggle"
                      , Type.maybe
                            (Type.function [ Type.bool ] (Type.var "msg"))
                      )
                    ]
                )
    , styleOptions =
        Type.alias
            moduleName_
            "StyleOptions"
            []
            (Type.record
                [ ( "entryStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "entryExpandedStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "entryClosedStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "headerStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "headerExpandedStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "headerClosedStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                , ( "contentStyles"
                  , Type.list (Type.namedWith [ "Css" ] "Style" [])
                  )
                ]
            )
    }


make_ :
    { h1 : Elm.Expression
    , h2 : Elm.Expression
    , h3 : Elm.Expression
    , h4 : Elm.Expression
    , h5 : Elm.Expression
    , h6 : Elm.Expression
    , accordionEntry : Elm.Expression -> Elm.Expression -> Elm.Expression
    , entry :
        { caret : Elm.Expression
        , content : Elm.Expression
        , entryClass : Elm.Expression
        , headerContent : Elm.Expression
        , headerId : Elm.Expression
        , headerLevel : Elm.Expression
        , isExpanded : Elm.Expression
        , toggle : Elm.Expression
        }
        -> Elm.Expression
    , styleOptions :
        { entryStyles : Elm.Expression
        , entryExpandedStyles : Elm.Expression
        , entryClosedStyles : Elm.Expression
        , headerStyles : Elm.Expression
        , headerExpandedStyles : Elm.Expression
        , headerClosedStyles : Elm.Expression
        , contentStyles : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { h1 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H1"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , h2 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H2"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , h3 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H3"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , h4 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H4"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , h5 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H5"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , h6 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "H6"
            , annotation = Just (Type.namedWith [] "HeaderLevel" [])
            }
    , accordionEntry =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
                    , name = "AccordionEntry"
                    , annotation =
                        Just
                            (Type.namedWith
                                []
                                "AccordionEntry"
                                [ Type.var "msg" ]
                            )
                    }
                )
                [ ar0, ar1 ]
    , entry =
        \entry_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Accordion", "V3" ]
                    "Entry"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "caret"
                          , Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "content"
                          , Type.function
                                [ Type.unit ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "entryClass", Type.string )
                        , ( "headerContent"
                          , Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        , ( "headerId", Type.string )
                        , ( "headerLevel"
                          , Type.namedWith
                                [ "Nri", "Ui", "Accordion", "V3" ]
                                "HeaderLevel"
                                []
                          )
                        , ( "isExpanded", Type.bool )
                        , ( "toggle"
                          , Type.maybe
                                (Type.function [ Type.bool ] (Type.var "msg"))
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "caret" entry_args.caret
                    , Tuple.pair "content" entry_args.content
                    , Tuple.pair "entryClass" entry_args.entryClass
                    , Tuple.pair "headerContent" entry_args.headerContent
                    , Tuple.pair "headerId" entry_args.headerId
                    , Tuple.pair "headerLevel" entry_args.headerLevel
                    , Tuple.pair "isExpanded" entry_args.isExpanded
                    , Tuple.pair "toggle" entry_args.toggle
                    ]
                )
    , styleOptions =
        \styleOptions_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Accordion", "V3" ]
                    "StyleOptions"
                    []
                    (Type.record
                        [ ( "entryStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "entryExpandedStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "entryClosedStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "headerStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "headerExpandedStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "headerClosedStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        , ( "contentStyles"
                          , Type.list (Type.namedWith [ "Css" ] "Style" [])
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "entryStyles" styleOptions_args.entryStyles
                    , Tuple.pair
                        "entryExpandedStyles"
                        styleOptions_args.entryExpandedStyles
                    , Tuple.pair
                        "entryClosedStyles"
                        styleOptions_args.entryClosedStyles
                    , Tuple.pair "headerStyles" styleOptions_args.headerStyles
                    , Tuple.pair
                        "headerExpandedStyles"
                        styleOptions_args.headerExpandedStyles
                    , Tuple.pair
                        "headerClosedStyles"
                        styleOptions_args.headerClosedStyles
                    , Tuple.pair "contentStyles" styleOptions_args.contentStyles
                    ]
                )
    }


caseOf_ :
    { headerLevel :
        Elm.Expression
        -> { headerLevelTags_0_0
            | h1 : Elm.Expression
            , h2 : Elm.Expression
            , h3 : Elm.Expression
            , h4 : Elm.Expression
            , h5 : Elm.Expression
            , h6 : Elm.Expression
        }
        -> Elm.Expression
    , accordionEntry :
        Elm.Expression
        -> { accordionEntryTags_1_0
            | accordionEntry :
                Elm.Expression -> Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { headerLevel =
        \headerLevelExpression headerLevelTags ->
            Elm.Case.custom
                headerLevelExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Accordion", "V3" ]
                    "HeaderLevel"
                    []
                )
                [ Elm.Case.branch0 "H1" headerLevelTags.h1
                , Elm.Case.branch0 "H2" headerLevelTags.h2
                , Elm.Case.branch0 "H3" headerLevelTags.h3
                , Elm.Case.branch0 "H4" headerLevelTags.h4
                , Elm.Case.branch0 "H5" headerLevelTags.h5
                , Elm.Case.branch0 "H6" headerLevelTags.h6
                ]
    , accordionEntry =
        \accordionEntryExpression accordionEntryTags ->
            Elm.Case.custom
                accordionEntryExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Accordion", "V3" ]
                    "AccordionEntry"
                    [ Type.var "msg" ]
                )
                [ Elm.Case.branch2
                    "AccordionEntry"
                    ( "nri.Ui.Accordion.V3.Entry"
                    , Type.namedWith
                        [ "Nri", "Ui", "Accordion", "V3" ]
                        "Entry"
                        [ Type.var "msg" ]
                    )
                    ( "list.List"
                    , Type.list
                        (Type.namedWith
                            [ "Nri", "Ui", "Accordion", "V3" ]
                            "AccordionEntry"
                            [ Type.var "msg" ]
                        )
                    )
                    accordionEntryTags.accordionEntry
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , styleAccordion : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "entries"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri"
                                                , "Ui"
                                                , "Accordion"
                                                , "V3"
                                                ]
                                                "AccordionEntry"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "focus"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
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
                [ viewArg ]
    , styleAccordion =
        \styleAccordionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
                    , name = "styleAccordion"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Accordion", "V3" ]
                                    "StyleOptions"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styleAccordionArg ]
    }


values_ : { view : Elm.Expression, styleAccordion : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "entries"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Accordion", "V3" ]
                                        "AccordionEntry"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "focus"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , styleAccordion =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Accordion", "V3" ]
            , name = "styleAccordion"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Accordion", "V3" ]
                            "StyleOptions"
                            []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


