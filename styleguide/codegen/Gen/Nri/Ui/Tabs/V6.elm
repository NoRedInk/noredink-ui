module Gen.Nri.Ui.Tabs.V6 exposing (annotation_, call_, caseOf_, make_, moduleName_, values_, view, viewTabDefault)

{-| 
@docs moduleName_, view, viewTabDefault, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Tabs", "V6" ]


{-| view: 
    { title : Maybe String
    , alignment : Nri.Ui.Tabs.V6.Alignment
    , customSpacing : Maybe Float
    , onSelect : id -> msg
    , onFocus : String -> msg
    , selected : id
    , tabs : List (Nri.Ui.Tabs.V6.Tab id msg)
    }
    -> Html.Styled.Html msg
-}
view :
    { title : Elm.Expression
    , alignment : Elm.Expression
    , customSpacing : Elm.Expression
    , onSelect : Elm.Expression -> Elm.Expression
    , onFocus : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression
    , tabs : List Elm.Expression
    }
    -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.maybe Type.string )
                            , ( "alignment"
                              , Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V6" ]
                                    "Alignment"
                                    []
                              )
                            , ( "customSpacing", Type.maybe Type.float )
                            , ( "onSelect"
                              , Type.function [ Type.var "id" ] (Type.var "msg")
                              )
                            , ( "onFocus"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "tabs"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tabs", "V6" ]
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
                "onSelect"
                (Elm.functionReduced "viewUnpack" viewArg.onSelect)
            , Tuple.pair
                "onFocus"
                (Elm.functionReduced "viewUnpack" viewArg.onFocus)
            , Tuple.pair "selected" viewArg.selected
            , Tuple.pair "tabs" (Elm.list viewArg.tabs)
            ]
        ]


{-| viewTabDefault: String -> Html.Styled.Html msg -}
viewTabDefault : String -> Elm.Expression
viewTabDefault viewTabDefaultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "viewTabDefault"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string viewTabDefaultArg ]


annotation_ :
    { alignment : Type.Annotation
    , tab : Type.Annotation -> Type.Annotation -> Type.Annotation
    }
annotation_ =
    { alignment = Type.namedWith [ "Nri", "Ui", "Tabs", "V6" ] "Alignment" []
    , tab =
        \tabArg0 tabArg1 ->
            Type.alias
                moduleName_
                "Tab"
                [ tabArg0, tabArg1 ]
                (Type.record
                    [ ( "id", Type.var "id" )
                    , ( "idString", Type.string )
                    , ( "tabView"
                      , Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    , ( "panelView"
                      , Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    , ( "spaHref", Type.maybe Type.string )
                    ]
                )
    }


make_ :
    { left : Elm.Expression
    , center : Elm.Expression
    , right : Elm.Expression
    , tab :
        { id : Elm.Expression
        , idString : Elm.Expression
        , tabView : Elm.Expression
        , panelView : Elm.Expression
        , spaHref : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { left =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "Left"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , center =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "Center"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , right =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "Right"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , tab =
        \tab_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Tabs", "V6" ]
                    "Tab"
                    [ Type.var "id", Type.var "msg" ]
                    (Type.record
                        [ ( "id", Type.var "id" )
                        , ( "idString", Type.string )
                        , ( "tabView"
                          , Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        , ( "panelView"
                          , Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        , ( "spaHref", Type.maybe Type.string )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "id" tab_args.id
                    , Tuple.pair "idString" tab_args.idString
                    , Tuple.pair "tabView" tab_args.tabView
                    , Tuple.pair "panelView" tab_args.panelView
                    , Tuple.pair "spaHref" tab_args.spaHref
                    ]
                )
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
                (Type.namedWith [ "Nri", "Ui", "Tabs", "V6" ] "Alignment" [])
                [ Elm.Case.branch0 "Left" alignmentTags.left
                , Elm.Case.branch0 "Center" alignmentTags.center
                , Elm.Case.branch0 "Right" alignmentTags.right
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , viewTabDefault : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "title", Type.maybe Type.string )
                                    , ( "alignment"
                                      , Type.namedWith
                                            [ "Nri", "Ui", "Tabs", "V6" ]
                                            "Alignment"
                                            []
                                      )
                                    , ( "customSpacing", Type.maybe Type.float )
                                    , ( "onSelect"
                                      , Type.function
                                            [ Type.var "id" ]
                                            (Type.var "msg")
                                      )
                                    , ( "onFocus"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "selected", Type.var "id" )
                                    , ( "tabs"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri", "Ui", "Tabs", "V6" ]
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
    , viewTabDefault =
        \viewTabDefaultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
                    , name = "viewTabDefault"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewTabDefaultArg ]
    }


values_ : { view : Elm.Expression, viewTabDefault : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.maybe Type.string )
                            , ( "alignment"
                              , Type.namedWith
                                    [ "Nri", "Ui", "Tabs", "V6" ]
                                    "Alignment"
                                    []
                              )
                            , ( "customSpacing", Type.maybe Type.float )
                            , ( "onSelect"
                              , Type.function [ Type.var "id" ] (Type.var "msg")
                              )
                            , ( "onFocus"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "tabs"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tabs", "V6" ]
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
    , viewTabDefault =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tabs", "V6" ]
            , name = "viewTabDefault"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


