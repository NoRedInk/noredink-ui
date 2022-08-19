module Gen.Nri.Ui.Carousel.V1 exposing (annotation_, buildItem, call_, moduleName_, values_, view)

{-| 
@docs moduleName_, view, buildItem, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Carousel", "V1" ]


{-| view: 
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , controlStyles : Bool -> List Css.Style
    , controlListStyles : List Css.Style
    , items : List (Nri.Ui.Carousel.V1.Item id msg)
    }
    -> { controls : Html.Styled.Html msg, slides : Html.Styled.Html msg }
-}
view :
    { focusAndSelect : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression
    , controlStyles : Elm.Expression -> Elm.Expression
    , controlListStyles : List Elm.Expression
    , items : List Elm.Expression
    }
    -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "id" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "controlStyles"
                              , Type.function
                                    [ Type.bool ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "controlListStyles"
                              , Type.list (Type.namedWith [ "Css" ] "Style" [])
                              )
                            , ( "items"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Carousel", "V1" ]
                                        "Item"
                                        [ Type.var "id", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.record
                            [ ( "controls"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "slides"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "focusAndSelect"
                (Elm.functionReduced "viewUnpack" viewArg.focusAndSelect)
            , Tuple.pair "selected" viewArg.selected
            , Tuple.pair
                "controlStyles"
                (Elm.functionReduced "viewUnpack" viewArg.controlStyles)
            , Tuple.pair
                "controlListStyles"
                (Elm.list viewArg.controlListStyles)
            , Tuple.pair "items" (Elm.list viewArg.items)
            ]
        ]


{-| Builds an selectable item in the Caroursel

`controlHtml` represents the element that will appear in the list of options.

`slideHtml` represents the element that will be shown in your carousel when this item is selected.

buildItem: 
    { id : id
    , idString : String
    , slideHtml : Html.Styled.Html msg
    , controlHtml : Html.Styled.Html Basics.Never
    }
    -> Nri.Ui.Carousel.V1.Item id msg
-}
buildItem :
    { id : Elm.Expression
    , idString : String
    , slideHtml : Elm.Expression
    , controlHtml : Elm.Expression
    }
    -> Elm.Expression
buildItem buildItemArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
            , name = "buildItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "idString", Type.string )
                            , ( "slideHtml"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "controlHtml"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Carousel", "V1" ]
                            "Item"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "id" buildItemArg.id
            , Tuple.pair "idString" (Elm.string buildItemArg.idString)
            , Tuple.pair "slideHtml" buildItemArg.slideHtml
            , Tuple.pair "controlHtml" buildItemArg.controlHtml
            ]
        ]


annotation_ : { item : Type.Annotation -> Type.Annotation -> Type.Annotation }
annotation_ =
    { item =
        \itemArg0 itemArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "Carousel", "V1" ]
                "Item"
                [ itemArg0, itemArg1 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , buildItem : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "focusAndSelect"
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
                                    , ( "controlStyles"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Css" ]
                                                    "Style"
                                                    []
                                                )
                                            )
                                      )
                                    , ( "controlListStyles"
                                      , Type.list
                                            (Type.namedWith [ "Css" ] "Style" []
                                            )
                                      )
                                    , ( "items"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri"
                                                , "Ui"
                                                , "Carousel"
                                                , "V1"
                                                ]
                                                "Item"
                                                [ Type.var "id"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    ]
                                ]
                                (Type.record
                                    [ ( "controls"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "slides"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , buildItem =
        \buildItemArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
                    , name = "buildItem"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "id", Type.var "id" )
                                    , ( "idString", Type.string )
                                    , ( "slideHtml"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "controlHtml"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.namedWith
                                                [ "Basics" ]
                                                "Never"
                                                []
                                            ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Carousel", "V1" ]
                                    "Item"
                                    [ Type.var "id", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buildItemArg ]
    }


values_ : { view : Elm.Expression, buildItem : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "id" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "selected", Type.var "id" )
                            , ( "controlStyles"
                              , Type.function
                                    [ Type.bool ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "controlListStyles"
                              , Type.list (Type.namedWith [ "Css" ] "Style" [])
                              )
                            , ( "items"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Carousel", "V1" ]
                                        "Item"
                                        [ Type.var "id", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.record
                            [ ( "controls"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "slides"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            ]
                        )
                    )
            }
    , buildItem =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Carousel", "V1" ]
            , name = "buildItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "idString", Type.string )
                            , ( "slideHtml"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "controlHtml"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Carousel", "V1" ]
                            "Item"
                            [ Type.var "id", Type.var "msg" ]
                        )
                    )
            }
    }


