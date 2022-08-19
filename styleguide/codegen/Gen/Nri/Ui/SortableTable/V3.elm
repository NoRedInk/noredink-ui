module Gen.Nri.Ui.SortableTable.V3 exposing (annotation_, call_, combineSorters, custom, init, initDescending, invariantSort, make_, moduleName_, simpleSort, string, values_, view, viewLoading)

{-| 
@docs moduleName_, init, initDescending, custom, string, view, viewLoading, invariantSort, simpleSort, combineSorters, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "SortableTable", "V3" ]


{-| init: id -> Nri.Ui.SortableTable.V3.State id -}
init : Elm.Expression -> Elm.Expression
init initArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "id" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        )
                    )
            }
        )
        [ initArg ]


{-| initDescending: id -> Nri.Ui.SortableTable.V3.State id -}
initDescending : Elm.Expression -> Elm.Expression
initDescending initDescendingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "initDescending"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "id" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        )
                    )
            }
        )
        [ initDescendingArg ]


{-| custom: 
    { id : id
    , header : Html.Styled.Html msg
    , view : entry -> Html.Styled.Html msg
    , sorter : Maybe (Nri.Ui.SortableTable.V3.Sorter entry)
    , width : Int
    , cellStyles : entry -> List Css.Style
    }
    -> Nri.Ui.SortableTable.V3.Column id entry msg
-}
custom :
    { id : Elm.Expression
    , header : Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , sorter : Elm.Expression
    , width : Int
    , cellStyles : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "header"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "sorter"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SortableTable", "V3" ]
                                        "Sorter"
                                        [ Type.var "entry" ]
                                    )
                              )
                            , ( "width", Type.int )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Column"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "id" customArg.id
            , Tuple.pair "header" customArg.header
            , Tuple.pair
                "view"
                (Elm.functionReduced "customUnpack" customArg.view)
            , Tuple.pair "sorter" customArg.sorter
            , Tuple.pair "width" (Elm.int customArg.width)
            , Tuple.pair
                "cellStyles"
                (Elm.functionReduced "customUnpack" customArg.cellStyles)
            ]
        ]


{-| string: 
    { id : id
    , header : String
    , value : entry -> String
    , width : Int
    , cellStyles : entry -> List Css.Style
    }
    -> Nri.Ui.SortableTable.V3.Column id entry msg
-}
string :
    { id : Elm.Expression
    , header : String
    , value : Elm.Expression -> Elm.Expression
    , width : Int
    , cellStyles : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
string stringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "header", Type.string )
                            , ( "value"
                              , Type.function [ Type.var "entry" ] Type.string
                              )
                            , ( "width", Type.int )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Column"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "id" stringArg.id
            , Tuple.pair "header" (Elm.string stringArg.header)
            , Tuple.pair
                "value"
                (Elm.functionReduced "stringUnpack" stringArg.value)
            , Tuple.pair "width" (Elm.int stringArg.width)
            , Tuple.pair
                "cellStyles"
                (Elm.functionReduced "stringUnpack" stringArg.cellStyles)
            ]
        ]


{-| view: 
    Nri.Ui.SortableTable.V3.Config id entry msg
    -> Nri.Ui.SortableTable.V3.State id
    -> List entry
    -> Html.Styled.Html msg
-}
view : Elm.Expression -> Elm.Expression -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 viewArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Config"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        , Type.list (Type.var "entry")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewArg, viewArg0, Elm.list viewArg1 ]


{-| viewLoading: 
    Nri.Ui.SortableTable.V3.Config id entry msg
    -> Nri.Ui.SortableTable.V3.State id
    -> Html.Styled.Html msg
-}
viewLoading : Elm.Expression -> Elm.Expression -> Elm.Expression
viewLoading viewLoadingArg viewLoadingArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "viewLoading"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Config"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewLoadingArg, viewLoadingArg0 ]


{-| Create a sorter function that always orders the entries in the same order.
For example, this is useful when we want to resolve ties and sort the tied
entries by name, no matter of the sort direction set on the table.

invariantSort: (entry -> comparable) -> Nri.Ui.SortableTable.V3.Sorter entry
-}
invariantSort : (Elm.Expression -> Elm.Expression) -> Elm.Expression
invariantSort invariantSortArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "invariantSort"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "entry" ]
                            (Type.var "comparable")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "invariantSortUnpack" invariantSortArg ]


{-| Create a simple sorter function that orders entries by mapping a function
over the collection. It will also reverse it when the sort direction is descending.

simpleSort: (entry -> comparable) -> Nri.Ui.SortableTable.V3.Sorter entry
-}
simpleSort : (Elm.Expression -> Elm.Expression) -> Elm.Expression
simpleSort simpleSortArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "simpleSort"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "entry" ]
                            (Type.var "comparable")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "simpleSortUnpack" simpleSortArg ]


{-| combineSorters: 
    List (Nri.Ui.SortableTable.V3.Sorter entry)
    -> Nri.Ui.SortableTable.V3.Sorter entry
-}
combineSorters : List Elm.Expression -> Elm.Expression
combineSorters combineSortersArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "combineSorters"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SortableTable", "V3" ]
                                "Sorter"
                                [ Type.var "entry" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
        )
        [ Elm.list combineSortersArg ]


annotation_ :
    { column :
        Type.Annotation -> Type.Annotation -> Type.Annotation -> Type.Annotation
    , config :
        Type.Annotation -> Type.Annotation -> Type.Annotation -> Type.Annotation
    , sorter : Type.Annotation -> Type.Annotation
    , state : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { column =
        \columnArg0 columnArg1 columnArg2 ->
            Type.namedWith
                [ "Nri", "Ui", "SortableTable", "V3" ]
                "Column"
                [ columnArg0, columnArg1, columnArg2 ]
    , config =
        \configArg0 configArg1 configArg2 ->
            Type.alias
                moduleName_
                "Config"
                [ configArg0, configArg1, configArg2 ]
                (Type.record
                    [ ( "updateMsg"
                      , Type.function
                            [ Type.namedWith
                                [ "Nri", "Ui", "SortableTable", "V3" ]
                                "State"
                                [ Type.var "id" ]
                            ]
                            (Type.var "msg")
                      )
                    , ( "columns"
                      , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SortableTable", "V3" ]
                                "Column"
                                [ Type.var "id"
                                , Type.var "entry"
                                , Type.var "msg"
                                ]
                            )
                      )
                    ]
                )
    , sorter =
        \sorterArg0 ->
            Type.alias
                moduleName_
                "Sorter"
                [ sorterArg0 ]
                (Type.function
                    [ Type.namedWith
                        [ "Nri", "Ui", "Table", "V6" ]
                        "SortDirection"
                        []
                    , Type.var "a"
                    , Type.var "a"
                    ]
                    (Type.namedWith [ "Basics" ] "Order" [])
                )
    , state =
        \stateArg0 ->
            Type.alias
                moduleName_
                "State"
                [ stateArg0 ]
                (Type.record
                    [ ( "column", Type.var "id" )
                    , ( "sortDirection"
                      , Type.namedWith
                            [ "Nri", "Ui", "Table", "V6" ]
                            "SortDirection"
                            []
                      )
                    ]
                )
    }


make_ :
    { config :
        { updateMsg : Elm.Expression, columns : Elm.Expression }
        -> Elm.Expression
    , state :
        { column : Elm.Expression, sortDirection : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { config =
        \config_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "SortableTable", "V3" ]
                    "Config"
                    [ Type.var "id", Type.var "entry", Type.var "msg" ]
                    (Type.record
                        [ ( "updateMsg"
                          , Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "State"
                                    [ Type.var "id" ]
                                ]
                                (Type.var "msg")
                          )
                        , ( "columns"
                          , Type.list
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Column"
                                    [ Type.var "id"
                                    , Type.var "entry"
                                    , Type.var "msg"
                                    ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "updateMsg" config_args.updateMsg
                    , Tuple.pair "columns" config_args.columns
                    ]
                )
    , state =
        \state_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "SortableTable", "V3" ]
                    "State"
                    [ Type.var "id" ]
                    (Type.record
                        [ ( "column", Type.var "id" )
                        , ( "sortDirection"
                          , Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "SortDirection"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "column" state_args.column
                    , Tuple.pair "sortDirection" state_args.sortDirection
                    ]
                )
    }


call_ :
    { init : Elm.Expression -> Elm.Expression
    , initDescending : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , string : Elm.Expression -> Elm.Expression
    , view :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewLoading : Elm.Expression -> Elm.Expression -> Elm.Expression
    , invariantSort : Elm.Expression -> Elm.Expression
    , simpleSort : Elm.Expression -> Elm.Expression
    , combineSorters : Elm.Expression -> Elm.Expression
    }
call_ =
    { init =
        \initArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "id" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "State"
                                    [ Type.var "id" ]
                                )
                            )
                    }
                )
                [ initArg ]
    , initDescending =
        \initDescendingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "initDescending"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "id" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "State"
                                    [ Type.var "id" ]
                                )
                            )
                    }
                )
                [ initDescendingArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "id", Type.var "id" )
                                    , ( "header"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "view"
                                      , Type.function
                                            [ Type.var "entry" ]
                                            (Type.namedWith
                                                [ "Html", "Styled" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "sorter"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Nri"
                                                , "Ui"
                                                , "SortableTable"
                                                , "V3"
                                                ]
                                                "Sorter"
                                                [ Type.var "entry" ]
                                            )
                                      )
                                    , ( "width", Type.int )
                                    , ( "cellStyles"
                                      , Type.function
                                            [ Type.var "entry" ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Css" ]
                                                    "Style"
                                                    []
                                                )
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Column"
                                    [ Type.var "id"
                                    , Type.var "entry"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ customArg ]
    , string =
        \stringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "string"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "id", Type.var "id" )
                                    , ( "header", Type.string )
                                    , ( "value"
                                      , Type.function
                                            [ Type.var "entry" ]
                                            Type.string
                                      )
                                    , ( "width", Type.int )
                                    , ( "cellStyles"
                                      , Type.function
                                            [ Type.var "entry" ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Css" ]
                                                    "Style"
                                                    []
                                                )
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Column"
                                    [ Type.var "id"
                                    , Type.var "entry"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ stringArg ]
    , view =
        \viewArg viewArg0 viewArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Config"
                                    [ Type.var "id"
                                    , Type.var "entry"
                                    , Type.var "msg"
                                    ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "State"
                                    [ Type.var "id" ]
                                , Type.list (Type.var "entry")
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0, viewArg1 ]
    , viewLoading =
        \viewLoadingArg viewLoadingArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "viewLoading"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Config"
                                    [ Type.var "id"
                                    , Type.var "entry"
                                    , Type.var "msg"
                                    ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "State"
                                    [ Type.var "id" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewLoadingArg, viewLoadingArg0 ]
    , invariantSort =
        \invariantSortArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "invariantSort"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "entry" ]
                                    (Type.var "comparable")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Sorter"
                                    [ Type.var "entry" ]
                                )
                            )
                    }
                )
                [ invariantSortArg ]
    , simpleSort =
        \simpleSortArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "simpleSort"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "entry" ]
                                    (Type.var "comparable")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Sorter"
                                    [ Type.var "entry" ]
                                )
                            )
                    }
                )
                [ simpleSortArg ]
    , combineSorters =
        \combineSortersArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
                    , name = "combineSorters"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SortableTable", "V3" ]
                                        "Sorter"
                                        [ Type.var "entry" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "SortableTable", "V3" ]
                                    "Sorter"
                                    [ Type.var "entry" ]
                                )
                            )
                    }
                )
                [ combineSortersArg ]
    }


values_ :
    { init : Elm.Expression
    , initDescending : Elm.Expression
    , custom : Elm.Expression
    , string : Elm.Expression
    , view : Elm.Expression
    , viewLoading : Elm.Expression
    , invariantSort : Elm.Expression
    , simpleSort : Elm.Expression
    , combineSorters : Elm.Expression
    }
values_ =
    { init =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "id" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        )
                    )
            }
    , initDescending =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "initDescending"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "id" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "header"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "sorter"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "SortableTable", "V3" ]
                                        "Sorter"
                                        [ Type.var "entry" ]
                                    )
                              )
                            , ( "width", Type.int )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Column"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        )
                    )
            }
    , string =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "id", Type.var "id" )
                            , ( "header", Type.string )
                            , ( "value"
                              , Type.function [ Type.var "entry" ] Type.string
                              )
                            , ( "width", Type.int )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "entry" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Column"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        )
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Config"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        , Type.list (Type.var "entry")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewLoading =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "viewLoading"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Config"
                            [ Type.var "id", Type.var "entry", Type.var "msg" ]
                        , Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "State"
                            [ Type.var "id" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , invariantSort =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "invariantSort"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "entry" ]
                            (Type.var "comparable")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
    , simpleSort =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "simpleSort"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "entry" ]
                            (Type.var "comparable")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
    , combineSorters =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SortableTable", "V3" ]
            , name = "combineSorters"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "SortableTable", "V3" ]
                                "Sorter"
                                [ Type.var "entry" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "SortableTable", "V3" ]
                            "Sorter"
                            [ Type.var "entry" ]
                        )
                    )
            }
    }


