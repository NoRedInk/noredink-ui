module Gen.Nri.Ui.Table.V6 exposing (annotation_, call_, caseOf_, custom, make_, moduleName_, string, values_, view, viewLoading, viewLoadingWithoutHeader, viewWithoutHeader)

{-| 
@docs moduleName_, custom, string, view, viewWithoutHeader, viewLoading, viewLoadingWithoutHeader, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Table", "V6" ]


{-| A column that renders however you want it to

custom: 
    { header : Html.Styled.Html msg
    , view : data -> Html.Styled.Html msg
    , width : Css.LengthOrAuto compatible
    , cellStyles : data -> List Css.Style
    , sort : Maybe Nri.Ui.Table.V6.SortDirection
    }
    -> Nri.Ui.Table.V6.Column data msg
-}
custom :
    { header : Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , width : Elm.Expression
    , cellStyles : Elm.Expression -> Elm.Expression
    , sort : Elm.Expression
    }
    -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "header"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "width"
                              , Type.namedWith
                                    [ "Css" ]
                                    "LengthOrAuto"
                                    [ Type.var "compatible" ]
                              )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "sort"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "SortDirection"
                                        []
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Table", "V6" ]
                            "Column"
                            [ Type.var "data", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "header" customArg.header
            , Tuple.pair
                "view"
                (Elm.functionReduced "customUnpack" customArg.view)
            , Tuple.pair "width" customArg.width
            , Tuple.pair
                "cellStyles"
                (Elm.functionReduced "customUnpack" customArg.cellStyles)
            , Tuple.pair "sort" customArg.sort
            ]
        ]


{-| A column that renders some aspect of a value as text

string: 
    { header : String
    , value : data -> String
    , width : Css.LengthOrAuto compatible
    , cellStyles : data -> List Css.Style
    , sort : Maybe Nri.Ui.Table.V6.SortDirection
    }
    -> Nri.Ui.Table.V6.Column data msg
-}
string :
    { header : String
    , value : Elm.Expression -> Elm.Expression
    , width : Elm.Expression
    , cellStyles : Elm.Expression -> Elm.Expression
    , sort : Elm.Expression
    }
    -> Elm.Expression
string stringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "header", Type.string )
                            , ( "value"
                              , Type.function [ Type.var "data" ] Type.string
                              )
                            , ( "width"
                              , Type.namedWith
                                    [ "Css" ]
                                    "LengthOrAuto"
                                    [ Type.var "compatible" ]
                              )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "sort"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "SortDirection"
                                        []
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Table", "V6" ]
                            "Column"
                            [ Type.var "data", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "header" (Elm.string stringArg.header)
            , Tuple.pair
                "value"
                (Elm.functionReduced "stringUnpack" stringArg.value)
            , Tuple.pair "width" stringArg.width
            , Tuple.pair
                "cellStyles"
                (Elm.functionReduced "stringUnpack" stringArg.cellStyles)
            , Tuple.pair "sort" stringArg.sort
            ]
        ]


{-| Displays a table of data based on the provided column definitions

view: List (Nri.Ui.Table.V6.Column data msg) -> List data -> Html.Styled.Html msg
-}
view : List Elm.Expression -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        , Type.list (Type.var "data")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list viewArg, Elm.list viewArg0 ]


{-| Displays a table of data without a header row

viewWithoutHeader: List (Nri.Ui.Table.V6.Column data msg) -> List data -> Html.Styled.Html msg
-}
viewWithoutHeader : List Elm.Expression -> List Elm.Expression -> Elm.Expression
viewWithoutHeader viewWithoutHeaderArg viewWithoutHeaderArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewWithoutHeader"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        , Type.list (Type.var "data")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list viewWithoutHeaderArg, Elm.list viewWithoutHeaderArg0 ]


{-| Display a table with the given columns but instead of data, show blocked
out text with an interesting animation. This view lets the user know that
data is on its way and what it will look like when it arrives.

viewLoading: List (Nri.Ui.Table.V6.Column data msg) -> Html.Styled.Html msg
-}
viewLoading : List Elm.Expression -> Elm.Expression
viewLoading viewLoadingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewLoading"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
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
        [ Elm.list viewLoadingArg ]


{-| Display the loading table without a header row

viewLoadingWithoutHeader: List (Nri.Ui.Table.V6.Column data msg) -> Html.Styled.Html msg
-}
viewLoadingWithoutHeader : List Elm.Expression -> Elm.Expression
viewLoadingWithoutHeader viewLoadingWithoutHeaderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewLoadingWithoutHeader"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
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
        [ Elm.list viewLoadingWithoutHeaderArg ]


annotation_ :
    { column : Type.Annotation -> Type.Annotation -> Type.Annotation
    , sortDirection : Type.Annotation
    }
annotation_ =
    { column =
        \columnArg0 columnArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "Table", "V6" ]
                "Column"
                [ columnArg0, columnArg1 ]
    , sortDirection =
        Type.namedWith [ "Nri", "Ui", "Table", "V6" ] "SortDirection" []
    }


make_ : { ascending : Elm.Expression, descending : Elm.Expression }
make_ =
    { ascending =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "Ascending"
            , annotation = Just (Type.namedWith [] "SortDirection" [])
            }
    , descending =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "Descending"
            , annotation = Just (Type.namedWith [] "SortDirection" [])
            }
    }


caseOf_ :
    { sortDirection :
        Elm.Expression
        -> { sortDirectionTags_0_0
            | ascending : Elm.Expression
            , descending : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { sortDirection =
        \sortDirectionExpression sortDirectionTags ->
            Elm.Case.custom
                sortDirectionExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Table", "V6" ]
                    "SortDirection"
                    []
                )
                [ Elm.Case.branch0 "Ascending" sortDirectionTags.ascending
                , Elm.Case.branch0 "Descending" sortDirectionTags.descending
                ]
    }


call_ :
    { custom : Elm.Expression -> Elm.Expression
    , string : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewWithoutHeader : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewLoading : Elm.Expression -> Elm.Expression
    , viewLoadingWithoutHeader : Elm.Expression -> Elm.Expression
    }
call_ =
    { custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "header"
                                      , Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "view"
                                      , Type.function
                                            [ Type.var "data" ]
                                            (Type.namedWith
                                                [ "Html", "Styled" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "width"
                                      , Type.namedWith
                                            [ "Css" ]
                                            "LengthOrAuto"
                                            [ Type.var "compatible" ]
                                      )
                                    , ( "cellStyles"
                                      , Type.function
                                            [ Type.var "data" ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Css" ]
                                                    "Style"
                                                    []
                                                )
                                            )
                                      )
                                    , ( "sort"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Nri", "Ui", "Table", "V6" ]
                                                "SortDirection"
                                                []
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Table", "V6" ]
                                    "Column"
                                    [ Type.var "data", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , string =
        \stringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "string"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "header", Type.string )
                                    , ( "value"
                                      , Type.function
                                            [ Type.var "data" ]
                                            Type.string
                                      )
                                    , ( "width"
                                      , Type.namedWith
                                            [ "Css" ]
                                            "LengthOrAuto"
                                            [ Type.var "compatible" ]
                                      )
                                    , ( "cellStyles"
                                      , Type.function
                                            [ Type.var "data" ]
                                            (Type.list
                                                (Type.namedWith
                                                    [ "Css" ]
                                                    "Style"
                                                    []
                                                )
                                            )
                                      )
                                    , ( "sort"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Nri", "Ui", "Table", "V6" ]
                                                "SortDirection"
                                                []
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Table", "V6" ]
                                    "Column"
                                    [ Type.var "data", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stringArg ]
    , view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "Column"
                                        [ Type.var "data", Type.var "msg" ]
                                    )
                                , Type.list (Type.var "data")
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , viewWithoutHeader =
        \viewWithoutHeaderArg viewWithoutHeaderArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "viewWithoutHeader"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "Column"
                                        [ Type.var "data", Type.var "msg" ]
                                    )
                                , Type.list (Type.var "data")
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewWithoutHeaderArg, viewWithoutHeaderArg0 ]
    , viewLoading =
        \viewLoadingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "viewLoading"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "Column"
                                        [ Type.var "data", Type.var "msg" ]
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
                [ viewLoadingArg ]
    , viewLoadingWithoutHeader =
        \viewLoadingWithoutHeaderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Table", "V6" ]
                    , name = "viewLoadingWithoutHeader"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "Column"
                                        [ Type.var "data", Type.var "msg" ]
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
                [ viewLoadingWithoutHeaderArg ]
    }


values_ :
    { custom : Elm.Expression
    , string : Elm.Expression
    , view : Elm.Expression
    , viewWithoutHeader : Elm.Expression
    , viewLoading : Elm.Expression
    , viewLoadingWithoutHeader : Elm.Expression
    }
values_ =
    { custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "header"
                              , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "width"
                              , Type.namedWith
                                    [ "Css" ]
                                    "LengthOrAuto"
                                    [ Type.var "compatible" ]
                              )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "sort"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "SortDirection"
                                        []
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Table", "V6" ]
                            "Column"
                            [ Type.var "data", Type.var "msg" ]
                        )
                    )
            }
    , string =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "header", Type.string )
                            , ( "value"
                              , Type.function [ Type.var "data" ] Type.string
                              )
                            , ( "width"
                              , Type.namedWith
                                    [ "Css" ]
                                    "LengthOrAuto"
                                    [ Type.var "compatible" ]
                              )
                            , ( "cellStyles"
                              , Type.function
                                    [ Type.var "data" ]
                                    (Type.list
                                        (Type.namedWith [ "Css" ] "Style" [])
                                    )
                              )
                            , ( "sort"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Table", "V6" ]
                                        "SortDirection"
                                        []
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Table", "V6" ]
                            "Column"
                            [ Type.var "data", Type.var "msg" ]
                        )
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        , Type.list (Type.var "data")
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewWithoutHeader =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewWithoutHeader"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        , Type.list (Type.var "data")
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
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewLoading"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewLoadingWithoutHeader =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Table", "V6" ]
            , name = "viewLoadingWithoutHeader"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Table", "V6" ]
                                "Column"
                                [ Type.var "data", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


