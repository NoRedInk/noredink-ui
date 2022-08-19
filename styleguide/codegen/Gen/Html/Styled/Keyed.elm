module Gen.Html.Styled.Keyed exposing (call_, moduleName_, node, ol, ul, values_)

{-| 
@docs moduleName_, node, ol, ul, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Html", "Styled", "Keyed" ]


{-| Works just like `Html.node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.

node: 
    String
    -> List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
node : String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
node nodeArg nodeArg0 nodeArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
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
        [ Elm.string nodeArg, Elm.list nodeArg0, Elm.list nodeArg1 ]


{-| ol: 
    List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
ol : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ol olArg olArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
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
        [ Elm.list olArg, Elm.list olArg0 ]


{-| ul: 
    List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
ul : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ul ulArg ulArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
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
        [ Elm.list ulArg, Elm.list ulArg0 ]


call_ :
    { node :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , ol : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ul : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { node =
        \nodeArg nodeArg0 nodeArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Keyed" ]
                    , name = "node"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                        )
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
                [ nodeArg, nodeArg0, nodeArg1 ]
    , ol =
        \olArg olArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Keyed" ]
                    , name = "ol"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                        )
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
                [ olArg, olArg0 ]
    , ul =
        \ulArg ulArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Keyed" ]
                    , name = "ul"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                        )
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
                [ ulArg, ulArg0 ]
    }


values_ : { node : Elm.Expression, ol : Elm.Expression, ul : Elm.Expression }
values_ =
    { node =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ol =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ul =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
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


