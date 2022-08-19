module Gen.Svg.Styled.Keyed exposing (call_, moduleName_, node, values_)

{-| 
@docs moduleName_, node, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Svg", "Styled", "Keyed" ]


{-| Works just like `Svg.node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.

node: 
    String
    -> List (Svg.Styled.Attribute msg)
    -> List ( String, Svg.Styled.Svg msg )
    -> Svg.Styled.Svg msg
-}
node : String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
node nodeArg nodeArg0 nodeArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Keyed" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nodeArg, Elm.list nodeArg0, Elm.list nodeArg1 ]


call_ :
    { node :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { node =
        \nodeArg nodeArg0 nodeArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Keyed" ]
                    , name = "node"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Svg", "Styled" ]
                                            "Svg"
                                            [ Type.var "msg" ]
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nodeArg, nodeArg0, nodeArg1 ]
    }


values_ : { node : Elm.Expression }
values_ =
    { node =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Keyed" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


