module Gen.Nri.Ui.DisclosureIndicator.V2 exposing (call_, large, medium, moduleName_, values_)

{-| 
@docs moduleName_, medium, large, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "DisclosureIndicator", "V2" ]


{-| medium: List Css.Style -> Bool -> Nri.Ui.Svg.V1.Svg -}
medium : List Elm.Expression -> Bool -> Elm.Expression
medium mediumArg mediumArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.bool
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.list mediumArg, Elm.bool mediumArg0 ]


{-| large: List Css.Style -> Bool -> Nri.Ui.Svg.V1.Svg -}
large : List Elm.Expression -> Bool -> Elm.Expression
large largeArg largeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
            , name = "large"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.bool
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
        )
        [ Elm.list largeArg, Elm.bool largeArg0 ]


call_ :
    { medium : Elm.Expression -> Elm.Expression -> Elm.Expression
    , large : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { medium =
        \mediumArg mediumArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
                    , name = "medium"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.bool
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ mediumArg, mediumArg0 ]
    , large =
        \largeArg largeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
                    , name = "large"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.bool
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                            )
                    }
                )
                [ largeArg, largeArg0 ]
    }


values_ : { medium : Elm.Expression, large : Elm.Expression }
values_ =
    { medium =
        Elm.value
            { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.bool
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    , large =
        Elm.value
            { importFrom = [ "Nri", "Ui", "DisclosureIndicator", "V2" ]
            , name = "large"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.bool
                        ]
                        (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
                    )
            }
    }


