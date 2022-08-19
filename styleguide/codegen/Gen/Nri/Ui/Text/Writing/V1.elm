module Gen.Nri.Ui.Text.Writing.V1 exposing (call_, footnote, moduleName_, values_)

{-| 
@docs moduleName_, footnote, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Text", "Writing", "V1" ]


{-| This is a little note or footnote.

footnote: List (Html.Styled.Html msg) -> Html.Styled.Html msg
-}
footnote : List Elm.Expression -> Elm.Expression
footnote footnoteArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "Writing", "V1" ]
            , name = "footnote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
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
        [ Elm.list footnoteArg ]


call_ : { footnote : Elm.Expression -> Elm.Expression }
call_ =
    { footnote =
        \footnoteArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Text", "Writing", "V1" ]
                    , name = "footnote"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
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
                [ footnoteArg ]
    }


values_ : { footnote : Elm.Expression }
values_ =
    { footnote =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Text", "Writing", "V1" ]
            , name = "footnote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
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


