module Gen.Nri.Ui.Divider.V2 exposing (call_, moduleName_, values_, view)

{-| 
@docs moduleName_, view, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Divider", "V2" ]


{-| view: String -> Html.Styled.Html msg -}
view : String -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Divider", "V2" ]
            , name = "view"
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
        [ Elm.string viewArg ]


call_ : { view : Elm.Expression -> Elm.Expression }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Divider", "V2" ]
                    , name = "view"
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
                [ viewArg ]
    }


values_ : { view : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Divider", "V2" ]
            , name = "view"
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


