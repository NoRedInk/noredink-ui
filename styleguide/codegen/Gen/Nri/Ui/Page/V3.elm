module Gen.Nri.Ui.Page.V3 exposing (annotation_, blocked, blockedV4, broken, call_, caseOf_, httpError, loggedOut, make_, moduleName_, networkError, noPermission, notFound, timeOut, values_)

{-| 
@docs moduleName_, httpError, broken, blockedV4, blocked, notFound, noPermission, loggedOut, timeOut, networkError, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Page", "V3" ]


{-| httpError: Http.Error -> Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg -}
httpError : Elm.Expression -> Elm.Expression -> Elm.Expression
httpError httpErrorArg httpErrorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "httpError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Http" ] "Error" []
                        , Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ httpErrorArg, httpErrorArg0 ]


{-| For HTTP errors and other broken states.

broken: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
broken : Elm.Expression -> Elm.Expression
broken brokenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "broken"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ brokenArg ]


{-| Error page with details for engineers.

blockedV4: String -> Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
blockedV4 : String -> Elm.Expression -> Elm.Expression
blockedV4 blockedV4Arg blockedV4Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "blockedV4"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string blockedV4Arg, blockedV4Arg0 ]


{-| DEPRECATED: please use blockedV4.

For HTTP errors and other broken states, where link goes to "/".

blocked: String -> Html.Styled.Html msg
-}
blocked : String -> Elm.Expression
blocked blockedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "blocked"
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
        [ Elm.string blockedArg ]


{-| For the not found page.

notFound: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
notFound : Elm.Expression -> Elm.Expression
notFound notFoundArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "notFound"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ notFoundArg ]


{-| For pages the user does not have access to.

noPermission: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
noPermission : Elm.Expression -> Elm.Expression
noPermission noPermissionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "noPermission"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ noPermissionArg ]


{-| When the user has been logged out.

loggedOut: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
loggedOut : Elm.Expression -> Elm.Expression
loggedOut loggedOutArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "loggedOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ loggedOutArg ]


{-| When a request takes too long to complete.

timeOut: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
timeOut : Elm.Expression -> Elm.Expression
timeOut timeOutArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "timeOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ timeOutArg ]


{-| When a request fails due to a connectivity failure.

networkError: Nri.Ui.Page.V3.DefaultPage msg -> Html.Styled.Html msg
-}
networkError : Elm.Expression -> Elm.Expression
networkError networkErrorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "networkError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ networkErrorArg ]


annotation_ :
    { defaultPage : Type.Annotation -> Type.Annotation
    , recoveryText : Type.Annotation
    }
annotation_ =
    { defaultPage =
        \defaultPageArg0 ->
            Type.alias
                moduleName_
                "DefaultPage"
                [ defaultPageArg0 ]
                (Type.record
                    [ ( "link", Type.var "msg" )
                    , ( "recoveryText"
                      , Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "RecoveryText"
                            []
                      )
                    ]
                )
    , recoveryText =
        Type.namedWith [ "Nri", "Ui", "Page", "V3" ] "RecoveryText" []
    }


make_ :
    { defaultPage :
        { link : Elm.Expression, recoveryText : Elm.Expression }
        -> Elm.Expression
    , returnTo : Elm.Expression -> Elm.Expression
    , reload : Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    }
make_ =
    { defaultPage =
        \defaultPage_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Page", "V3" ]
                    "DefaultPage"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "link", Type.var "msg" )
                        , ( "recoveryText"
                          , Type.namedWith
                                [ "Nri", "Ui", "Page", "V3" ]
                                "RecoveryText"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "link" defaultPage_args.link
                    , Tuple.pair "recoveryText" defaultPage_args.recoveryText
                    ]
                )
    , returnTo =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "ReturnTo"
                    , annotation = Just (Type.namedWith [] "RecoveryText" [])
                    }
                )
                [ ar0 ]
    , reload =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "Reload"
            , annotation = Just (Type.namedWith [] "RecoveryText" [])
            }
    , custom =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "Custom"
                    , annotation = Just (Type.namedWith [] "RecoveryText" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { recoveryText :
        Elm.Expression
        -> { recoveryTextTags_0_0
            | returnTo : Elm.Expression -> Elm.Expression
            , reload : Elm.Expression
            , custom : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { recoveryText =
        \recoveryTextExpression recoveryTextTags ->
            Elm.Case.custom
                recoveryTextExpression
                (Type.namedWith [ "Nri", "Ui", "Page", "V3" ] "RecoveryText" [])
                [ Elm.Case.branch1
                    "ReturnTo"
                    ( "string.String", Type.string )
                    recoveryTextTags.returnTo
                , Elm.Case.branch0 "Reload" recoveryTextTags.reload
                , Elm.Case.branch1
                    "Custom"
                    ( "string.String", Type.string )
                    recoveryTextTags.custom
                ]
    }


call_ :
    { httpError : Elm.Expression -> Elm.Expression -> Elm.Expression
    , broken : Elm.Expression -> Elm.Expression
    , blockedV4 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , blocked : Elm.Expression -> Elm.Expression
    , notFound : Elm.Expression -> Elm.Expression
    , noPermission : Elm.Expression -> Elm.Expression
    , loggedOut : Elm.Expression -> Elm.Expression
    , timeOut : Elm.Expression -> Elm.Expression
    , networkError : Elm.Expression -> Elm.Expression
    }
call_ =
    { httpError =
        \httpErrorArg httpErrorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "httpError"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Http" ] "Error" []
                                , Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ httpErrorArg, httpErrorArg0 ]
    , broken =
        \brokenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "broken"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ brokenArg ]
    , blockedV4 =
        \blockedV4Arg blockedV4Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "blockedV4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ blockedV4Arg, blockedV4Arg0 ]
    , blocked =
        \blockedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "blocked"
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
                [ blockedArg ]
    , notFound =
        \notFoundArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "notFound"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ notFoundArg ]
    , noPermission =
        \noPermissionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "noPermission"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ noPermissionArg ]
    , loggedOut =
        \loggedOutArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "loggedOut"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ loggedOutArg ]
    , timeOut =
        \timeOutArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "timeOut"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ timeOutArg ]
    , networkError =
        \networkErrorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Page", "V3" ]
                    , name = "networkError"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Page", "V3" ]
                                    "DefaultPage"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ networkErrorArg ]
    }


values_ :
    { httpError : Elm.Expression
    , broken : Elm.Expression
    , blockedV4 : Elm.Expression
    , blocked : Elm.Expression
    , notFound : Elm.Expression
    , noPermission : Elm.Expression
    , loggedOut : Elm.Expression
    , timeOut : Elm.Expression
    , networkError : Elm.Expression
    }
values_ =
    { httpError =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "httpError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Http" ] "Error" []
                        , Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , broken =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "broken"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , blockedV4 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "blockedV4"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , blocked =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "blocked"
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
    , notFound =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "notFound"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , noPermission =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "noPermission"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , loggedOut =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "loggedOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , timeOut =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "timeOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , networkError =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Page", "V3" ]
            , name = "networkError"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Page", "V3" ]
                            "DefaultPage"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


