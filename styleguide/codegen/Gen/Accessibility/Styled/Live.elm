module Gen.Accessibility.Styled.Live exposing (atomic, busy, call_, liveAssertive, livePolite, moduleName_, relevantAdditions, relevantAdditionsText, relevantAll, relevantRemovals, relevantText, values_)

{-| 
@docs moduleName_, atomic, busy, livePolite, liveAssertive, relevantAdditions, relevantAdditionsText, relevantAll, relevantRemovals, relevantText, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Live" ]


{-| Supported for all elements.

This property indicates that a region is live, and may change even when the region doesn't have focus. When `True`, all the contents of the element will be presented to the user.

atomic: Bool -> Html.Styled.Attribute msg
-}
atomic : Bool -> Elm.Expression
atomic atomicArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "atomic"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool atomicArg ]


{-| Supported for all elements.

When set to `True`, this is the aria equivalent of a loading spinner--indicates that stuff is changing/is not ready for interaction/reading-off yet.

busy: Bool -> Html.Styled.Attribute msg
-}
busy : Bool -> Elm.Expression
busy busyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "busy"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool busyArg ]


{-| Supported by all elements.

When the region's contents change, assistive technologies will wait for a good moment to interrupt and do so politely with the update.

livePolite: Html.Styled.Attribute msg
-}
livePolite : Elm.Expression
livePolite =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "livePolite"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by all elements.

Updates to the region will cause the assistive technologies to immediately interrupt the user with the big news.

liveAssertive: Html.Styled.Attribute msg
-}
liveAssertive : Elm.Expression
liveAssertive =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "liveAssertive"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by live regions.

Keep track of additions to the live region.

relevantAdditions: Html.Styled.Attribute msg
-}
relevantAdditions : Elm.Expression
relevantAdditions =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "relevantAdditions"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by live regions.

Keep track of node additions to the live region and text additions.

relevantAdditionsText: Html.Styled.Attribute msg
-}
relevantAdditionsText : Elm.Expression
relevantAdditionsText =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "relevantAdditionsText"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by live regions.

Keep track of everything to occur in the live region. Use sparingly!

relevantAll: Html.Styled.Attribute msg
-}
relevantAll : Elm.Expression
relevantAll =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "relevantAll"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by live regions.

Keep track of text or node removals. Use sparingly!

relevantRemovals: Html.Styled.Attribute msg
-}
relevantRemovals : Elm.Expression
relevantRemovals =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "relevantRemovals"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by live regions.

Keep track of text additions to the live region.

relevantText: Html.Styled.Attribute msg
-}
relevantText : Elm.Expression
relevantText =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Live" ]
        , name = "relevantText"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


call_ :
    { atomic : Elm.Expression -> Elm.Expression
    , busy : Elm.Expression -> Elm.Expression
    }
call_ =
    { atomic =
        \atomicArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Live" ]
                    , name = "atomic"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ atomicArg ]
    , busy =
        \busyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Live" ]
                    , name = "busy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ busyArg ]
    }


values_ :
    { atomic : Elm.Expression
    , busy : Elm.Expression
    , livePolite : Elm.Expression
    , liveAssertive : Elm.Expression
    , relevantAdditions : Elm.Expression
    , relevantAdditionsText : Elm.Expression
    , relevantAll : Elm.Expression
    , relevantRemovals : Elm.Expression
    , relevantText : Elm.Expression
    }
values_ =
    { atomic =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "atomic"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , busy =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "busy"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , livePolite =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "livePolite"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , liveAssertive =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "liveAssertive"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , relevantAdditions =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "relevantAdditions"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , relevantAdditionsText =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "relevantAdditionsText"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , relevantAll =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "relevantAll"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , relevantRemovals =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "relevantRemovals"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , relevantText =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Live" ]
            , name = "relevantText"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    }


