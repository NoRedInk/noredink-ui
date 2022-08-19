module Gen.Nri.Ui.MediaQuery.V1 exposing (anyMotion, call_, mobile, mobileBreakpoint, moduleName_, narrowMobile, narrowMobileBreakPoint, notMobile, prefersReducedMotion, quizEngineBreakpoint, quizEngineMobile, values_)

{-| 
@docs moduleName_, anyMotion, prefersReducedMotion, mobile, notMobile, mobileBreakpoint, quizEngineMobile, quizEngineBreakpoint, narrowMobile, narrowMobileBreakPoint, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "MediaQuery", "V1" ]


{-| anyMotion: List Css.Style -> Css.Style -}
anyMotion : List Elm.Expression -> Elm.Expression
anyMotion anyMotionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "anyMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list anyMotionArg ]


{-| prefersReducedMotion: List Css.Style -> Css.Style -}
prefersReducedMotion : List Elm.Expression -> Elm.Expression
prefersReducedMotion prefersReducedMotionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "prefersReducedMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list prefersReducedMotionArg ]


{-| Styles using the `mobileBreakpoint` value as the maxWidth.

mobile: Css.Media.MediaQuery
-}
mobile : Elm.Expression
mobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "mobile"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
        }


{-| Styles using the `mobileBreakpoint` value as the minWidth.

notMobile: Css.Media.MediaQuery
-}
notMobile : Elm.Expression
notMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "notMobile"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
        }


{-| 1000px

mobileBreakpoint: Css.Px
-}
mobileBreakpoint : Elm.Expression
mobileBreakpoint =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "mobileBreakpoint"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| Styles using the `quizEngineBreakpoint` value as the maxWidth.

quizEngineMobile: Css.Media.MediaQuery
-}
quizEngineMobile : Elm.Expression
quizEngineMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "quizEngineMobile"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
        }


{-| 750px

quizEngineBreakpoint: Css.Px
-}
quizEngineBreakpoint : Elm.Expression
quizEngineBreakpoint =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "quizEngineBreakpoint"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


{-| Styles using the `narrowMobileBreakPoint` value as the maxWidth

narrowMobile: Css.Media.MediaQuery
-}
narrowMobile : Elm.Expression
narrowMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "narrowMobile"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
        }


{-| 500px

narrowMobileBreakPoint: Css.Px
-}
narrowMobileBreakPoint : Elm.Expression
narrowMobileBreakPoint =
    Elm.value
        { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
        , name = "narrowMobileBreakPoint"
        , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
        }


call_ :
    { anyMotion : Elm.Expression -> Elm.Expression
    , prefersReducedMotion : Elm.Expression -> Elm.Expression
    }
call_ =
    { anyMotion =
        \anyMotionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
                    , name = "anyMotion"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ anyMotionArg ]
    , prefersReducedMotion =
        \prefersReducedMotionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
                    , name = "prefersReducedMotion"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ prefersReducedMotionArg ]
    }


values_ :
    { anyMotion : Elm.Expression
    , prefersReducedMotion : Elm.Expression
    , mobile : Elm.Expression
    , notMobile : Elm.Expression
    , mobileBreakpoint : Elm.Expression
    , quizEngineMobile : Elm.Expression
    , quizEngineBreakpoint : Elm.Expression
    , narrowMobile : Elm.Expression
    , narrowMobileBreakPoint : Elm.Expression
    }
values_ =
    { anyMotion =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "anyMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , prefersReducedMotion =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "prefersReducedMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , mobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "mobile"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
            }
    , notMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "notMobile"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
            }
    , mobileBreakpoint =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "mobileBreakpoint"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , quizEngineMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "quizEngineMobile"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
            }
    , quizEngineBreakpoint =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "quizEngineBreakpoint"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    , narrowMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "narrowMobile"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
            }
    , narrowMobileBreakPoint =
        Elm.value
            { importFrom = [ "Nri", "Ui", "MediaQuery", "V1" ]
            , name = "narrowMobileBreakPoint"
            , annotation = Just (Type.namedWith [ "Css" ] "Px" [])
            }
    }


