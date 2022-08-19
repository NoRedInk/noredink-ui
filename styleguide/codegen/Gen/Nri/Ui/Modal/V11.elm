module Gen.Nri.Ui.Modal.V11 exposing (annotation_, call_, close, closeButton, closeButtonId, css, custom, hideTitle, info, init, isOpen, moduleName_, open, showTitle, subscriptions, testId, update, values_, view, warning)

{-| 
@docs moduleName_, view, closeButton, closeButtonId, init, open, close, update, subscriptions, info, warning, showTitle, hideTitle, testId, css, custom, isOpen, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Modal", "V11" ]


{-| `FocusTrap` comes from `Nri.Ui.FocusTrap.V1`.

view: 
    { title : String
    , wrapMsg : Nri.Ui.Modal.V11.Msg -> msg
    , focusTrap : Nri.Ui.FocusTrap.V1.FocusTrap msg
    , content : List (Accessibility.Styled.Html msg)
    , footer : List (Accessibility.Styled.Html msg)
    }
    -> List Nri.Ui.Modal.V11.Attribute
    -> Nri.Ui.Modal.V11.Model
    -> Accessibility.Styled.Html msg
-}
view :
    { title : String
    , wrapMsg : Elm.Expression -> Elm.Expression
    , focusTrap : Elm.Expression
    , content : List Elm.Expression
    , footer : List Elm.Expression
    }
    -> List Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 viewArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.string )
                            , ( "wrapMsg"
                              , Type.function
                                    [ Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Msg"
                                        []
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "focusTrap"
                              , Type.namedWith
                                    [ "Nri", "Ui", "FocusTrap", "V1" ]
                                    "FocusTrap"
                                    [ Type.var "msg" ]
                              )
                            , ( "content"
                              , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "footer"
                              , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Attribute"
                                []
                            )
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "title" (Elm.string viewArg.title)
            , Tuple.pair
                "wrapMsg"
                (Elm.functionReduced "viewUnpack" viewArg.wrapMsg)
            , Tuple.pair "focusTrap" viewArg.focusTrap
            , Tuple.pair "content" (Elm.list viewArg.content)
            , Tuple.pair "footer" (Elm.list viewArg.footer)
            ]
        , Elm.list viewArg0
        , viewArg1
        ]


{-| Include the close button.

closeButton: Nri.Ui.Modal.V11.Attribute
-}
closeButton : Elm.Expression
closeButton =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "closeButton"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" [])
        }


{-| closeButtonId: String -}
closeButtonId : Elm.Expression
closeButtonId =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "closeButtonId"
        , annotation = Just Type.string
        }


{-| init: Nri.Ui.Modal.V11.Model -}
init : Elm.Expression
init =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "init"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Model" [])
        }


{-| Pass the id of the element that should receive focus when the modal closes.

> ...if a dialog was opened on page load, then focus could be placed on either the body or main element.
> If the trigger was removed from the DOM, then placing focus as close to the trigger’s DOM location would be ideal.

<https://developer.paciellogroup.com/blog/2018/06/the-current-state-of-modal-dialog-accessibility/>

open: 
    { startFocusOn : String, returnFocusTo : String }
    -> ( Nri.Ui.Modal.V11.Model, Platform.Cmd.Cmd Nri.Ui.Modal.V11.Msg )
-}
open : { startFocusOn : String, returnFocusTo : String } -> Elm.Expression
open openArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "open"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "startFocusOn", Type.string )
                            , ( "returnFocusTo", Type.string )
                            ]
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "startFocusOn" (Elm.string openArg.startFocusOn)
            , Tuple.pair "returnFocusTo" (Elm.string openArg.returnFocusTo)
            ]
        ]


{-| close: 
    Nri.Ui.Modal.V11.Model
    -> ( Nri.Ui.Modal.V11.Model, Platform.Cmd.Cmd Nri.Ui.Modal.V11.Msg )
-}
close : Elm.Expression -> Elm.Expression
close closeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "close"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
        )
        [ closeArg ]


{-| update: 
    { dismissOnEscAndOverlayClick : Bool }
    -> Nri.Ui.Modal.V11.Msg
    -> Nri.Ui.Modal.V11.Model
    -> ( Nri.Ui.Modal.V11.Model, Platform.Cmd.Cmd Nri.Ui.Modal.V11.Msg )
-}
update :
    { dismissOnEscAndOverlayClick : Bool }
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
update updateArg updateArg0 updateArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "dismissOnEscAndOverlayClick", Type.bool ) ]
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Msg"
                            []
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "dismissOnEscAndOverlayClick"
                (Elm.bool updateArg.dismissOnEscAndOverlayClick)
            ]
        , updateArg0
        , updateArg1
        ]


{-| Include the subscription if you want the modal to dismiss on `Esc`.

subscriptions: Nri.Ui.Modal.V11.Model -> Platform.Sub.Sub Nri.Ui.Modal.V11.Msg
-}
subscriptions : Elm.Expression -> Elm.Expression
subscriptions subscriptionsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            []
                            "Sub"
                            [ Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Msg"
                                []
                            ]
                        )
                    )
            }
        )
        [ subscriptionsArg ]


{-| This is the default theme.

info: Nri.Ui.Modal.V11.Attribute
-}
info : Elm.Expression
info =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "info"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" [])
        }


{-| warning: Nri.Ui.Modal.V11.Attribute -}
warning : Elm.Expression
warning =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "warning"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" [])
        }


{-| This is the default setting.

showTitle: Nri.Ui.Modal.V11.Attribute
-}
showTitle : Elm.Expression
showTitle =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "showTitle"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" [])
        }


{-| hideTitle: Nri.Ui.Modal.V11.Attribute -}
hideTitle : Elm.Expression
hideTitle =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
        , name = "hideTitle"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" [])
        }


{-| testId: String -> Nri.Ui.Modal.V11.Attribute -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| css: List Css.Style -> Nri.Ui.Modal.V11.Attribute -}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Do NOT use this function for attaching styles -- use the `css` helper instead.

    import Html.Styled.Attribute exposing (id)

    Modal.view
        { title = "Some Great Modal"
        , wrapMsg = ModalMsg
        , content = []
        , footer = []
        }
        [ Modal.custom [ id "my-modal" ]]
        modalState

custom: List (Accessibility.Styled.Attribute Basics.Never) -> Nri.Ui.Modal.V11.Attribute
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| isOpen: Nri.Ui.Modal.V11.Model -> Bool -}
isOpen : Elm.Expression -> Elm.Expression
isOpen isOpenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "isOpen"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        Type.bool
                    )
            }
        )
        [ isOpenArg ]


annotation_ :
    { model : Type.Annotation
    , msg : Type.Annotation
    , attribute : Type.Annotation
    }
annotation_ =
    { model = Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Model" []
    , msg = Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Msg" []
    , attribute = Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Attribute" []
    }


call_ :
    { view :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , open : Elm.Expression -> Elm.Expression
    , close : Elm.Expression -> Elm.Expression
    , update :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , subscriptions : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , isOpen : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 viewArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "title", Type.string )
                                    , ( "wrapMsg"
                                      , Type.function
                                            [ Type.namedWith
                                                [ "Nri", "Ui", "Modal", "V11" ]
                                                "Msg"
                                                []
                                            ]
                                            (Type.var "msg")
                                      )
                                    , ( "focusTrap"
                                      , Type.namedWith
                                            [ "Nri", "Ui", "FocusTrap", "V1" ]
                                            "FocusTrap"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "content"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Accessibility", "Styled" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "footer"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Accessibility", "Styled" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Attribute"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0, viewArg1 ]
    , open =
        \openArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "open"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "startFocusOn", Type.string )
                                    , ( "returnFocusTo", Type.string )
                                    ]
                                ]
                                (Type.tuple
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Model"
                                        []
                                    )
                                    (Type.namedWith
                                        []
                                        "Cmd"
                                        [ Type.namedWith
                                            [ "Nri", "Ui", "Modal", "V11" ]
                                            "Msg"
                                            []
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ openArg ]
    , close =
        \closeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "close"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Model"
                                    []
                                ]
                                (Type.tuple
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Model"
                                        []
                                    )
                                    (Type.namedWith
                                        []
                                        "Cmd"
                                        [ Type.namedWith
                                            [ "Nri", "Ui", "Modal", "V11" ]
                                            "Msg"
                                            []
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ closeArg ]
    , update =
        \updateArg updateArg0 updateArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "update"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "dismissOnEscAndOverlayClick"
                                      , Type.bool
                                      )
                                    ]
                                , Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                , Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Model"
                                    []
                                ]
                                (Type.tuple
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Model"
                                        []
                                    )
                                    (Type.namedWith
                                        []
                                        "Cmd"
                                        [ Type.namedWith
                                            [ "Nri", "Ui", "Modal", "V11" ]
                                            "Msg"
                                            []
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ updateArg, updateArg0, updateArg1 ]
    , subscriptions =
        \subscriptionsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "subscriptions"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Model"
                                    []
                                ]
                                (Type.namedWith
                                    []
                                    "Sub"
                                    [ Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Msg"
                                        []
                                    ]
                                )
                            )
                    }
                )
                [ subscriptionsArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ testIdArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ cssArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Attribute"
                                    []
                                )
                            )
                    }
                )
                [ customArg ]
    , isOpen =
        \isOpenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
                    , name = "isOpen"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Model"
                                    []
                                ]
                                Type.bool
                            )
                    }
                )
                [ isOpenArg ]
    }


values_ :
    { view : Elm.Expression
    , closeButton : Elm.Expression
    , closeButtonId : Elm.Expression
    , init : Elm.Expression
    , open : Elm.Expression
    , close : Elm.Expression
    , update : Elm.Expression
    , subscriptions : Elm.Expression
    , info : Elm.Expression
    , warning : Elm.Expression
    , showTitle : Elm.Expression
    , hideTitle : Elm.Expression
    , testId : Elm.Expression
    , css : Elm.Expression
    , custom : Elm.Expression
    , isOpen : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "title", Type.string )
                            , ( "wrapMsg"
                              , Type.function
                                    [ Type.namedWith
                                        [ "Nri", "Ui", "Modal", "V11" ]
                                        "Msg"
                                        []
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "focusTrap"
                              , Type.namedWith
                                    [ "Nri", "Ui", "FocusTrap", "V1" ]
                                    "FocusTrap"
                                    [ Type.var "msg" ]
                              )
                            , ( "content"
                              , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "footer"
                              , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Attribute"
                                []
                            )
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , closeButton =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "closeButton"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Modal", "V11" ]
                        "Attribute"
                        []
                    )
            }
    , closeButtonId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "closeButtonId"
            , annotation = Just Type.string
            }
    , init =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "init"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Modal", "V11" ] "Model" [])
            }
    , open =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "open"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "startFocusOn", Type.string )
                            , ( "returnFocusTo", Type.string )
                            ]
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
    , close =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "close"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
    , update =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "dismissOnEscAndOverlayClick", Type.bool ) ]
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Msg"
                            []
                        , Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Model"
                                []
                            )
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Modal", "V11" ]
                                    "Msg"
                                    []
                                ]
                            )
                        )
                    )
            }
    , subscriptions =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        (Type.namedWith
                            []
                            "Sub"
                            [ Type.namedWith
                                [ "Nri", "Ui", "Modal", "V11" ]
                                "Msg"
                                []
                            ]
                        )
                    )
            }
    , info =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "info"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Modal", "V11" ]
                        "Attribute"
                        []
                    )
            }
    , warning =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "warning"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Modal", "V11" ]
                        "Attribute"
                        []
                    )
            }
    , showTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "showTitle"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Modal", "V11" ]
                        "Attribute"
                        []
                    )
            }
    , hideTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "hideTitle"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Modal", "V11" ]
                        "Attribute"
                        []
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Attribute"
                            []
                        )
                    )
            }
    , isOpen =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Modal", "V11" ]
            , name = "isOpen"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Modal", "V11" ]
                            "Model"
                            []
                        ]
                        Type.bool
                    )
            }
    }


