module Gen.Accessibility.Styled.Key exposing (call_, down, enter, escape, left, moduleName_, onKeyDown, right, space, tab, tabBack, tabbable, up, values_)

{-| 
@docs moduleName_, tabbable, onKeyDown, tab, tabBack, up, right, down, left, enter, space, escape, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Key" ]


{-| Add or remove an element from the normal flow of tabbable/focusable elements.

`tabbable True` will set the tabindex to 0, and `tabbable False` will set the tabindex to -1.

You may use `Html.Styled.Attributes.tabindex` if you need to control the tab order more explicitly, but you may want to restructure your HTML to match how you want users to interact with it instead. If you're considering changing tabindex or restructuring your HTML, read [Understanding Success Criterion 1.3.2: Meaningful Sequence](https://www.w3.org/WAI/WCAG21/Understanding/meaningful-sequence).

tabbable: Bool -> Html.Styled.Attribute msg
-}
tabbable : Bool -> Elm.Expression
tabbable tabbableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tabbable"
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
        [ Elm.bool tabbableArg ]


{-| Pass a list of decoders.

    onKeyDown [ enter TheyHitEnterDoSomething, left DoSomeOtherThing ]

onKeyDown: List (Json.Decode.Decoder msg) -> Html.Styled.Attribute msg
-}
onKeyDown : List Elm.Expression -> Elm.Expression
onKeyDown onKeyDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "onKeyDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Json", "Decode" ]
                                "Decoder"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list onKeyDownArg ]


{-| Use with `onKeyDown` to succeed when user hits the tab key.

    onKeyDown [ tab Tab ]

tab: msg -> Json.Decode.Decoder msg
-}
tab : Elm.Expression -> Elm.Expression
tab tabArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tab"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ tabArg ]


{-| Use with `onKeyDown` to succeed when user hits the tab key while hitting shift.

    onKeyDown [ tabBack GoBack ]

tabBack: msg -> Json.Decode.Decoder msg
-}
tabBack : Elm.Expression -> Elm.Expression
tabBack tabBackArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tabBack"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ tabBackArg ]


{-| Use with `onKeyDown` to succeed when user hits the up arrow key.

    onKeyDown [ up Up ]

up: msg -> Json.Decode.Decoder msg
-}
up : Elm.Expression -> Elm.Expression
up upArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "up"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ upArg ]


{-| Use with `onKeyDown` to succeed when user hits the right arrow key.

    onKeyDown [ right Right ]

right: msg -> Json.Decode.Decoder msg
-}
right : Elm.Expression -> Elm.Expression
right rightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "right"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ rightArg ]


{-| Use with `onKeyDown` to succeed when user hits the down arrow key.

    onKeyDown [ down Down ]

down: msg -> Json.Decode.Decoder msg
-}
down : Elm.Expression -> Elm.Expression
down downArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "down"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ downArg ]


{-| Use with `onKeyDown` to succeed when user hits the left arrow key.

    onKeyDown [ left Left ]

left: msg -> Json.Decode.Decoder msg
-}
left : Elm.Expression -> Elm.Expression
left leftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "left"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ leftArg ]


{-| Use with `onKeyDown` to succeed when user hits the Enter key.

    onKeyDown [ enter TheyHitEnterDoSomething ]

enter: msg -> Json.Decode.Decoder msg
-}
enter : Elm.Expression -> Elm.Expression
enter enterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "enter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ enterArg ]


{-| Use with `onKeyDown` to succeed when user hits the spacebar.

    onKeyDown [ space SpaceBar ]

space: msg -> Json.Decode.Decoder msg
-}
space : Elm.Expression -> Elm.Expression
space spaceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "space"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ spaceArg ]


{-| Use with `onKeyDown` to succeed when user hits `esc`.

    onKeyDown [ escape CloseModal ]

escape: msg -> Json.Decode.Decoder msg
-}
escape : Elm.Expression -> Elm.Expression
escape escapeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "escape"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ escapeArg ]


call_ :
    { tabbable : Elm.Expression -> Elm.Expression
    , onKeyDown : Elm.Expression -> Elm.Expression
    , tab : Elm.Expression -> Elm.Expression
    , tabBack : Elm.Expression -> Elm.Expression
    , up : Elm.Expression -> Elm.Expression
    , right : Elm.Expression -> Elm.Expression
    , down : Elm.Expression -> Elm.Expression
    , left : Elm.Expression -> Elm.Expression
    , enter : Elm.Expression -> Elm.Expression
    , space : Elm.Expression -> Elm.Expression
    , escape : Elm.Expression -> Elm.Expression
    }
call_ =
    { tabbable =
        \tabbableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "tabbable"
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
                [ tabbableArg ]
    , onKeyDown =
        \onKeyDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "onKeyDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Json", "Decode" ]
                                        "Decoder"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onKeyDownArg ]
    , tab =
        \tabArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "tab"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabArg ]
    , tabBack =
        \tabBackArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "tabBack"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabBackArg ]
    , up =
        \upArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "up"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ upArg ]
    , right =
        \rightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "right"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rightArg ]
    , down =
        \downArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "down"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ downArg ]
    , left =
        \leftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "left"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ leftArg ]
    , enter =
        \enterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "enter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ enterArg ]
    , space =
        \spaceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "space"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spaceArg ]
    , escape =
        \escapeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Key" ]
                    , name = "escape"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ escapeArg ]
    }


values_ :
    { tabbable : Elm.Expression
    , onKeyDown : Elm.Expression
    , tab : Elm.Expression
    , tabBack : Elm.Expression
    , up : Elm.Expression
    , right : Elm.Expression
    , down : Elm.Expression
    , left : Elm.Expression
    , enter : Elm.Expression
    , space : Elm.Expression
    , escape : Elm.Expression
    }
values_ =
    { tabbable =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tabbable"
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
    , onKeyDown =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "onKeyDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Json", "Decode" ]
                                "Decoder"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tab =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tab"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tabBack =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "tabBack"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , up =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "up"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , right =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "right"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , down =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "down"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , left =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "left"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , enter =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "enter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , space =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "space"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , escape =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Key" ]
            , name = "escape"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


