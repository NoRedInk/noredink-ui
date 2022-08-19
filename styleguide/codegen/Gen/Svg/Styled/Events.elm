module Gen.Svg.Styled.Events exposing (call_, custom, moduleName_, on, onClick, onMouseDown, onMouseOut, onMouseOver, onMouseUp, preventDefaultOn, stopPropagationOn, values_)

{-| 
@docs moduleName_, onClick, onMouseDown, onMouseUp, onMouseOver, onMouseOut, on, stopPropagationOn, preventDefaultOn, custom, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Svg", "Styled", "Events" ]


{-| onClick: msg -> VirtualDom.Styled.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| onMouseDown: msg -> VirtualDom.Styled.Attribute msg -}
onMouseDown : Elm.Expression -> Elm.Expression
onMouseDown onMouseDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseDownArg ]


{-| onMouseUp: msg -> VirtualDom.Styled.Attribute msg -}
onMouseUp : Elm.Expression -> Elm.Expression
onMouseUp onMouseUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseUpArg ]


{-| onMouseOver: msg -> VirtualDom.Styled.Attribute msg -}
onMouseOver : Elm.Expression -> Elm.Expression
onMouseOver onMouseOverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseOverArg ]


{-| onMouseOut: msg -> VirtualDom.Styled.Attribute msg -}
onMouseOut : Elm.Expression -> Elm.Expression
onMouseOut onMouseOutArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseOutArg ]


{-| Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Decode

    onClick : msg -> Attribute msg
    onClick message =
        on "click" (Decode.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.

The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.

If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really helps!

[aEL]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
[decoder]: /packages/elm/json/latest/Json-Decode
[tutorial]: https://github.com/evancz/elm-architecture-tutorial/

**Note:** This creates a [passive] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md

on: String -> Json.Decode.Decoder msg -> VirtualDom.Styled.Attribute msg
-}
on : String -> Elm.Expression -> Elm.Expression
on onArg onArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "on"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string onArg, onArg0 ]


{-| Create an event listener that may [`stopPropagation`][stop]. Your decoder
must produce a message and a `Bool` that decides if `stopPropagation` should
be called.

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation

**Note:** This creates a [passive] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md

stopPropagationOn: String -> Json.Decode.Decoder ( msg, Bool ) -> VirtualDom.Styled.Attribute msg
-}
stopPropagationOn : String -> Elm.Expression -> Elm.Expression
stopPropagationOn stopPropagationOnArg stopPropagationOnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "stopPropagationOn"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.tuple (Type.var "msg") Type.bool ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stopPropagationOnArg, stopPropagationOnArg0 ]


{-| Create an event listener that may [`preventDefault`][prevent]. Your decoder
must produce a message and a `Bool` that decides if `preventDefault` should
be called.

For example, the `onSubmit` function in this library _always_ prevents the
default behavior:

[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

    onSubmit : msg -> Attribute msg
    onSubmit msg =
        preventDefaultOn "submit" (Json.map alwaysPreventDefault (Json.succeed msg))

    alwaysPreventDefault : msg -> ( msg, Bool )
    alwaysPreventDefault msg =
        ( msg, True )

preventDefaultOn: String -> Json.Decode.Decoder ( msg, Bool ) -> VirtualDom.Styled.Attribute msg
-}
preventDefaultOn : String -> Elm.Expression -> Elm.Expression
preventDefaultOn preventDefaultOnArg preventDefaultOnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "preventDefaultOn"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.tuple (Type.var "msg") Type.bool ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string preventDefaultOnArg, preventDefaultOnArg0 ]


{-| Create an event listener that may [`stopPropagation`][stop] or
[`preventDefault`][prevent].

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation
[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

**Note:** If you need something even more custom (like capture phase) check
out the lower-level event API in `elm/virtual-dom`.

custom: 
    String
    -> Json.Decode.Decoder { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }
    -> VirtualDom.Styled.Attribute msg
-}
custom : String -> Elm.Expression -> Elm.Expression
custom customArg customArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.record
                                [ ( "message", Type.var "msg" )
                                , ( "stopPropagation", Type.bool )
                                , ( "preventDefault", Type.bool )
                                ]
                            ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string customArg, customArg0 ]


call_ :
    { onClick : Elm.Expression -> Elm.Expression
    , onMouseDown : Elm.Expression -> Elm.Expression
    , onMouseUp : Elm.Expression -> Elm.Expression
    , onMouseOver : Elm.Expression -> Elm.Expression
    , onMouseOut : Elm.Expression -> Elm.Expression
    , on : Elm.Expression -> Elm.Expression -> Elm.Expression
    , stopPropagationOn : Elm.Expression -> Elm.Expression -> Elm.Expression
    , preventDefaultOn : Elm.Expression -> Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onClickArg ]
    , onMouseDown =
        \onMouseDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "onMouseDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseDownArg ]
    , onMouseUp =
        \onMouseUpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "onMouseUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseUpArg ]
    , onMouseOver =
        \onMouseOverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "onMouseOver"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseOverArg ]
    , onMouseOut =
        \onMouseOutArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "onMouseOut"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseOutArg ]
    , on =
        \onArg onArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "on"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onArg, onArg0 ]
    , stopPropagationOn =
        \stopPropagationOnArg stopPropagationOnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "stopPropagationOn"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.tuple (Type.var "msg") Type.bool ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stopPropagationOnArg, stopPropagationOnArg0 ]
    , preventDefaultOn =
        \preventDefaultOnArg preventDefaultOnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "preventDefaultOn"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.tuple (Type.var "msg") Type.bool ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ preventDefaultOnArg, preventDefaultOnArg0 ]
    , custom =
        \customArg customArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Events" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.record
                                        [ ( "message", Type.var "msg" )
                                        , ( "stopPropagation", Type.bool )
                                        , ( "preventDefault", Type.bool )
                                        ]
                                    ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg, customArg0 ]
    }


values_ :
    { onClick : Elm.Expression
    , onMouseDown : Elm.Expression
    , onMouseUp : Elm.Expression
    , onMouseOver : Elm.Expression
    , onMouseOut : Elm.Expression
    , on : Elm.Expression
    , stopPropagationOn : Elm.Expression
    , preventDefaultOn : Elm.Expression
    , custom : Elm.Expression
    }
values_ =
    { onClick =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseDown =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseUp =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseOver =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseOut =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "onMouseOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , on =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "on"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stopPropagationOn =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "stopPropagationOn"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.tuple (Type.var "msg") Type.bool ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , preventDefaultOn =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "preventDefaultOn"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.tuple (Type.var "msg") Type.bool ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Events" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.record
                                [ ( "message", Type.var "msg" )
                                , ( "stopPropagation", Type.bool )
                                , ( "preventDefault", Type.bool )
                                ]
                            ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


