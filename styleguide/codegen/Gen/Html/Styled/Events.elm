module Gen.Html.Styled.Events exposing (call_, custom, keyCode, moduleName_, on, onBlur, onCheck, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver, onMouseUp, onSubmit, preventDefaultOn, stopPropagationOn, targetChecked, targetValue, values_)

{-| 
@docs moduleName_, onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseOver, onMouseOut, onInput, onCheck, onSubmit, onBlur, onFocus, on, stopPropagationOn, preventDefaultOn, custom, targetValue, targetChecked, keyCode, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Html", "Styled", "Events" ]


{-| onClick: msg -> Html.Styled.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| onDoubleClick: msg -> Html.Styled.Attribute msg -}
onDoubleClick : Elm.Expression -> Elm.Expression
onDoubleClick onDoubleClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onDoubleClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onDoubleClickArg ]


{-| onMouseDown: msg -> Html.Styled.Attribute msg -}
onMouseDown : Elm.Expression -> Elm.Expression
onMouseDown onMouseDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseDownArg ]


{-| onMouseUp: msg -> Html.Styled.Attribute msg -}
onMouseUp : Elm.Expression -> Elm.Expression
onMouseUp onMouseUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseUpArg ]


{-| onMouseEnter: msg -> Html.Styled.Attribute msg -}
onMouseEnter : Elm.Expression -> Elm.Expression
onMouseEnter onMouseEnterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseEnterArg ]


{-| onMouseLeave: msg -> Html.Styled.Attribute msg -}
onMouseLeave : Elm.Expression -> Elm.Expression
onMouseLeave onMouseLeaveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseLeave"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseLeaveArg ]


{-| onMouseOver: msg -> Html.Styled.Attribute msg -}
onMouseOver : Elm.Expression -> Elm.Expression
onMouseOver onMouseOverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseOverArg ]


{-| onMouseOut: msg -> Html.Styled.Attribute msg -}
onMouseOut : Elm.Expression -> Elm.Expression
onMouseOut onMouseOutArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseOutArg ]


{-| Detect [input](https://developer.mozilla.org/en-US/docs/Web/Events/input)
events for things like text fields or text areas.

For more details on how `onInput` works, check out [`targetValue`](#targetValue).

**Note 1:** It grabs the **string** value at `event.target.value`, so it will
not work if you need some other information. For example, if you want to track
inputs on a range slider, make a custom handler with [`on`](#on).

**Note 2:** It uses `stopPropagationOn` internally to always stop propagation
of the event. This is important for complicated reasons explained [here][1] and
[here][2].

[1]: /packages/elm/virtual-dom/latest/VirtualDom#Handler
[2]: https://github.com/elm/virtual-dom/issues/125

onInput: (String -> msg) -> Html.Styled.Attribute msg
-}
onInput : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onInput onInputArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onInput"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "onInputUnpack" onInputArg ]


{-| Detect [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)
events on checkboxes. It will grab the boolean value from `event.target.checked`
on any input event.

Check out [`targetChecked`](#targetChecked) for more details on how this works.

onCheck: (Bool -> msg) -> Html.Styled.Attribute msg
-}
onCheck : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onCheck onCheckArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onCheck"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "onCheckUnpack" onCheckArg ]


{-| Detect a [submit](https://developer.mozilla.org/en-US/docs/Web/Events/submit)
event with [`preventDefault`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault)
in order to prevent the form from changing the page’s location. If you need
different behavior, create a custom event handler.

onSubmit: msg -> Html.Styled.Attribute msg
-}
onSubmit : Elm.Expression -> Elm.Expression
onSubmit onSubmitArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onSubmit"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onSubmitArg ]


{-| onBlur: msg -> Html.Styled.Attribute msg -}
onBlur : Elm.Expression -> Elm.Expression
onBlur onBlurArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onBlurArg ]


{-| onFocus: msg -> Html.Styled.Attribute msg -}
onFocus : Elm.Expression -> Elm.Expression
onFocus onFocusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onFocusArg ]


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

on: String -> Json.Decode.Decoder msg -> Html.Styled.Attribute msg
-}
on : String -> Elm.Expression -> Elm.Expression
on onArg onArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
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

stopPropagationOn: String -> Json.Decode.Decoder ( msg, Bool ) -> Html.Styled.Attribute msg
-}
stopPropagationOn : String -> Elm.Expression -> Elm.Expression
stopPropagationOn stopPropagationOnArg stopPropagationOnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
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

preventDefaultOn: String -> Json.Decode.Decoder ( msg, Bool ) -> Html.Styled.Attribute msg
-}
preventDefaultOn : String -> Elm.Expression -> Elm.Expression
preventDefaultOn preventDefaultOnArg preventDefaultOnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
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
    -> Html.Styled.Attribute msg
-}
custom : String -> Elm.Expression -> Elm.Expression
custom customArg customArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string customArg, customArg0 ]


{-| A `Json.Decoder` for grabbing `event.target.value`. We use this to define
`onInput` as follows:

    import Json.Decode as Json

    onInput : (String -> msg) -> Attribute msg
    onInput tagger =
        stopPropagationOn "input" <|
            Json.map alwaysStop (Json.map tagger targetValue)

    alwaysStop : a -> ( a, Bool )
    alwaysStop x =
        ( x, True )

You probably will never need this, but hopefully it gives some insights into
how to make custom event handlers.

targetValue: Json.Decode.Decoder String
-}
targetValue : Elm.Expression
targetValue =
    Elm.value
        { importFrom = [ "Html", "Styled", "Events" ]
        , name = "targetValue"
        , annotation =
            Just (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.string ])
        }


{-| A `Json.Decoder` for grabbing `event.target.checked`. We use this to define
`onCheck` as follows:

    import Json.Decode as Json

    onCheck : (Bool -> msg) -> Attribute msg
    onCheck tagger =
        on "input" (Json.map tagger targetChecked)

targetChecked: Json.Decode.Decoder Bool
-}
targetChecked : Elm.Expression
targetChecked =
    Elm.value
        { importFrom = [ "Html", "Styled", "Events" ]
        , name = "targetChecked"
        , annotation =
            Just (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.bool ])
        }


{-| A `Json.Decoder` for grabbing `event.keyCode`. This helps you define
keyboard listeners like this:

    import Json.Decode as Json

    onKeyUp : (Int -> msg) -> Attribute msg
    onKeyUp tagger =
        on "keyup" (Json.map tagger keyCode)

**Note:** It looks like the spec is moving away from `event.keyCode` and
towards `event.key`. Once this is supported in more browsers, we may add
helpers here for `onKeyUp`, `onKeyDown`, `onKeyPress`, etc.

keyCode: Json.Decode.Decoder Int
-}
keyCode : Elm.Expression
keyCode =
    Elm.value
        { importFrom = [ "Html", "Styled", "Events" ]
        , name = "keyCode"
        , annotation =
            Just (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.int ])
        }


call_ :
    { onClick : Elm.Expression -> Elm.Expression
    , onDoubleClick : Elm.Expression -> Elm.Expression
    , onMouseDown : Elm.Expression -> Elm.Expression
    , onMouseUp : Elm.Expression -> Elm.Expression
    , onMouseEnter : Elm.Expression -> Elm.Expression
    , onMouseLeave : Elm.Expression -> Elm.Expression
    , onMouseOver : Elm.Expression -> Elm.Expression
    , onMouseOut : Elm.Expression -> Elm.Expression
    , onInput : Elm.Expression -> Elm.Expression
    , onCheck : Elm.Expression -> Elm.Expression
    , onSubmit : Elm.Expression -> Elm.Expression
    , onBlur : Elm.Expression -> Elm.Expression
    , onFocus : Elm.Expression -> Elm.Expression
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
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onClickArg ]
    , onDoubleClick =
        \onDoubleClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onDoubleClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onDoubleClickArg ]
    , onMouseDown =
        \onMouseDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
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
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseUpArg ]
    , onMouseEnter =
        \onMouseEnterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseEnter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseEnterArg ]
    , onMouseLeave =
        \onMouseLeaveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseLeave"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseLeaveArg ]
    , onMouseOver =
        \onMouseOverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseOver"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
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
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onMouseOut"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseOutArg ]
    , onInput =
        \onInputArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onInput"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onInputArg ]
    , onCheck =
        \onCheckArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onCheck"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.bool ] (Type.var "msg") ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onCheckArg ]
    , onSubmit =
        \onSubmitArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onSubmit"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onSubmitArg ]
    , onBlur =
        \onBlurArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onBlur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onBlurArg ]
    , onFocus =
        \onFocusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
                    , name = "onFocus"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onFocusArg ]
    , on =
        \onArg onArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Events" ]
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
                                    [ "Html", "Styled" ]
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
                    { importFrom = [ "Html", "Styled", "Events" ]
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
                                    [ "Html", "Styled" ]
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
                    { importFrom = [ "Html", "Styled", "Events" ]
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
                                    [ "Html", "Styled" ]
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
                    { importFrom = [ "Html", "Styled", "Events" ]
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
                                    [ "Html", "Styled" ]
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
    , onDoubleClick : Elm.Expression
    , onMouseDown : Elm.Expression
    , onMouseUp : Elm.Expression
    , onMouseEnter : Elm.Expression
    , onMouseLeave : Elm.Expression
    , onMouseOver : Elm.Expression
    , onMouseOut : Elm.Expression
    , onInput : Elm.Expression
    , onCheck : Elm.Expression
    , onSubmit : Elm.Expression
    , onBlur : Elm.Expression
    , onFocus : Elm.Expression
    , on : Elm.Expression
    , stopPropagationOn : Elm.Expression
    , preventDefaultOn : Elm.Expression
    , custom : Elm.Expression
    , targetValue : Elm.Expression
    , targetChecked : Elm.Expression
    , keyCode : Elm.Expression
    }
values_ =
    { onClick =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onDoubleClick =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onDoubleClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseDown =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseUp =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseEnter =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseLeave =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseLeave"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseOver =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseOut =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onMouseOut"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onInput =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onInput"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onCheck =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onCheck"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onSubmit =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onSubmit"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onBlur =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onFocus =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , on =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stopPropagationOn =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , preventDefaultOn =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
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
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , targetValue =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "targetValue"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Json", "Decode" ]
                        "Decoder"
                        [ Type.string ]
                    )
            }
    , targetChecked =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "targetChecked"
            , annotation =
                Just
                    (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.bool ]
                    )
            }
    , keyCode =
        Elm.value
            { importFrom = [ "Html", "Styled", "Events" ]
            , name = "keyCode"
            , annotation =
                Just
                    (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.int ])
            }
    }


