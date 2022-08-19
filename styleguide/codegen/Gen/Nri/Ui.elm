module Gen.Nri.Ui exposing (call_, moduleName_, styled, values_)

{-| 
@docs moduleName_, styled, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui" ]


{-| Wrapper around [`Html.Styled.style`](http://package.elm-lang.org/packages/rtfeldman/elm-css/13.1.1/Html-Styled#styled) which adds a data-nri-description attribute to make it easier to tell from Inspect Element where in our code that element was defined.

Takes a function that creates an element, and pre-applies styles and a `data-nri-description` attribution to it.

    bigButton : List (Attribute msg) -> List (Html msg) -> Html msg
    bigButton =
        styled button
            "big button"
            [ padding (px 30)
            , fontWeight bold
            ]

    view : Model -> Html msg
    view model =
        [ text "These two buttons are identical:"
        , bigButton [] [ text "Hi!" ]
        , button [ css [ padding (px 30), fontWeight bold ] ] [] [ text "Hi!" ]
        ]

Here, the bigButton function we've defined using styled button is identical to the normal button function, except that it has pre-applied the attribute of css [ padding (px 30), fontWeight bold ], as well as `(attribute "data-nri-description" "big button")`.

You can pass more attributes to bigButton as usual (including other css attributes). They will be applied after the pre-applied styles.

Note: normally `attributeMsg` will be the same as `msg`, but we need them to be different types for special cases when `fn` needs to do tricky things. For example, some elements from the Accessibility.Styled package use the following type signature:

    div : List (Attribute Never) -> List (Html msg) -> Html msg

styled: 
    (List (Html.Styled.Attribute attributeMsg)
    -> List (Html.Styled.Html msg)
    -> Html.Styled.Html msg)
    -> String
    -> List Css.Style
    -> List (Html.Styled.Attribute attributeMsg)
    -> List (Html.Styled.Html msg)
    -> Html.Styled.Html msg
-}
styled :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> String
    -> List Elm.Expression
    -> List Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
styled styledArg styledArg0 styledArg1 styledArg2 styledArg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "attributeMsg" ]
                                )
                            , Type.list
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
                        , Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "attributeMsg" ]
                            )
                        , Type.list
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
        [ Elm.functionReduced
            "styledUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (styledArg functionReducedUnpack)
            )
        , Elm.string styledArg0
        , Elm.list styledArg1
        , Elm.list styledArg2
        , Elm.list styledArg3
        ]


call_ :
    { styled :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    }
call_ =
    { styled =
        \styledArg styledArg0 styledArg1 styledArg2 styledArg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui" ]
                    , name = "styled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Attribute"
                                            [ Type.var "attributeMsg" ]
                                        )
                                    , Type.list
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
                                , Type.string
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "attributeMsg" ]
                                    )
                                , Type.list
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
                [ styledArg, styledArg0, styledArg1, styledArg2, styledArg3 ]
    }


values_ : { styled : Elm.Expression }
values_ =
    { styled =
        Elm.value
            { importFrom = [ "Nri", "Ui" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "attributeMsg" ]
                                )
                            , Type.list
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
                        , Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "attributeMsg" ]
                            )
                        , Type.list
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


