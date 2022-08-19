module Gen.Nri.Ui.Html.Attributes.V2 exposing (call_, includeIf, moduleName_, none, nriDescription, nriDescriptionSelector, targetBlank, testId, values_)

{-| 
@docs moduleName_, none, includeIf, targetBlank, nriDescription, nriDescriptionSelector, testId, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Html", "Attributes", "V2" ]


{-| Represents an attribute with no semantic meaning, useful for conditionals.

This is implemented such that whenever Html.Attributes.Extra.none is encountered
by VirtualDom it will set a meaningless property on the element object itself to
null:

    domNode['Html.Attributes.Extra.none'] = null

It's totally safe and lets us clean up conditional and maybe attributes

none: Html.Styled.Attribute msg
-}
none : Elm.Expression
none =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
        , name = "none"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| conditionally include an attribute. Useful for CSS classes generated with
`UniqueClass`!

includeIf: Bool -> Html.Styled.Attribute msg -> Html.Styled.Attribute msg
-}
includeIf : Bool -> Elm.Expression -> Elm.Expression
includeIf includeIfArg includeIfArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "includeIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
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
        [ Elm.bool includeIfArg, includeIfArg0 ]


{-| Use this list of attributes instead of applying `Attributes.target "_blank"`
directly. This prevents an exploits like "tabnabbing", among other things.

See these resources:

  - <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#Security_and_privacy_concerns>
  - <https://www.jitbit.com/alexblog/256-targetblank---the-most-underestimated-vulnerability-ever>

targetBlank: List (Html.Styled.Attribute msg)
-}
targetBlank : Elm.Expression
targetBlank =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
        , name = "targetBlank"
        , annotation =
            Just
                (Type.list
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
                )
        }


{-| nriDescription: String -> Html.Styled.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| Note: this does not handle html escaping the description before building the query

nriDescriptionSelector: String -> List Css.Style -> Css.Global.Snippet
-}
nriDescriptionSelector : String -> List Elm.Expression -> Elm.Expression
nriDescriptionSelector nriDescriptionSelectorArg nriDescriptionSelectorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "nriDescriptionSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.string nriDescriptionSelectorArg
        , Elm.list nriDescriptionSelectorArg0
        ]


{-| See Cypress best practices: <https://docs.cypress.io/guides/references/best-practices.html#Selecting-Elements>

testId: String -> Html.Styled.Attribute msg
-}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


call_ :
    { includeIf : Elm.Expression -> Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , nriDescriptionSelector :
        Elm.Expression -> Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    }
call_ =
    { includeIf =
        \includeIfArg includeIfArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
                    , name = "includeIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool
                                , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
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
                [ includeIfArg, includeIfArg0 ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , nriDescriptionSelector =
        \nriDescriptionSelectorArg nriDescriptionSelectorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
                    , name = "nriDescriptionSelector"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ nriDescriptionSelectorArg, nriDescriptionSelectorArg0 ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    }


values_ :
    { none : Elm.Expression
    , includeIf : Elm.Expression
    , targetBlank : Elm.Expression
    , nriDescription : Elm.Expression
    , nriDescriptionSelector : Elm.Expression
    , testId : Elm.Expression
    }
values_ =
    { none =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "none"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , includeIf =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "includeIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , targetBlank =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "targetBlank"
            , annotation =
                Just
                    (Type.list
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescriptionSelector =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "nriDescriptionSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Html", "Attributes", "V2" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


