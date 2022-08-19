module Gen.Nri.Ui.Sprite.V1 exposing (annotation_, attach, bold, call_, italic, link, list, moduleName_, redo, spriteIdToString, underline, undo, use, values_)

{-| 
@docs moduleName_, attach, use, spriteIdToString, bold, italic, underline, list, link, undo, redo, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Sprite", "V1" ]


{-| Sprites _must_ be attached to the page for them to be usable!

If your icons are missing, please make sure you're using this HTML in your view!

attach: Accessibility.Styled.Html Basics.Never
-}
attach : Elm.Expression
attach =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "attach"
        , annotation =
            Just
                (Type.namedWith
                    [ "Accessibility", "Styled" ]
                    "Html"
                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                )
        }


{-| use: Nri.Ui.Sprite.V1.SpriteId -> Svg.Styled.Svg msg -}
use : Elm.Expression -> Elm.Expression
use useArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "use"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Sprite", "V1" ]
                            "SpriteId"
                            []
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ useArg ]


{-| spriteIdToString: Nri.Ui.Sprite.V1.SpriteId -> String -}
spriteIdToString : Elm.Expression -> Elm.Expression
spriteIdToString spriteIdToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "spriteIdToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Sprite", "V1" ]
                            "SpriteId"
                            []
                        ]
                        Type.string
                    )
            }
        )
        [ spriteIdToStringArg ]


{-| svg [] [ Sprite.use Sprite.bold ]

bold: Nri.Ui.Sprite.V1.SpriteId
-}
bold : Elm.Expression
bold =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "bold"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| italic: Nri.Ui.Sprite.V1.SpriteId -}
italic : Elm.Expression
italic =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "italic"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| underline: Nri.Ui.Sprite.V1.SpriteId -}
underline : Elm.Expression
underline =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "underline"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| list: Nri.Ui.Sprite.V1.SpriteId -}
list : Elm.Expression
list =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "list"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| link: Nri.Ui.Sprite.V1.SpriteId -}
link : Elm.Expression
link =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "link"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| undo: Nri.Ui.Sprite.V1.SpriteId -}
undo : Elm.Expression
undo =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "undo"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


{-| redo: Nri.Ui.Sprite.V1.SpriteId -}
redo : Elm.Expression
redo =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
        , name = "redo"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" [])
        }


annotation_ : { sprite : Type.Annotation, spriteId : Type.Annotation }
annotation_ =
    { sprite = Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "Sprite" []
    , spriteId = Type.namedWith [ "Nri", "Ui", "Sprite", "V1" ] "SpriteId" []
    }


call_ :
    { use : Elm.Expression -> Elm.Expression
    , spriteIdToString : Elm.Expression -> Elm.Expression
    }
call_ =
    { use =
        \useArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
                    , name = "use"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Sprite", "V1" ]
                                    "SpriteId"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ useArg ]
    , spriteIdToString =
        \spriteIdToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
                    , name = "spriteIdToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Sprite", "V1" ]
                                    "SpriteId"
                                    []
                                ]
                                Type.string
                            )
                    }
                )
                [ spriteIdToStringArg ]
    }


values_ :
    { attach : Elm.Expression
    , use : Elm.Expression
    , spriteIdToString : Elm.Expression
    , bold : Elm.Expression
    , italic : Elm.Expression
    , underline : Elm.Expression
    , list : Elm.Expression
    , link : Elm.Expression
    , undo : Elm.Expression
    , redo : Elm.Expression
    }
values_ =
    { attach =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "attach"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Accessibility", "Styled" ]
                        "Html"
                        [ Type.namedWith [ "Basics" ] "Never" [] ]
                    )
            }
    , use =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "use"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Sprite", "V1" ]
                            "SpriteId"
                            []
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spriteIdToString =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "spriteIdToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Sprite", "V1" ]
                            "SpriteId"
                            []
                        ]
                        Type.string
                    )
            }
    , bold =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "bold"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , italic =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "italic"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , underline =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "underline"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , list =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "list"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , link =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "link"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , undo =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "undo"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    , redo =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Sprite", "V1" ]
            , name = "redo"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Sprite", "V1" ]
                        "SpriteId"
                        []
                    )
            }
    }


