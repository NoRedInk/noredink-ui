module Gen.Nri.Ui.Menu.V3 exposing (alignment, annotation_, button, buttonId, buttonWidth, call_, caseOf_, custom, disclosure, entry, group, hasBorder, icon, isDisabled, make_, menuId, menuWidth, menuZIndex, moduleName_, opensOnHover, values_, view, wrapping)

{-| 
@docs moduleName_, view, button, custom, alignment, isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover, disclosure, icon, wrapping, hasBorder, buttonWidth, group, entry, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Menu", "V3" ]


{-| Menu/pulldown configuration:

  - `attributes`: List of (attributes)[#menu-attributes] to apply to the menu.
  - `config`: Configuration parameters:
      - `button`: the `Button` to open the menu
      - `entries`: the entries of the menu
      - `isOpen`: whether the menu is currently open or not
      - `focusAndToggle`: the message produced to control the open/closed state and DOM focus

view: 
    List (Nri.Ui.Menu.V3.Attribute msg)
    -> Nri.Ui.Menu.V3.Config msg
    -> Html.Styled.Html msg
-}
view : List Elm.Expression -> Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Config"
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
        [ Elm.list viewArg, viewArg0 ]


{-| Defines a standard `Button` for the menu

button: List Nri.Ui.Menu.V3.ButtonAttribute -> String -> Nri.Ui.Menu.V3.Button msg
-}
button : List Elm.Expression -> String -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "ButtonAttribute"
                                []
                            )
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Button"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list buttonArg, Elm.string buttonArg0 ]


{-| Defines a custom `Button` for the menu

custom: 
    (List (Html.Styled.Attribute msg) -> Html.Styled.Html msg)
    -> Nri.Ui.Menu.V3.Button msg
-}
custom : (Elm.Expression -> Elm.Expression) -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Button"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "customUnpack" customArg ]


{-| Where the menu popover should appear relative to the button

alignment: Nri.Ui.Menu.V3.Alignment -> Nri.Ui.Menu.V3.Attribute msg
-}
alignment : Elm.Expression -> Elm.Expression
alignment alignmentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "alignment"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Alignment"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ alignmentArg ]


{-| Whether the menu can be openned

isDisabled: Bool -> Nri.Ui.Menu.V3.Attribute msg
-}
isDisabled : Bool -> Elm.Expression
isDisabled isDisabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "isDisabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool isDisabledArg ]


{-| Fix the width of the popover |

menuWidth: Int -> Nri.Ui.Menu.V3.Attribute msg
-}
menuWidth : Int -> Elm.Expression
menuWidth menuWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int menuWidthArg ]


{-| A unique string identifier for the button that opens/closes the menu

buttonId: String -> Nri.Ui.Menu.V3.Attribute msg
-}
buttonId : String -> Elm.Expression
buttonId buttonIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "buttonId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string buttonIdArg ]


{-| A unique string identifier for the menu

menuId: String -> Nri.Ui.Menu.V3.Attribute msg
-}
menuId : String -> Elm.Expression
menuId menuIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string menuIdArg ]


{-| The CSS `z-index` used to render the menu. Defaults to `1`.

menuZIndex: Int -> Nri.Ui.Menu.V3.Attribute msg
-}
menuZIndex : Int -> Elm.Expression
menuZIndex menuZIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuZIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int menuZIndexArg ]


{-| Whether the menu will be opened/closed by mouseEnter and mouseLeave interaction. Defaults to `False`.

opensOnHover: Bool -> Nri.Ui.Menu.V3.Attribute msg
-}
opensOnHover : Bool -> Elm.Expression
opensOnHover opensOnHoverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "opensOnHover"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool opensOnHoverArg ]


{-| Makes the menu behave as a disclosure.

For more information, please read [Disclosure (Show/Hide) pattern](https://www.w3.org/WAI/ARIA/apg/patterns/disclosure/).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content

disclosure: { lastId : String } -> Nri.Ui.Menu.V3.Attribute msg
-}
disclosure : { lastId : String } -> Elm.Expression
disclosure disclosureArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.record [ ( "lastId", Type.string ) ] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record [ Tuple.pair "lastId" (Elm.string disclosureArg.lastId) ] ]


{-| Display a particular icon to the left of the title

icon: Nri.Ui.Svg.V1.Svg -> Nri.Ui.Menu.V3.ButtonAttribute
-}
icon : Elm.Expression -> Elm.Expression
icon iconArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
        )
        [ iconArg ]


{-| Determines how we deal with long titles. If not specified it defaults to `WrapAndExpandTitle`.

wrapping: Nri.Ui.Menu.V3.TitleWrapping -> Nri.Ui.Menu.V3.ButtonAttribute
-}
wrapping : Elm.Expression -> Elm.Expression
wrapping wrappingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "wrapping"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "TitleWrapping"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
        )
        [ wrappingArg ]


{-| Whether the menu button has a border. If not specified it defaults to `True`.

hasBorder: Bool -> Nri.Ui.Menu.V3.ButtonAttribute
-}
hasBorder : Bool -> Elm.Expression
hasBorder hasBorderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "hasBorder"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
        )
        [ Elm.bool hasBorderArg ]


{-| Fix the width of the button to a number of pixels

buttonWidth: Int -> Nri.Ui.Menu.V3.ButtonAttribute
-}
buttonWidth : Int -> Elm.Expression
buttonWidth buttonWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "buttonWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
        )
        [ Elm.int buttonWidthArg ]


{-| Represents a group of entries with a named legend.

group: String -> List (Nri.Ui.Menu.V3.Entry msg) -> Nri.Ui.Menu.V3.Entry msg
-}
group : String -> List Elm.Expression -> Elm.Expression
group groupArg groupArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "group"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Entry"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Entry"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string groupArg, Elm.list groupArg0 ]


{-| Represents a single **focusable** entry.

Pass in the id you'd like for your menu item, which will be used to manage the focus.

    Menu.entry "my-button-id"
        (\attributes -> Button.button "One great button" [ Button.custom attributes ])

entry: 
    String
    -> (List (Html.Styled.Attribute msg) -> Html.Styled.Html msg)
    -> Nri.Ui.Menu.V3.Entry msg
-}
entry : String -> (Elm.Expression -> Elm.Expression) -> Elm.Expression
entry entryArg entryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "entry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Entry"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string entryArg, Elm.functionReduced "entryUnpack" entryArg0 ]


annotation_ :
    { config : Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    , button : Type.Annotation -> Type.Annotation
    , buttonAttribute : Type.Annotation
    , alignment : Type.Annotation
    , titleWrapping : Type.Annotation
    , entry : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { config =
        \configArg0 ->
            Type.alias
                moduleName_
                "Config"
                [ configArg0 ]
                (Type.record
                    [ ( "button"
                      , Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Button"
                            [ Type.var "msg" ]
                      )
                    , ( "entries"
                      , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Entry"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "isOpen", Type.bool )
                    , ( "focusAndToggle"
                      , Type.function
                            [ Type.record
                                [ ( "isOpen", Type.bool )
                                , ( "focus", Type.maybe Type.string )
                                ]
                            ]
                            (Type.var "msg")
                      )
                    ]
                )
    , attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Menu", "V3" ]
                "Attribute"
                [ attributeArg0 ]
    , button =
        \buttonArg0 ->
            Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "Button" [ buttonArg0 ]
    , buttonAttribute =
        Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "ButtonAttribute" []
    , alignment = Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "Alignment" []
    , titleWrapping =
        Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "TitleWrapping" []
    , entry =
        \entryArg0 ->
            Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "Entry" [ entryArg0 ]
    }


make_ :
    { config :
        { button : Elm.Expression
        , entries : Elm.Expression
        , isOpen : Elm.Expression
        , focusAndToggle : Elm.Expression
        }
        -> Elm.Expression
    , left : Elm.Expression
    , right : Elm.Expression
    , wrapAndExpandTitle : Elm.Expression
    , truncateTitle : Elm.Expression
    }
make_ =
    { config =
        \config_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Menu", "V3" ]
                    "Config"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "button"
                          , Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Button"
                                [ Type.var "msg" ]
                          )
                        , ( "entries"
                          , Type.list
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Entry"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "isOpen", Type.bool )
                        , ( "focusAndToggle"
                          , Type.function
                                [ Type.record
                                    [ ( "isOpen", Type.bool )
                                    , ( "focus", Type.maybe Type.string )
                                    ]
                                ]
                                (Type.var "msg")
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "button" config_args.button
                    , Tuple.pair "entries" config_args.entries
                    , Tuple.pair "isOpen" config_args.isOpen
                    , Tuple.pair "focusAndToggle" config_args.focusAndToggle
                    ]
                )
    , left =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "Left"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , right =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "Right"
            , annotation = Just (Type.namedWith [] "Alignment" [])
            }
    , wrapAndExpandTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "WrapAndExpandTitle"
            , annotation = Just (Type.namedWith [] "TitleWrapping" [])
            }
    , truncateTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "TruncateTitle"
            , annotation = Just (Type.namedWith [] "TitleWrapping" [])
            }
    }


caseOf_ :
    { alignment :
        Elm.Expression
        -> { alignmentTags_0_0 | left : Elm.Expression, right : Elm.Expression }
        -> Elm.Expression
    , titleWrapping :
        Elm.Expression
        -> { titleWrappingTags_1_0
            | wrapAndExpandTitle : Elm.Expression
            , truncateTitle : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { alignment =
        \alignmentExpression alignmentTags ->
            Elm.Case.custom
                alignmentExpression
                (Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "Alignment" [])
                [ Elm.Case.branch0 "Left" alignmentTags.left
                , Elm.Case.branch0 "Right" alignmentTags.right
                ]
    , titleWrapping =
        \titleWrappingExpression titleWrappingTags ->
            Elm.Case.custom
                titleWrappingExpression
                (Type.namedWith [ "Nri", "Ui", "Menu", "V3" ] "TitleWrapping" []
                )
                [ Elm.Case.branch0
                    "WrapAndExpandTitle"
                    titleWrappingTags.wrapAndExpandTitle
                , Elm.Case.branch0
                    "TruncateTitle"
                    titleWrappingTags.truncateTitle
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , button : Elm.Expression -> Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , alignment : Elm.Expression -> Elm.Expression
    , isDisabled : Elm.Expression -> Elm.Expression
    , menuWidth : Elm.Expression -> Elm.Expression
    , buttonId : Elm.Expression -> Elm.Expression
    , menuId : Elm.Expression -> Elm.Expression
    , menuZIndex : Elm.Expression -> Elm.Expression
    , opensOnHover : Elm.Expression -> Elm.Expression
    , disclosure : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , wrapping : Elm.Expression -> Elm.Expression
    , hasBorder : Elm.Expression -> Elm.Expression
    , buttonWidth : Elm.Expression -> Elm.Expression
    , group : Elm.Expression -> Elm.Expression -> Elm.Expression
    , entry : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Menu", "V3" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Config"
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
                [ viewArg, viewArg0 ]
    , button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Menu", "V3" ]
                                        "ButtonAttribute"
                                        []
                                    )
                                , Type.string
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Button"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buttonArg, buttonArg0 ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Button"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , alignment =
        \alignmentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "alignment"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Alignment"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignmentArg ]
    , isDisabled =
        \isDisabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "isDisabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ isDisabledArg ]
    , menuWidth =
        \menuWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "menuWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ menuWidthArg ]
    , buttonId =
        \buttonIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "buttonId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buttonIdArg ]
    , menuId =
        \menuIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "menuId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ menuIdArg ]
    , menuZIndex =
        \menuZIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "menuZIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ menuZIndexArg ]
    , opensOnHover =
        \opensOnHoverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "opensOnHover"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ opensOnHoverArg ]
    , disclosure =
        \disclosureArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "disclosure"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record [ ( "lastId", Type.string ) ] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disclosureArg ]
    , icon =
        \iconArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "icon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "ButtonAttribute"
                                    []
                                )
                            )
                    }
                )
                [ iconArg ]
    , wrapping =
        \wrappingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "wrapping"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "TitleWrapping"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "ButtonAttribute"
                                    []
                                )
                            )
                    }
                )
                [ wrappingArg ]
    , hasBorder =
        \hasBorderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "hasBorder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "ButtonAttribute"
                                    []
                                )
                            )
                    }
                )
                [ hasBorderArg ]
    , buttonWidth =
        \buttonWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "buttonWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "ButtonAttribute"
                                    []
                                )
                            )
                    }
                )
                [ buttonWidthArg ]
    , group =
        \groupArg groupArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "group"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Menu", "V3" ]
                                        "Entry"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Entry"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ groupArg, groupArg0 ]
    , entry =
        \entryArg entryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
                    , name = "entry"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Menu", "V3" ]
                                    "Entry"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ entryArg, entryArg0 ]
    }


values_ :
    { view : Elm.Expression
    , button : Elm.Expression
    , custom : Elm.Expression
    , alignment : Elm.Expression
    , isDisabled : Elm.Expression
    , menuWidth : Elm.Expression
    , buttonId : Elm.Expression
    , menuId : Elm.Expression
    , menuZIndex : Elm.Expression
    , opensOnHover : Elm.Expression
    , disclosure : Elm.Expression
    , icon : Elm.Expression
    , wrapping : Elm.Expression
    , hasBorder : Elm.Expression
    , buttonWidth : Elm.Expression
    , group : Elm.Expression
    , entry : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Config"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "ButtonAttribute"
                                []
                            )
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Button"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Button"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alignment =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "alignment"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Alignment"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , isDisabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "isDisabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , menuWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , buttonId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "buttonId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , menuId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , menuZIndex =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "menuZIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , opensOnHover =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "opensOnHover"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , disclosure =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.record [ ( "lastId", Type.string ) ] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , icon =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "icon"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
    , wrapping =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "wrapping"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "TitleWrapping"
                            []
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
    , hasBorder =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "hasBorder"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
    , buttonWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "buttonWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "ButtonAttribute"
                            []
                        )
                    )
            }
    , group =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "group"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Menu", "V3" ]
                                "Entry"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Entry"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , entry =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Menu", "V3" ]
            , name = "entry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Menu", "V3" ]
                            "Entry"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


