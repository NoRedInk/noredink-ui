module Gen.Nri.Ui.Tooltip.V3 exposing (alignEnd, alignEndForMobile, alignMiddle, alignMiddleForMobile, alignStart, alignStartForMobile, annotation_, auxiliaryDescription, call_, containerCss, css, custom, customPadding, disclosure, exactWidth, fitToContent, html, mobileCss, moduleName_, normalPadding, notMobileCss, nriDescription, onBottom, onBottomForMobile, onLeft, onLeftForMobile, onRight, onRightForMobile, onToggle, onTop, onTopForMobile, open, plaintext, primaryLabel, quizEngineMobileCss, smallPadding, testId, values_, view, viewToggleTip, withoutTail)

{-| 
@docs moduleName_, view, viewToggleTip, plaintext, html, withoutTail, onTop, onBottom, onLeft, onRight, onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile, alignStart, alignMiddle, alignEnd, alignStartForMobile, alignMiddleForMobile, alignEndForMobile, exactWidth, fitToContent, smallPadding, normalPadding, customPadding, onToggle, open, css, notMobileCss, mobileCss, quizEngineMobileCss, containerCss, custom, nriDescription, testId, primaryLabel, auxiliaryDescription, disclosure, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Tooltip", "V3" ]


{-| Here's what the fields in the configuration record do:

  - `trigger`: What element do you interact with to open the tooltip?
  - `id`: A unique identifier used to associate the trigger with its content

view: 
    { trigger :
        List (Accessibility.Styled.Attribute msg) -> Accessibility.Styled.Html msg
    , id : String
    }
    -> List (Nri.Ui.Tooltip.V3.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
view :
    { trigger : Elm.Expression -> Elm.Expression, id : String }
    -> List Elm.Expression
    -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "trigger"
                              , Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "id", Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
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
            [ Tuple.pair
                "trigger"
                (Elm.functionReduced "viewUnpack" viewArg.trigger)
            , Tuple.pair "id" (Elm.string viewArg.id)
            ]
        , Elm.list viewArg0
        ]


{-| Supplementary information triggered by a "?" icon.

This is a helper for setting up a commonly-used `disclosure` tooltip. Please see the documentation for `disclosure` to learn more.

viewToggleTip: 
    { label : String, lastId : Maybe String }
    -> List (Nri.Ui.Tooltip.V3.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
viewToggleTip :
    { label : String, lastId : Elm.Expression }
    -> List Elm.Expression
    -> Elm.Expression
viewToggleTip viewToggleTipArg viewToggleTipArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "viewToggleTip"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "lastId", Type.maybe Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
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
            [ Tuple.pair "label" (Elm.string viewToggleTipArg.label)
            , Tuple.pair "lastId" viewToggleTipArg.lastId
            ]
        , Elm.list viewToggleTipArg0
        ]


{-| plaintext: String -> Nri.Ui.Tooltip.V3.Attribute msg -}
plaintext : String -> Elm.Expression
plaintext plaintextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string plaintextArg ]


{-| html: List (Accessibility.Styled.Html msg) -> Nri.Ui.Tooltip.V3.Attribute msg -}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| Makes it so that the tooltip does not have a tail!

withoutTail: Nri.Ui.Tooltip.V3.Attribute msg
-}
withoutTail : Elm.Expression
withoutTail =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "withoutTail"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| __________
    |         |
    |___  ____|
        \/

onTop: Nri.Ui.Tooltip.V3.Attribute msg
-}
onTop : Elm.Expression
onTop =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onTop"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| ___/\_____
    |         |
    |_________|

onBottom: Nri.Ui.Tooltip.V3.Attribute msg
-}
onBottom : Elm.Expression
onBottom =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onBottom"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| __________
     |         |
     |          >
     |_________|

onLeft: Nri.Ui.Tooltip.V3.Attribute msg
-}
onLeft : Elm.Expression
onLeft =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onLeft"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| __________
     |         |
    <          |
     |_________|

onRight: Nri.Ui.Tooltip.V3.Attribute msg
-}
onRight : Elm.Expression
onRight =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onRight"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set the position of the tooltip when the mobile breakpoint applies.

     __________
    |         |
    |___  ____|
        \/

onTopForMobile: Nri.Ui.Tooltip.V3.Attribute msg
-}
onTopForMobile : Elm.Expression
onTopForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onTopForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set the position of the tooltip when the mobile breakpoint applies.

     ___/\_____
    |         |
    |_________|

onBottomForMobile: Nri.Ui.Tooltip.V3.Attribute msg
-}
onBottomForMobile : Elm.Expression
onBottomForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onBottomForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set the position of the tooltip when the mobile breakpoint applies.

      __________
     |         |
     |          >
     |_________|

onLeftForMobile: Nri.Ui.Tooltip.V3.Attribute msg
-}
onLeftForMobile : Elm.Expression
onLeftForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onLeftForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Set the position of the tooltip when the mobile breakpoint applies.

      __________
     |         |
    <          |
     |_________|

onRightForMobile: Nri.Ui.Tooltip.V3.Attribute msg
-}
onRightForMobile : Elm.Expression
onRightForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "onRightForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Put the tail at the "start" of the tooltip.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

alignStart: Css.Px -> Nri.Ui.Tooltip.V3.Attribute msg
-}
alignStart : Elm.Expression -> Elm.Expression
alignStart alignStartArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignStart"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ alignStartArg ]


{-| Put the tail at the "middle" of the tooltip. This is the default behavior.

     __________
    |___  ____|
        \/

alignMiddle: Nri.Ui.Tooltip.V3.Attribute msg
-}
alignMiddle : Elm.Expression
alignMiddle =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "alignMiddle"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Put the tail at the "end" of the tooltip.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

alignEnd: Css.Px -> Nri.Ui.Tooltip.V3.Attribute msg
-}
alignEnd : Elm.Expression -> Elm.Expression
alignEnd alignEndArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignEnd"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ alignEndArg ]


{-| Put the tail at the "start" of the tooltip when the viewport has a mobile width.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

alignStartForMobile: Css.Px -> Nri.Ui.Tooltip.V3.Attribute msg
-}
alignStartForMobile : Elm.Expression -> Elm.Expression
alignStartForMobile alignStartForMobileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignStartForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ alignStartForMobileArg ]


{-| Put the tail at the "middle" of the tooltip when the viewport has a mobile width. This is the default behavior.

     __________
    |___  ____|
        \/

alignMiddleForMobile: Nri.Ui.Tooltip.V3.Attribute msg
-}
alignMiddleForMobile : Elm.Expression
alignMiddleForMobile =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "alignMiddleForMobile"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Put the tail at the "end" of the tooltip when the viewport has a mobile width.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

alignEndForMobile: Css.Px -> Nri.Ui.Tooltip.V3.Attribute msg
-}
alignEndForMobile : Elm.Expression -> Elm.Expression
alignEndForMobile alignEndForMobileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignEndForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ alignEndForMobileArg ]


{-| Define a size in `px` for the tooltips's total width. The default is 320px.

exactWidth: Int -> Nri.Ui.Tooltip.V3.Attribute msg
-}
exactWidth : Int -> Elm.Expression
exactWidth exactWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int exactWidthArg ]


{-| Tooltip width fits its content.

fitToContent: Nri.Ui.Tooltip.V3.Attribute msg
-}
fitToContent : Elm.Expression
fitToContent =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "fitToContent"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| smallPadding: Nri.Ui.Tooltip.V3.Attribute msg -}
smallPadding : Elm.Expression
smallPadding =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "smallPadding"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| This the default spacing.

normalPadding: Nri.Ui.Tooltip.V3.Attribute msg
-}
normalPadding : Elm.Expression
normalPadding =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "normalPadding"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Pass in the desired spacing around the edge of the tooltip (pixels).

customPadding: Float -> Nri.Ui.Tooltip.V3.Attribute msg
-}
customPadding : Float -> Elm.Expression
customPadding customPaddingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "customPadding"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float customPaddingArg ]


{-| The Tooltip event cycle depends on whether you're following the Disclosure pattern, but disguising the Disclosure as a tooltip visually or you're actually adding a hint or label for sighted users.

If you're adding a tooltip to an element that _does_ something on its own, e.g., a "Print" ClickableSvg, then it doesn't make sense for the tooltip to change state on click/enter/space.

However, if you're adding a tooltip to an element that is not interactive at all if you don't count the tooltip, then we can use the click/enter/space events to manage the tooltip state too. This style of "tooltip" is the only kind that will be accessible for touch users on mobile -- it's important to get the access pattern right!

If the tooltip behavior you're seeing doesn't _feel_ quite right, consider whether you need to change tooltip "types" to `disclosure` or to `auxiliaryDescription`.

onToggle: (Bool -> msg) -> Nri.Ui.Tooltip.V3.Attribute msg
-}
onToggle : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onToggle onToggleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onToggle"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "onToggleUnpack" onToggleArg ]


{-| Pass a bool indicating whether the tooltip should be open or closed.

open: Bool -> Nri.Ui.Tooltip.V3.Attribute msg
-}
open : Bool -> Elm.Expression
open openArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "open"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool openArg ]


{-| Set some custom styles on the tooltip.

css: List Css.Style -> Nri.Ui.Tooltip.V3.Attribute msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Set styles that will only apply if the viewport is wider than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

notMobileCss: List Css.Style -> Nri.Ui.Tooltip.V3.Attribute msg
-}
notMobileCss : List Elm.Expression -> Elm.Expression
notMobileCss notMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list notMobileCssArg ]


{-| Set styles that will only apply if the viewport is narrower than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

mobileCss: List Css.Style -> Nri.Ui.Tooltip.V3.Attribute msg
-}
mobileCss : List Elm.Expression -> Elm.Expression
mobileCss mobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mobileCssArg ]


{-| Set styles that will only apply if the viewport is narrower than NRI's quiz-engine-specific mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

quizEngineMobileCss: List Css.Style -> Nri.Ui.Tooltip.V3.Attribute msg
-}
quizEngineMobileCss : List Elm.Expression -> Elm.Expression
quizEngineMobileCss quizEngineMobileCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list quizEngineMobileCssArg ]


{-| containerCss: List Css.Style -> Nri.Ui.Tooltip.V3.Attribute msg -}
containerCss : List Elm.Expression -> Elm.Expression
containerCss containerCssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list containerCssArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Nri.Ui.Tooltip.V3.Attribute msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
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
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.Tooltip.V3.Attribute msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| testId: String -> Nri.Ui.Tooltip.V3.Attribute msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| Used when the content of the tooltip is identical to the accessible name.

For example, when using the Tooltip component with the ClickableSvg component, the Tooltip is providing
extra information to sighted users that screenreader users already have.

This is the default.

primaryLabel: Nri.Ui.Tooltip.V3.Attribute msg
-}
primaryLabel : Elm.Expression
primaryLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "primaryLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Used when the content of the tooltip provides an "auxiliary description" for its content.

An auxiliary description is used when the tooltip content provides supplementary information about its trigger content.

auxiliaryDescription: Nri.Ui.Tooltip.V3.Attribute msg
-}
auxiliaryDescription : Elm.Expression
auxiliaryDescription =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
        , name = "auxiliaryDescription"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "Tooltip", "V3" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Sometimes a "tooltip" only _looks_ like a tooltip, but is really more about hiding and showing extra information when the user asks for it.

If clicking the "tooltip trigger" only ever shows you more info (and especially if this info is rich or interactable), use this attribute.

For more information, please read [Sarah Higley's "Tooltips in the time of WCAG 2.1" post](https://sarahmhigley.com/writing/tooltips-in-wcag-21).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content

You may pass a lastId of Nothing if there is NO focusable content within the disclosure.

disclosure: { triggerId : String, lastId : Maybe String } -> Nri.Ui.Tooltip.V3.Attribute msg
-}
disclosure : { triggerId : String, lastId : Elm.Expression } -> Elm.Expression
disclosure disclosureArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "triggerId", Type.string )
                            , ( "lastId", Type.maybe Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "triggerId" (Elm.string disclosureArg.triggerId)
            , Tuple.pair "lastId" disclosureArg.lastId
            ]
        ]


annotation_ : { attribute : Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 ->
            Type.namedWith
                [ "Nri", "Ui", "Tooltip", "V3" ]
                "Attribute"
                [ attributeArg0 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewToggleTip : Elm.Expression -> Elm.Expression -> Elm.Expression
    , plaintext : Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , alignStart : Elm.Expression -> Elm.Expression
    , alignEnd : Elm.Expression -> Elm.Expression
    , alignStartForMobile : Elm.Expression -> Elm.Expression
    , alignEndForMobile : Elm.Expression -> Elm.Expression
    , exactWidth : Elm.Expression -> Elm.Expression
    , customPadding : Elm.Expression -> Elm.Expression
    , onToggle : Elm.Expression -> Elm.Expression
    , open : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , notMobileCss : Elm.Expression -> Elm.Expression
    , mobileCss : Elm.Expression -> Elm.Expression
    , quizEngineMobileCss : Elm.Expression -> Elm.Expression
    , containerCss : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , disclosure : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "trigger"
                                      , Type.function
                                            [ Type.list
                                                (Type.namedWith
                                                    [ "Accessibility"
                                                    , "Styled"
                                                    ]
                                                    "Attribute"
                                                    [ Type.var "msg" ]
                                                )
                                            ]
                                            (Type.namedWith
                                                [ "Accessibility", "Styled" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "id", Type.string )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tooltip", "V3" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , viewToggleTip =
        \viewToggleTipArg viewToggleTipArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "viewToggleTip"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "label", Type.string )
                                    , ( "lastId", Type.maybe Type.string )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "Tooltip", "V3" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewToggleTipArg, viewToggleTipArg0 ]
    , plaintext =
        \plaintextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "plaintext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ plaintextArg ]
    , html =
        \htmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "html"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , alignStart =
        \alignStartArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "alignStart"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" [] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignStartArg ]
    , alignEnd =
        \alignEndArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "alignEnd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" [] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignEndArg ]
    , alignStartForMobile =
        \alignStartForMobileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "alignStartForMobile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" [] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignStartForMobileArg ]
    , alignEndForMobile =
        \alignEndForMobileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "alignEndForMobile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css" ] "Px" [] ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignEndForMobileArg ]
    , exactWidth =
        \exactWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "exactWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exactWidthArg ]
    , customPadding =
        \customPaddingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "customPadding"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customPaddingArg ]
    , onToggle =
        \onToggleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "onToggle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.bool ] (Type.var "msg") ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onToggleArg ]
    , open =
        \openArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "open"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ openArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , notMobileCss =
        \notMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "notMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ notMobileCssArg ]
    , mobileCss =
        \mobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "mobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mobileCssArg ]
    , quizEngineMobileCss =
        \quizEngineMobileCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "quizEngineMobileCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ quizEngineMobileCssArg ]
    , containerCss =
        \containerCssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "containerCss"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ containerCssArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
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
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , disclosure =
        \disclosureArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
                    , name = "disclosure"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "triggerId", Type.string )
                                    , ( "lastId", Type.maybe Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disclosureArg ]
    }


values_ :
    { view : Elm.Expression
    , viewToggleTip : Elm.Expression
    , plaintext : Elm.Expression
    , html : Elm.Expression
    , withoutTail : Elm.Expression
    , onTop : Elm.Expression
    , onBottom : Elm.Expression
    , onLeft : Elm.Expression
    , onRight : Elm.Expression
    , onTopForMobile : Elm.Expression
    , onBottomForMobile : Elm.Expression
    , onLeftForMobile : Elm.Expression
    , onRightForMobile : Elm.Expression
    , alignStart : Elm.Expression
    , alignMiddle : Elm.Expression
    , alignEnd : Elm.Expression
    , alignStartForMobile : Elm.Expression
    , alignMiddleForMobile : Elm.Expression
    , alignEndForMobile : Elm.Expression
    , exactWidth : Elm.Expression
    , fitToContent : Elm.Expression
    , smallPadding : Elm.Expression
    , normalPadding : Elm.Expression
    , customPadding : Elm.Expression
    , onToggle : Elm.Expression
    , open : Elm.Expression
    , css : Elm.Expression
    , notMobileCss : Elm.Expression
    , mobileCss : Elm.Expression
    , quizEngineMobileCss : Elm.Expression
    , containerCss : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , testId : Elm.Expression
    , primaryLabel : Elm.Expression
    , auxiliaryDescription : Elm.Expression
    , disclosure : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "trigger"
                              , Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "id", Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewToggleTip =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "viewToggleTip"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "label", Type.string )
                            , ( "lastId", Type.maybe Type.string )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , plaintext =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "plaintext"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , withoutTail =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "withoutTail"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onTop =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onTop"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onBottom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onBottom"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onLeft =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onLeft"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onRight =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onRight"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onTopForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onTopForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onBottomForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onBottomForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onLeftForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onLeftForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , onRightForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onRightForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alignStart =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignStart"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alignMiddle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignMiddle"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alignEnd =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignEnd"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alignStartForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignStartForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alignMiddleForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignMiddleForMobile"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alignEndForMobile =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "alignEndForMobile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css" ] "Px" [] ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , exactWidth =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "exactWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fitToContent =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "fitToContent"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , smallPadding =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "smallPadding"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , normalPadding =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "normalPadding"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , customPadding =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "customPadding"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onToggle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "onToggle"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.bool ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , open =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "open"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , notMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "notMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "mobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , quizEngineMobileCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "quizEngineMobileCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , containerCss =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "containerCss"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
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
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , primaryLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "primaryLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , auxiliaryDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "auxiliaryDescription"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Tooltip", "V3" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , disclosure =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Tooltip", "V3" ]
            , name = "disclosure"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "triggerId", Type.string )
                            , ( "lastId", Type.maybe Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "Tooltip", "V3" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


