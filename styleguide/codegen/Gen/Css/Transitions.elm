module Gen.Css.Transitions exposing (annotation_, background, background2, background3, backgroundColor, backgroundColor2, backgroundColor3, backgroundPosition, backgroundPosition2, backgroundPosition3, backgroundSize, backgroundSize2, backgroundSize3, border, border2, border3, borderBottom, borderBottom2, borderBottom3, borderBottomColor, borderBottomColor2, borderBottomColor3, borderBottomLeftRadius, borderBottomLeftRadius2, borderBottomLeftRadius3, borderBottomRightRadius, borderBottomRightRadius2, borderBottomRightRadius3, borderBottomWidth, borderBottomWidth2, borderBottomWidth3, borderColor, borderColor2, borderColor3, borderLeft, borderLeft2, borderLeft3, borderLeftColor, borderLeftColor2, borderLeftColor3, borderLeftWidth, borderLeftWidth2, borderLeftWidth3, borderRadius, borderRadius2, borderRadius3, borderRight, borderRight2, borderRight3, borderRightColor, borderRightColor2, borderRightColor3, borderRightWidth, borderRightWidth2, borderRightWidth3, borderTop, borderTop2, borderTop3, borderTopColor, borderTopColor2, borderTopColor3, borderTopLeftRadius, borderTopLeftRadius2, borderTopLeftRadius3, borderTopRightRadius, borderTopRightRadius2, borderTopRightRadius3, borderTopWidth, borderTopWidth2, borderTopWidth3, borderWidth, borderWidth2, borderWidth3, bottom, bottom2, bottom3, boxShadow, boxShadow2, boxShadow3, call_, caretColor, caretColor2, caretColor3, clip, clip2, clip3, clipPath, clipPath2, clipPath3, color, color2, color3, columnCount, columnCount2, columnCount3, columnGap, columnGap2, columnGap3, columnRule, columnRule2, columnRule3, columnRuleColor, columnRuleColor2, columnRuleColor3, columnRuleWidth, columnRuleWidth2, columnRuleWidth3, columnWidth, columnWidth2, columnWidth3, columns, columns2, columns3, cubicBezier, ease, easeIn, easeInOut, easeOut, filter, filter2, filter3, flex, flex2, flex3, flexBasis, flexBasis2, flexBasis3, flexGrow, flexGrow2, flexGrow3, flexShrink, flexShrink2, flexShrink3, font, font2, font3, fontSize, fontSize2, fontSize3, fontSizeAdjust, fontSizeAdjust2, fontSizeAdjust3, fontStretch, fontStretch2, fontStretch3, fontVariationSettings, fontVariationSettings2, fontVariationSettings3, fontWeight, fontWeight2, fontWeight3, gridColumnGap, gridColumnGap2, gridColumnGap3, gridGap, gridGap2, gridGap3, gridRowGap, gridRowGap2, gridRowGap3, height, height2, height3, left, left2, left3, letterSpacing, letterSpacing2, letterSpacing3, lineHeight, lineHeight2, lineHeight3, linear, margin, margin2, margin3, marginBottom, marginBottom2, marginBottom3, marginLeft, marginLeft2, marginLeft3, marginRight, marginRight2, marginRight3, marginTop, marginTop2, marginTop3, mask, mask2, mask3, maskPosition, maskPosition2, maskPosition3, maskSize, maskSize2, maskSize3, maxHeight, maxHeight2, maxHeight3, maxWidth, maxWidth2, maxWidth3, minHeight, minHeight2, minHeight3, minWidth, minWidth2, minWidth3, moduleName_, objectPosition, objectPosition2, objectPosition3, offset, offset2, offset3, offsetAnchor, offsetAnchor2, offsetAnchor3, offsetDistance, offsetDistance2, offsetDistance3, offsetPath, offsetPath2, offsetPath3, offsetRotate, offsetRotate2, offsetRotate3, opacity, opacity2, opacity3, order, order2, order3, outline, outline2, outline3, outlineColor, outlineColor2, outlineColor3, outlineOffset, outlineOffset2, outlineOffset3, outlineWidth, outlineWidth2, outlineWidth3, padding, padding2, padding3, paddingBottom, paddingBottom2, paddingBottom3, paddingLeft, paddingLeft2, paddingLeft3, paddingRight, paddingRight2, paddingRight3, paddingTop, paddingTop2, paddingTop3, right, right2, right3, stepEnd, stepStart, tabSize, tabSize2, tabSize3, textIndent, textIndent2, textIndent3, textShadow, textShadow2, textShadow3, top, top2, top3, transform, transform2, transform3, transformOrigin, transformOrigin2, transformOrigin3, transition, values_, verticalAlign, verticalAlign2, verticalAlign3, visibility, visibility2, visibility3, width, width2, width3, wordSpacing, wordSpacing2, wordSpacing3, zIndex, zIndex2, zIndex3)

{-| 
@docs moduleName_, transition, ease, linear, easeIn, easeOut, easeInOut, stepStart, stepEnd, cubicBezier, background, background2, background3, backgroundColor, backgroundColor2, backgroundColor3, backgroundPosition, backgroundPosition2, backgroundPosition3, backgroundSize, backgroundSize2, backgroundSize3, border, border2, border3, borderBottom, borderBottom2, borderBottom3, borderBottomColor, borderBottomColor2, borderBottomColor3, borderBottomLeftRadius, borderBottomLeftRadius2, borderBottomLeftRadius3, borderBottomRightRadius, borderBottomRightRadius2, borderBottomRightRadius3, borderBottomWidth, borderBottomWidth2, borderBottomWidth3, borderColor, borderColor2, borderColor3, borderLeft, borderLeft2, borderLeft3, borderLeftColor, borderLeftColor2, borderLeftColor3, borderLeftWidth, borderLeftWidth2, borderLeftWidth3, borderRadius, borderRadius2, borderRadius3, borderRight, borderRight2, borderRight3, borderRightColor, borderRightColor2, borderRightColor3, borderRightWidth, borderRightWidth2, borderRightWidth3, borderTop, borderTop2, borderTop3, borderTopColor, borderTopColor2, borderTopColor3, borderTopLeftRadius, borderTopLeftRadius2, borderTopLeftRadius3, borderTopRightRadius, borderTopRightRadius2, borderTopRightRadius3, borderTopWidth, borderTopWidth2, borderTopWidth3, borderWidth, borderWidth2, borderWidth3, bottom, bottom2, bottom3, boxShadow, boxShadow2, boxShadow3, caretColor, caretColor2, caretColor3, clip, clip2, clip3, clipPath, clipPath2, clipPath3, color, color2, color3, columnCount, columnCount2, columnCount3, columnGap, columnGap2, columnGap3, columnRule, columnRule2, columnRule3, columnRuleColor, columnRuleColor2, columnRuleColor3, columnRuleWidth, columnRuleWidth2, columnRuleWidth3, columnWidth, columnWidth2, columnWidth3, columns, columns2, columns3, filter, filter2, filter3, flex, flex2, flex3, flexBasis, flexBasis2, flexBasis3, flexGrow, flexGrow2, flexGrow3, flexShrink, flexShrink2, flexShrink3, font, font2, font3, fontSize, fontSize2, fontSize3, fontSizeAdjust, fontSizeAdjust2, fontSizeAdjust3, fontStretch, fontStretch2, fontStretch3, fontVariationSettings, fontVariationSettings2, fontVariationSettings3, fontWeight, fontWeight2, fontWeight3, gridColumnGap, gridColumnGap2, gridColumnGap3, gridGap, gridGap2, gridGap3, gridRowGap, gridRowGap2, gridRowGap3, height, height2, height3, left, left2, left3, letterSpacing, letterSpacing2, letterSpacing3, lineHeight, lineHeight2, lineHeight3, margin, margin2, margin3, marginBottom, marginBottom2, marginBottom3, marginLeft, marginLeft2, marginLeft3, marginRight, marginRight2, marginRight3, marginTop, marginTop2, marginTop3, mask, mask2, mask3, maskPosition, maskPosition2, maskPosition3, maskSize, maskSize2, maskSize3, maxHeight, maxHeight2, maxHeight3, maxWidth, maxWidth2, maxWidth3, minHeight, minHeight2, minHeight3, minWidth, minWidth2, minWidth3, objectPosition, objectPosition2, objectPosition3, offset, offset2, offset3, offsetAnchor, offsetAnchor2, offsetAnchor3, offsetDistance, offsetDistance2, offsetDistance3, offsetPath, offsetPath2, offsetPath3, offsetRotate, offsetRotate2, offsetRotate3, opacity, opacity2, opacity3, order, order2, order3, outline, outline2, outline3, outlineColor, outlineColor2, outlineColor3, outlineOffset, outlineOffset2, outlineOffset3, outlineWidth, outlineWidth2, outlineWidth3, padding, padding2, padding3, paddingBottom, paddingBottom2, paddingBottom3, paddingLeft, paddingLeft2, paddingLeft3, paddingRight, paddingRight2, paddingRight3, paddingTop, paddingTop2, paddingTop3, right, right2, right3, tabSize, tabSize2, tabSize3, textIndent, textIndent2, textIndent3, textShadow, textShadow2, textShadow3, top, top2, top3, transform, transform2, transform3, transformOrigin, transformOrigin2, transformOrigin3, verticalAlign, verticalAlign2, verticalAlign3, visibility, visibility2, visibility3, width, width2, width3, wordSpacing, wordSpacing2, wordSpacing3, zIndex, zIndex2, zIndex3, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Css", "Transitions" ]


{-| This function is used to batch up a list of supplied transitions that have been created (using the property functions listed below) and then produce a [`Css.Style`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#Style).
This can then be used with other functions (such as [`Html.Styled.Attributes.css`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Attributes#css)) to add the desired transitions to elements / classes as required.

transition: List Css.Transitions.Transition -> Css.Style
-}
transition : List Elm.Expression -> Elm.Expression
transition transitionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transition"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Css", "Transitions" ]
                                "Transition"
                                []
                            )
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list transitionArg ]


{-| CSS ease timing function

ease: Css.Transitions.TimingFunction
-}
ease : Elm.Expression
ease =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "ease"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS linear timing function

linear: Css.Transitions.TimingFunction
-}
linear : Elm.Expression
linear =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "linear"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS easeIn timing function

easeIn: Css.Transitions.TimingFunction
-}
easeIn : Elm.Expression
easeIn =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "easeIn"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS easeOut timing function

easeOut: Css.Transitions.TimingFunction
-}
easeOut : Elm.Expression
easeOut =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "easeOut"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS easeInOut timing function

easeInOut: Css.Transitions.TimingFunction
-}
easeInOut : Elm.Expression
easeInOut =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "easeInOut"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS stepStart timing function

stepStart: Css.Transitions.TimingFunction
-}
stepStart : Elm.Expression
stepStart =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "stepStart"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS stepEnd timing function

stepEnd: Css.Transitions.TimingFunction
-}
stepEnd : Elm.Expression
stepEnd =
    Elm.value
        { importFrom = [ "Css", "Transitions" ]
        , name = "stepEnd"
        , annotation =
            Just (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" [])
        }


{-| CSS cubicBezier timing function

cubicBezier: Float -> Float -> Float -> Float -> Css.Transitions.TimingFunction
-}
cubicBezier : Float -> Float -> Float -> Float -> Elm.Expression
cubicBezier cubicBezierArg cubicBezierArg0 cubicBezierArg1 cubicBezierArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "cubicBezier"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float, Type.float ]
                        (Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        )
                    )
            }
        )
        [ Elm.float cubicBezierArg
        , Elm.float cubicBezierArg0
        , Elm.float cubicBezierArg1
        , Elm.float cubicBezierArg2
        ]


{-| Create a [`Transition`](#Transition) for the [background](https://developer.mozilla.org/en-US/docs/Web/CSS/background)
property specifying only the duration

background: Float -> Css.Transitions.Transition
-}
background : Float -> Elm.Expression
background backgroundArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundArg ]


{-| Create a [`Transition`](#Transition) for the [background](https://developer.mozilla.org/en-US/docs/Web/CSS/background)
property specifying duration and delay

background2: Float -> Float -> Css.Transitions.Transition
-}
background2 : Float -> Float -> Elm.Expression
background2 background2Arg background2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float background2Arg, Elm.float background2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [background](https://developer.mozilla.org/en-US/docs/Web/CSS/background)
property specifying duration, delay and timing function

background3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
background3 : Float -> Float -> Elm.Expression -> Elm.Expression
background3 background3Arg background3Arg0 background3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float background3Arg, Elm.float background3Arg0, background3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [background-color](https://developer.mozilla.org/en-US/docs/Web/CSS/background-color)
property specifying only the duration

backgroundColor: Float -> Css.Transitions.Transition
-}
backgroundColor : Float -> Elm.Expression
backgroundColor backgroundColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundColorArg ]


{-| Create a [`Transition`](#Transition) for the [background-color](https://developer.mozilla.org/en-US/docs/Web/CSS/background-color)
property specifying duration and delay

backgroundColor2: Float -> Float -> Css.Transitions.Transition
-}
backgroundColor2 : Float -> Float -> Elm.Expression
backgroundColor2 backgroundColor2Arg backgroundColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundColor2Arg, Elm.float backgroundColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [background-color](https://developer.mozilla.org/en-US/docs/Web/CSS/background-color)
property specifying duration, delay and timing function

backgroundColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
backgroundColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
backgroundColor3 backgroundColor3Arg backgroundColor3Arg0 backgroundColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundColor3Arg
        , Elm.float backgroundColor3Arg0
        , backgroundColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [background-position](https://developer.mozilla.org/en-US/docs/Web/CSS/background-position)
property specifying only the duration

backgroundPosition: Float -> Css.Transitions.Transition
-}
backgroundPosition : Float -> Elm.Expression
backgroundPosition backgroundPositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundPositionArg ]


{-| Create a [`Transition`](#Transition) for the [background-position](https://developer.mozilla.org/en-US/docs/Web/CSS/background-position)
property specifying duration and delay

backgroundPosition2: Float -> Float -> Css.Transitions.Transition
-}
backgroundPosition2 : Float -> Float -> Elm.Expression
backgroundPosition2 backgroundPosition2Arg backgroundPosition2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundPosition2Arg, Elm.float backgroundPosition2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [background-position](https://developer.mozilla.org/en-US/docs/Web/CSS/background-position)
property specifying duration, delay and timing function

backgroundPosition3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
backgroundPosition3 : Float -> Float -> Elm.Expression -> Elm.Expression
backgroundPosition3 backgroundPosition3Arg backgroundPosition3Arg0 backgroundPosition3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundPosition3Arg
        , Elm.float backgroundPosition3Arg0
        , backgroundPosition3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [background-size](https://developer.mozilla.org/en-US/docs/Web/CSS/background-size)
property specifying only the duration

backgroundSize: Float -> Css.Transitions.Transition
-}
backgroundSize : Float -> Elm.Expression
backgroundSize backgroundSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundSizeArg ]


{-| Create a [`Transition`](#Transition) for the [background-size](https://developer.mozilla.org/en-US/docs/Web/CSS/background-size)
property specifying duration and delay

backgroundSize2: Float -> Float -> Css.Transitions.Transition
-}
backgroundSize2 : Float -> Float -> Elm.Expression
backgroundSize2 backgroundSize2Arg backgroundSize2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundSize2Arg, Elm.float backgroundSize2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [background-size](https://developer.mozilla.org/en-US/docs/Web/CSS/background-size)
property specifying duration, delay and timing function

backgroundSize3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
backgroundSize3 : Float -> Float -> Elm.Expression -> Elm.Expression
backgroundSize3 backgroundSize3Arg backgroundSize3Arg0 backgroundSize3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float backgroundSize3Arg
        , Elm.float backgroundSize3Arg0
        , backgroundSize3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border](https://developer.mozilla.org/en-US/docs/Web/CSS/border)
property specifying only the duration

border: Float -> Css.Transitions.Transition
-}
border : Float -> Elm.Expression
border borderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderArg ]


{-| Create a [`Transition`](#Transition) for the [border](https://developer.mozilla.org/en-US/docs/Web/CSS/border)
property specifying duration and delay

border2: Float -> Float -> Css.Transitions.Transition
-}
border2 : Float -> Float -> Elm.Expression
border2 border2Arg border2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float border2Arg, Elm.float border2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border](https://developer.mozilla.org/en-US/docs/Web/CSS/border)
property specifying duration, delay and timing function

border3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
border3 : Float -> Float -> Elm.Expression -> Elm.Expression
border3 border3Arg border3Arg0 border3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float border3Arg, Elm.float border3Arg0, border3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [border-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom)
property specifying only the duration

borderBottom: Float -> Css.Transitions.Transition
-}
borderBottom : Float -> Elm.Expression
borderBottom borderBottomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomArg ]


{-| Create a [`Transition`](#Transition) for the [border-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom)
property specifying duration and delay

borderBottom2: Float -> Float -> Css.Transitions.Transition
-}
borderBottom2 : Float -> Float -> Elm.Expression
borderBottom2 borderBottom2Arg borderBottom2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottom2Arg, Elm.float borderBottom2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom)
property specifying duration, delay and timing function

borderBottom3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderBottom3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderBottom3 borderBottom3Arg borderBottom3Arg0 borderBottom3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottom3Arg
        , Elm.float borderBottom3Arg0
        , borderBottom3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color)
property specifying only the duration

borderBottomColor: Float -> Css.Transitions.Transition
-}
borderBottomColor : Float -> Elm.Expression
borderBottomColor borderBottomColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomColorArg ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color)
property specifying duration and delay

borderBottomColor2: Float -> Float -> Css.Transitions.Transition
-}
borderBottomColor2 : Float -> Float -> Elm.Expression
borderBottomColor2 borderBottomColor2Arg borderBottomColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomColor2Arg, Elm.float borderBottomColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color)
property specifying duration, delay and timing function

borderBottomColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderBottomColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderBottomColor3 borderBottomColor3Arg borderBottomColor3Arg0 borderBottomColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomColor3Arg
        , Elm.float borderBottomColor3Arg0
        , borderBottomColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius)
property specifying only the duration

borderBottomLeftRadius: Float -> Css.Transitions.Transition
-}
borderBottomLeftRadius : Float -> Elm.Expression
borderBottomLeftRadius borderBottomLeftRadiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomLeftRadiusArg ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius)
property specifying duration and delay

borderBottomLeftRadius2: Float -> Float -> Css.Transitions.Transition
-}
borderBottomLeftRadius2 : Float -> Float -> Elm.Expression
borderBottomLeftRadius2 borderBottomLeftRadius2Arg borderBottomLeftRadius2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomLeftRadius2Arg
        , Elm.float borderBottomLeftRadius2Arg0
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius)
property specifying duration, delay and timing function

borderBottomLeftRadius3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderBottomLeftRadius3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderBottomLeftRadius3 borderBottomLeftRadius3Arg borderBottomLeftRadius3Arg0 borderBottomLeftRadius3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomLeftRadius3Arg
        , Elm.float borderBottomLeftRadius3Arg0
        , borderBottomLeftRadius3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius)
property specifying only the duration

borderBottomRightRadius: Float -> Css.Transitions.Transition
-}
borderBottomRightRadius : Float -> Elm.Expression
borderBottomRightRadius borderBottomRightRadiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomRightRadiusArg ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius)
property specifying duration and delay

borderBottomRightRadius2: Float -> Float -> Css.Transitions.Transition
-}
borderBottomRightRadius2 : Float -> Float -> Elm.Expression
borderBottomRightRadius2 borderBottomRightRadius2Arg borderBottomRightRadius2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomRightRadius2Arg
        , Elm.float borderBottomRightRadius2Arg0
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius)
property specifying duration, delay and timing function

borderBottomRightRadius3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderBottomRightRadius3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderBottomRightRadius3 borderBottomRightRadius3Arg borderBottomRightRadius3Arg0 borderBottomRightRadius3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomRightRadius3Arg
        , Elm.float borderBottomRightRadius3Arg0
        , borderBottomRightRadius3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width)
property specifying only the duration

borderBottomWidth: Float -> Css.Transitions.Transition
-}
borderBottomWidth : Float -> Elm.Expression
borderBottomWidth borderBottomWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomWidthArg ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width)
property specifying duration and delay

borderBottomWidth2: Float -> Float -> Css.Transitions.Transition
-}
borderBottomWidth2 : Float -> Float -> Elm.Expression
borderBottomWidth2 borderBottomWidth2Arg borderBottomWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomWidth2Arg, Elm.float borderBottomWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-bottom-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width)
property specifying duration, delay and timing function

borderBottomWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderBottomWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderBottomWidth3 borderBottomWidth3Arg borderBottomWidth3Arg0 borderBottomWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderBottomWidth3Arg
        , Elm.float borderBottomWidth3Arg0
        , borderBottomWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color)
property specifying only the duration

borderColor: Float -> Css.Transitions.Transition
-}
borderColor : Float -> Elm.Expression
borderColor borderColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderColorArg ]


{-| Create a [`Transition`](#Transition) for the [border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color)
property specifying duration and delay

borderColor2: Float -> Float -> Css.Transitions.Transition
-}
borderColor2 : Float -> Float -> Elm.Expression
borderColor2 borderColor2Arg borderColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderColor2Arg, Elm.float borderColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color)
property specifying duration, delay and timing function

borderColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderColor3 borderColor3Arg borderColor3Arg0 borderColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderColor3Arg
        , Elm.float borderColor3Arg0
        , borderColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-left](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left)
property specifying only the duration

borderLeft: Float -> Css.Transitions.Transition
-}
borderLeft : Float -> Elm.Expression
borderLeft borderLeftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftArg ]


{-| Create a [`Transition`](#Transition) for the [border-left](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left)
property specifying duration and delay

borderLeft2: Float -> Float -> Css.Transitions.Transition
-}
borderLeft2 : Float -> Float -> Elm.Expression
borderLeft2 borderLeft2Arg borderLeft2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeft2Arg, Elm.float borderLeft2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-left](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left)
property specifying duration, delay and timing function

borderLeft3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderLeft3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderLeft3 borderLeft3Arg borderLeft3Arg0 borderLeft3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeft3Arg, Elm.float borderLeft3Arg0, borderLeft3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [border-left-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color)
property specifying only the duration

borderLeftColor: Float -> Css.Transitions.Transition
-}
borderLeftColor : Float -> Elm.Expression
borderLeftColor borderLeftColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftColorArg ]


{-| Create a [`Transition`](#Transition) for the [border-left-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color)
property specifying duration and delay

borderLeftColor2: Float -> Float -> Css.Transitions.Transition
-}
borderLeftColor2 : Float -> Float -> Elm.Expression
borderLeftColor2 borderLeftColor2Arg borderLeftColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftColor2Arg, Elm.float borderLeftColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-left-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color)
property specifying duration, delay and timing function

borderLeftColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderLeftColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderLeftColor3 borderLeftColor3Arg borderLeftColor3Arg0 borderLeftColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftColor3Arg
        , Elm.float borderLeftColor3Arg0
        , borderLeftColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-left-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width)
property specifying only the duration

borderLeftWidth: Float -> Css.Transitions.Transition
-}
borderLeftWidth : Float -> Elm.Expression
borderLeftWidth borderLeftWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftWidthArg ]


{-| Create a [`Transition`](#Transition) for the [border-left-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width)
property specifying duration and delay

borderLeftWidth2: Float -> Float -> Css.Transitions.Transition
-}
borderLeftWidth2 : Float -> Float -> Elm.Expression
borderLeftWidth2 borderLeftWidth2Arg borderLeftWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftWidth2Arg, Elm.float borderLeftWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-left-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width)
property specifying duration, delay and timing function

borderLeftWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderLeftWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderLeftWidth3 borderLeftWidth3Arg borderLeftWidth3Arg0 borderLeftWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderLeftWidth3Arg
        , Elm.float borderLeftWidth3Arg0
        , borderLeftWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius)
property specifying only the duration

borderRadius: Float -> Css.Transitions.Transition
-}
borderRadius : Float -> Elm.Expression
borderRadius borderRadiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRadiusArg ]


{-| Create a [`Transition`](#Transition) for the [border-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius)
property specifying duration and delay

borderRadius2: Float -> Float -> Css.Transitions.Transition
-}
borderRadius2 : Float -> Float -> Elm.Expression
borderRadius2 borderRadius2Arg borderRadius2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRadius2Arg, Elm.float borderRadius2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius)
property specifying duration, delay and timing function

borderRadius3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderRadius3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderRadius3 borderRadius3Arg borderRadius3Arg0 borderRadius3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRadius3Arg
        , Elm.float borderRadius3Arg0
        , borderRadius3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-right](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right)
property specifying only the duration

borderRight: Float -> Css.Transitions.Transition
-}
borderRight : Float -> Elm.Expression
borderRight borderRightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightArg ]


{-| Create a [`Transition`](#Transition) for the [border-right](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right)
property specifying duration and delay

borderRight2: Float -> Float -> Css.Transitions.Transition
-}
borderRight2 : Float -> Float -> Elm.Expression
borderRight2 borderRight2Arg borderRight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRight2Arg, Elm.float borderRight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-right](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right)
property specifying duration, delay and timing function

borderRight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderRight3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderRight3 borderRight3Arg borderRight3Arg0 borderRight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRight3Arg
        , Elm.float borderRight3Arg0
        , borderRight3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-right-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color)
property specifying only the duration

borderRightColor: Float -> Css.Transitions.Transition
-}
borderRightColor : Float -> Elm.Expression
borderRightColor borderRightColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightColorArg ]


{-| Create a [`Transition`](#Transition) for the [border-right-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color)
property specifying duration and delay

borderRightColor2: Float -> Float -> Css.Transitions.Transition
-}
borderRightColor2 : Float -> Float -> Elm.Expression
borderRightColor2 borderRightColor2Arg borderRightColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightColor2Arg, Elm.float borderRightColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-right-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color)
property specifying duration, delay and timing function

borderRightColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderRightColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderRightColor3 borderRightColor3Arg borderRightColor3Arg0 borderRightColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightColor3Arg
        , Elm.float borderRightColor3Arg0
        , borderRightColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-right-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width)
property specifying only the duration

borderRightWidth: Float -> Css.Transitions.Transition
-}
borderRightWidth : Float -> Elm.Expression
borderRightWidth borderRightWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightWidthArg ]


{-| Create a [`Transition`](#Transition) for the [border-right-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width)
property specifying duration and delay

borderRightWidth2: Float -> Float -> Css.Transitions.Transition
-}
borderRightWidth2 : Float -> Float -> Elm.Expression
borderRightWidth2 borderRightWidth2Arg borderRightWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightWidth2Arg, Elm.float borderRightWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-right-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width)
property specifying duration, delay and timing function

borderRightWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderRightWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderRightWidth3 borderRightWidth3Arg borderRightWidth3Arg0 borderRightWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderRightWidth3Arg
        , Elm.float borderRightWidth3Arg0
        , borderRightWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-top](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top)
property specifying only the duration

borderTop: Float -> Css.Transitions.Transition
-}
borderTop : Float -> Elm.Expression
borderTop borderTopArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopArg ]


{-| Create a [`Transition`](#Transition) for the [border-top](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top)
property specifying duration and delay

borderTop2: Float -> Float -> Css.Transitions.Transition
-}
borderTop2 : Float -> Float -> Elm.Expression
borderTop2 borderTop2Arg borderTop2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTop2Arg, Elm.float borderTop2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-top](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top)
property specifying duration, delay and timing function

borderTop3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderTop3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderTop3 borderTop3Arg borderTop3Arg0 borderTop3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTop3Arg, Elm.float borderTop3Arg0, borderTop3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [border-top-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color)
property specifying only the duration

borderTopColor: Float -> Css.Transitions.Transition
-}
borderTopColor : Float -> Elm.Expression
borderTopColor borderTopColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopColorArg ]


{-| Create a [`Transition`](#Transition) for the [border-top-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color)
property specifying duration and delay

borderTopColor2: Float -> Float -> Css.Transitions.Transition
-}
borderTopColor2 : Float -> Float -> Elm.Expression
borderTopColor2 borderTopColor2Arg borderTopColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopColor2Arg, Elm.float borderTopColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-top-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color)
property specifying duration, delay and timing function

borderTopColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderTopColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderTopColor3 borderTopColor3Arg borderTopColor3Arg0 borderTopColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopColor3Arg
        , Elm.float borderTopColor3Arg0
        , borderTopColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-top-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius)
property specifying only the duration

borderTopLeftRadius: Float -> Css.Transitions.Transition
-}
borderTopLeftRadius : Float -> Elm.Expression
borderTopLeftRadius borderTopLeftRadiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopLeftRadiusArg ]


{-| Create a [`Transition`](#Transition) for the [border-top-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius)
property specifying duration and delay

borderTopLeftRadius2: Float -> Float -> Css.Transitions.Transition
-}
borderTopLeftRadius2 : Float -> Float -> Elm.Expression
borderTopLeftRadius2 borderTopLeftRadius2Arg borderTopLeftRadius2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopLeftRadius2Arg
        , Elm.float borderTopLeftRadius2Arg0
        ]


{-| Create a [`Transition`](#Transition) for the [border-top-left-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius)
property specifying duration, delay and timing function

borderTopLeftRadius3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderTopLeftRadius3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderTopLeftRadius3 borderTopLeftRadius3Arg borderTopLeftRadius3Arg0 borderTopLeftRadius3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopLeftRadius3Arg
        , Elm.float borderTopLeftRadius3Arg0
        , borderTopLeftRadius3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-top-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius)
property specifying only the duration

borderTopRightRadius: Float -> Css.Transitions.Transition
-}
borderTopRightRadius : Float -> Elm.Expression
borderTopRightRadius borderTopRightRadiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopRightRadiusArg ]


{-| Create a [`Transition`](#Transition) for the [border-top-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius)
property specifying duration and delay

borderTopRightRadius2: Float -> Float -> Css.Transitions.Transition
-}
borderTopRightRadius2 : Float -> Float -> Elm.Expression
borderTopRightRadius2 borderTopRightRadius2Arg borderTopRightRadius2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopRightRadius2Arg
        , Elm.float borderTopRightRadius2Arg0
        ]


{-| Create a [`Transition`](#Transition) for the [border-top-right-radius](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius)
property specifying duration, delay and timing function

borderTopRightRadius3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderTopRightRadius3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderTopRightRadius3 borderTopRightRadius3Arg borderTopRightRadius3Arg0 borderTopRightRadius3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopRightRadius3Arg
        , Elm.float borderTopRightRadius3Arg0
        , borderTopRightRadius3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-top-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width)
property specifying only the duration

borderTopWidth: Float -> Css.Transitions.Transition
-}
borderTopWidth : Float -> Elm.Expression
borderTopWidth borderTopWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopWidthArg ]


{-| Create a [`Transition`](#Transition) for the [border-top-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width)
property specifying duration and delay

borderTopWidth2: Float -> Float -> Css.Transitions.Transition
-}
borderTopWidth2 : Float -> Float -> Elm.Expression
borderTopWidth2 borderTopWidth2Arg borderTopWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopWidth2Arg, Elm.float borderTopWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-top-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width)
property specifying duration, delay and timing function

borderTopWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderTopWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderTopWidth3 borderTopWidth3Arg borderTopWidth3Arg0 borderTopWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderTopWidth3Arg
        , Elm.float borderTopWidth3Arg0
        , borderTopWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width)
property specifying only the duration

borderWidth: Float -> Css.Transitions.Transition
-}
borderWidth : Float -> Elm.Expression
borderWidth borderWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderWidthArg ]


{-| Create a [`Transition`](#Transition) for the [border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width)
property specifying duration and delay

borderWidth2: Float -> Float -> Css.Transitions.Transition
-}
borderWidth2 : Float -> Float -> Elm.Expression
borderWidth2 borderWidth2Arg borderWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderWidth2Arg, Elm.float borderWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width)
property specifying duration, delay and timing function

borderWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
borderWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
borderWidth3 borderWidth3Arg borderWidth3Arg0 borderWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float borderWidth3Arg
        , Elm.float borderWidth3Arg0
        , borderWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/bottom)
property specifying only the duration

bottom: Float -> Css.Transitions.Transition
-}
bottom : Float -> Elm.Expression
bottom bottomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float bottomArg ]


{-| Create a [`Transition`](#Transition) for the [bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/bottom)
property specifying duration and delay

bottom2: Float -> Float -> Css.Transitions.Transition
-}
bottom2 : Float -> Float -> Elm.Expression
bottom2 bottom2Arg bottom2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float bottom2Arg, Elm.float bottom2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/bottom)
property specifying duration, delay and timing function

bottom3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
bottom3 : Float -> Float -> Elm.Expression -> Elm.Expression
bottom3 bottom3Arg bottom3Arg0 bottom3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float bottom3Arg, Elm.float bottom3Arg0, bottom3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [box-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow)
property specifying only the duration

boxShadow: Float -> Css.Transitions.Transition
-}
boxShadow : Float -> Elm.Expression
boxShadow boxShadowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float boxShadowArg ]


{-| Create a [`Transition`](#Transition) for the [box-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/bottom)
property specifying duration and delay

boxShadow2: Float -> Float -> Css.Transitions.Transition
-}
boxShadow2 : Float -> Float -> Elm.Expression
boxShadow2 boxShadow2Arg boxShadow2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float boxShadow2Arg, Elm.float boxShadow2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [box-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow)
property specifying duration, delay and timing function

boxShadow3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
boxShadow3 : Float -> Float -> Elm.Expression -> Elm.Expression
boxShadow3 boxShadow3Arg boxShadow3Arg0 boxShadow3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float boxShadow3Arg, Elm.float boxShadow3Arg0, boxShadow3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [caret-color](https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color)
property specifying only the duration

caretColor: Float -> Css.Transitions.Transition
-}
caretColor : Float -> Elm.Expression
caretColor caretColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float caretColorArg ]


{-| Create a [`Transition`](#Transition) for the [caret-color](https://developer.mozilla.org/en-US/docs/Web/CSS/bottom)
property specifying duration and delay

caretColor2: Float -> Float -> Css.Transitions.Transition
-}
caretColor2 : Float -> Float -> Elm.Expression
caretColor2 caretColor2Arg caretColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float caretColor2Arg, Elm.float caretColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [caret-color](https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color)
property specifying duration, delay and timing function

caretColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
caretColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
caretColor3 caretColor3Arg caretColor3Arg0 caretColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float caretColor3Arg, Elm.float caretColor3Arg0, caretColor3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [clip](https://developer.mozilla.org/en-US/docs/Web/CSS/clip)
property specifying only the duration

clip: Float -> Css.Transitions.Transition
-}
clip : Float -> Elm.Expression
clip clipArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clipArg ]


{-| Create a [`Transition`](#Transition) for the [clip](https://developer.mozilla.org/en-US/docs/Web/CSS/clip)
property specifying duration and delay

clip2: Float -> Float -> Css.Transitions.Transition
-}
clip2 : Float -> Float -> Elm.Expression
clip2 clip2Arg clip2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clip2Arg, Elm.float clip2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [clip](https://developer.mozilla.org/en-US/docs/Web/CSS/clip)
property specifying duration, delay and timing function

clip3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
clip3 : Float -> Float -> Elm.Expression -> Elm.Expression
clip3 clip3Arg clip3Arg0 clip3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clip3Arg, Elm.float clip3Arg0, clip3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [clip-path](https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path)
property specifying only the duration

clipPath: Float -> Css.Transitions.Transition
-}
clipPath : Float -> Elm.Expression
clipPath clipPathArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clipPathArg ]


{-| Create a [`Transition`](#Transition) for the [clip-path](https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path)
property specifying duration and delay

clipPath2: Float -> Float -> Css.Transitions.Transition
-}
clipPath2 : Float -> Float -> Elm.Expression
clipPath2 clipPath2Arg clipPath2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clipPath2Arg, Elm.float clipPath2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [clip-path](https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path)
property specifying duration, delay and timing function

clipPath3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
clipPath3 : Float -> Float -> Elm.Expression -> Elm.Expression
clipPath3 clipPath3Arg clipPath3Arg0 clipPath3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float clipPath3Arg, Elm.float clipPath3Arg0, clipPath3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [color](https://developer.mozilla.org/en-US/docs/Web/CSS/color)
property specifying only the duration

color: Float -> Css.Transitions.Transition
-}
color : Float -> Elm.Expression
color colorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float colorArg ]


{-| Create a [`Transition`](#Transition) for the [color](https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path)
property specifying duration and delay

color2: Float -> Float -> Css.Transitions.Transition
-}
color2 : Float -> Float -> Elm.Expression
color2 color2Arg color2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float color2Arg, Elm.float color2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [color](https://developer.mozilla.org/en-US/docs/Web/CSS/color)
property specifying duration, delay and timing function

color3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
color3 : Float -> Float -> Elm.Expression -> Elm.Expression
color3 color3Arg color3Arg0 color3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float color3Arg, Elm.float color3Arg0, color3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [column-count](https://developer.mozilla.org/en-US/docs/Web/CSS/column-count)
property specifying only the duration

columnCount: Float -> Css.Transitions.Transition
-}
columnCount : Float -> Elm.Expression
columnCount columnCountArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnCountArg ]


{-| Create a [`Transition`](#Transition) for the [column-count](https://developer.mozilla.org/en-US/docs/Web/CSS/column-count)
property specifying duration and delay

columnCount2: Float -> Float -> Css.Transitions.Transition
-}
columnCount2 : Float -> Float -> Elm.Expression
columnCount2 columnCount2Arg columnCount2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnCount2Arg, Elm.float columnCount2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-count](https://developer.mozilla.org/en-US/docs/Web/CSS/column-count)
property specifying duration, delay and timing function

columnCount3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnCount3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnCount3 columnCount3Arg columnCount3Arg0 columnCount3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnCount3Arg
        , Elm.float columnCount3Arg0
        , columnCount3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap)
property specifying only the duration

columnGap: Float -> Css.Transitions.Transition
-}
columnGap : Float -> Elm.Expression
columnGap columnGapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnGapArg ]


{-| Create a [`Transition`](#Transition) for the [column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap)
property specifying duration and delay

columnGap2: Float -> Float -> Css.Transitions.Transition
-}
columnGap2 : Float -> Float -> Elm.Expression
columnGap2 columnGap2Arg columnGap2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnGap2Arg, Elm.float columnGap2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap)
property specifying duration, delay and timing function

columnGap3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnGap3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnGap3 columnGap3Arg columnGap3Arg0 columnGap3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnGap3Arg, Elm.float columnGap3Arg0, columnGap3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [column-rule](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule)
property specifying only the duration

columnRule: Float -> Css.Transitions.Transition
-}
columnRule : Float -> Elm.Expression
columnRule columnRuleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleArg ]


{-| Create a [`Transition`](#Transition) for the [column-rule](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule)
property specifying duration and delay

columnRule2: Float -> Float -> Css.Transitions.Transition
-}
columnRule2 : Float -> Float -> Elm.Expression
columnRule2 columnRule2Arg columnRule2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRule2Arg, Elm.float columnRule2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-rule](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule)
property specifying duration, delay and timing function

columnRule3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnRule3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnRule3 columnRule3Arg columnRule3Arg0 columnRule3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRule3Arg, Elm.float columnRule3Arg0, columnRule3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [column-rule-color](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-color)
property specifying only the duration

columnRuleColor: Float -> Css.Transitions.Transition
-}
columnRuleColor : Float -> Elm.Expression
columnRuleColor columnRuleColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleColorArg ]


{-| Create a [`Transition`](#Transition) for the [column-rule-color](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-color)
property specifying duration and delay

columnRuleColor2: Float -> Float -> Css.Transitions.Transition
-}
columnRuleColor2 : Float -> Float -> Elm.Expression
columnRuleColor2 columnRuleColor2Arg columnRuleColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleColor2Arg, Elm.float columnRuleColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-rule-color](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-color)
property specifying duration, delay and timing function

columnRuleColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnRuleColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnRuleColor3 columnRuleColor3Arg columnRuleColor3Arg0 columnRuleColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleColor3Arg
        , Elm.float columnRuleColor3Arg0
        , columnRuleColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [column-rule-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-width)
property specifying only the duration

columnRuleWidth: Float -> Css.Transitions.Transition
-}
columnRuleWidth : Float -> Elm.Expression
columnRuleWidth columnRuleWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleWidthArg ]


{-| Create a [`Transition`](#Transition) for the [column-rule-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-width)
property specifying duration and delay

columnRuleWidth2: Float -> Float -> Css.Transitions.Transition
-}
columnRuleWidth2 : Float -> Float -> Elm.Expression
columnRuleWidth2 columnRuleWidth2Arg columnRuleWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleWidth2Arg, Elm.float columnRuleWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-rule-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule-width)
property specifying duration, delay and timing function

columnRuleWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnRuleWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnRuleWidth3 columnRuleWidth3Arg columnRuleWidth3Arg0 columnRuleWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnRuleWidth3Arg
        , Elm.float columnRuleWidth3Arg0
        , columnRuleWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [column-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-width)
property specifying only the duration

columnWidth: Float -> Css.Transitions.Transition
-}
columnWidth : Float -> Elm.Expression
columnWidth columnWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnWidthArg ]


{-| Create a [`Transition`](#Transition) for the [column-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-width)
property specifying duration and delay

columnWidth2: Float -> Float -> Css.Transitions.Transition
-}
columnWidth2 : Float -> Float -> Elm.Expression
columnWidth2 columnWidth2Arg columnWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnWidth2Arg, Elm.float columnWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [column-width](https://developer.mozilla.org/en-US/docs/Web/CSS/column-width)
property specifying duration, delay and timing function

columnWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columnWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
columnWidth3 columnWidth3Arg columnWidth3Arg0 columnWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnWidth3Arg
        , Elm.float columnWidth3Arg0
        , columnWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [columns](https://developer.mozilla.org/en-US/docs/Web/CSS/columns)
property specifying only the duration

columns: Float -> Css.Transitions.Transition
-}
columns : Float -> Elm.Expression
columns columnsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columnsArg ]


{-| Create a [`Transition`](#Transition) for the [columns](https://developer.mozilla.org/en-US/docs/Web/CSS/columns)
property specifying duration and delay

columns2: Float -> Float -> Css.Transitions.Transition
-}
columns2 : Float -> Float -> Elm.Expression
columns2 columns2Arg columns2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columns2Arg, Elm.float columns2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [columns](https://developer.mozilla.org/en-US/docs/Web/CSS/columns)
property specifying duration, delay and timing function

columns3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
columns3 : Float -> Float -> Elm.Expression -> Elm.Expression
columns3 columns3Arg columns3Arg0 columns3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float columns3Arg, Elm.float columns3Arg0, columns3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [filter](https://developer.mozilla.org/en-US/docs/Web/CSS/filter)
property specifying only the duration

filter: Float -> Css.Transitions.Transition
-}
filter : Float -> Elm.Expression
filter filterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float filterArg ]


{-| Create a [`Transition`](#Transition) for the [filter](https://developer.mozilla.org/en-US/docs/Web/CSS/filter)
property specifying duration and delay

filter2: Float -> Float -> Css.Transitions.Transition
-}
filter2 : Float -> Float -> Elm.Expression
filter2 filter2Arg filter2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float filter2Arg, Elm.float filter2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [filter](https://developer.mozilla.org/en-US/docs/Web/CSS/filter)
property specifying duration, delay and timing function

filter3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
filter3 : Float -> Float -> Elm.Expression -> Elm.Expression
filter3 filter3Arg filter3Arg0 filter3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float filter3Arg, Elm.float filter3Arg0, filter3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [flex](https://developer.mozilla.org/en-US/docs/Web/CSS/flex)
property specifying only the duration

flex: Float -> Css.Transitions.Transition
-}
flex : Float -> Elm.Expression
flex flexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexArg ]


{-| Create a [`Transition`](#Transition) for the [flex](https://developer.mozilla.org/en-US/docs/Web/CSS/flex)
property specifying duration and delay

flex2: Float -> Float -> Css.Transitions.Transition
-}
flex2 : Float -> Float -> Elm.Expression
flex2 flex2Arg flex2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flex2Arg, Elm.float flex2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [flex](https://developer.mozilla.org/en-US/docs/Web/CSS/flex)
property specifying duration, delay and timing function

flex3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
flex3 : Float -> Float -> Elm.Expression -> Elm.Expression
flex3 flex3Arg flex3Arg0 flex3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flex3Arg, Elm.float flex3Arg0, flex3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [flex-basis](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis)
property specifying only the duration

flexBasis: Float -> Css.Transitions.Transition
-}
flexBasis : Float -> Elm.Expression
flexBasis flexBasisArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexBasisArg ]


{-| Create a [`Transition`](#Transition) for the [flex-basis](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis)
property specifying duration and delay

flexBasis2: Float -> Float -> Css.Transitions.Transition
-}
flexBasis2 : Float -> Float -> Elm.Expression
flexBasis2 flexBasis2Arg flexBasis2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexBasis2Arg, Elm.float flexBasis2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [flex-basis](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis)
property specifying duration, delay and timing function

flexBasis3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
flexBasis3 : Float -> Float -> Elm.Expression -> Elm.Expression
flexBasis3 flexBasis3Arg flexBasis3Arg0 flexBasis3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexBasis3Arg, Elm.float flexBasis3Arg0, flexBasis3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [flex-grow](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow)
property specifying only the duration

flexGrow: Float -> Css.Transitions.Transition
-}
flexGrow : Float -> Elm.Expression
flexGrow flexGrowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexGrowArg ]


{-| Create a [`Transition`](#Transition) for the [flex-grow](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow)
property specifying duration and delay

flexGrow2: Float -> Float -> Css.Transitions.Transition
-}
flexGrow2 : Float -> Float -> Elm.Expression
flexGrow2 flexGrow2Arg flexGrow2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexGrow2Arg, Elm.float flexGrow2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [flex-grow](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow)
property specifying duration, delay and timing function

flexGrow3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
flexGrow3 : Float -> Float -> Elm.Expression -> Elm.Expression
flexGrow3 flexGrow3Arg flexGrow3Arg0 flexGrow3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexGrow3Arg, Elm.float flexGrow3Arg0, flexGrow3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [flex-shrink](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink)
property specifying only the duration

flexShrink: Float -> Css.Transitions.Transition
-}
flexShrink : Float -> Elm.Expression
flexShrink flexShrinkArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexShrinkArg ]


{-| Create a [`Transition`](#Transition) for the [flex-shrink](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink)
property specifying duration and delay

flexShrink2: Float -> Float -> Css.Transitions.Transition
-}
flexShrink2 : Float -> Float -> Elm.Expression
flexShrink2 flexShrink2Arg flexShrink2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexShrink2Arg, Elm.float flexShrink2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [flex-shrink](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink)
property specifying duration, delay and timing function

flexShrink3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
flexShrink3 : Float -> Float -> Elm.Expression -> Elm.Expression
flexShrink3 flexShrink3Arg flexShrink3Arg0 flexShrink3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float flexShrink3Arg, Elm.float flexShrink3Arg0, flexShrink3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [font](https://developer.mozilla.org/en-US/docs/Web/CSS/font)
property specifying only the duration

font: Float -> Css.Transitions.Transition
-}
font : Float -> Elm.Expression
font fontArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontArg ]


{-| Create a [`Transition`](#Transition) for the [font](https://developer.mozilla.org/en-US/docs/Web/CSS/font)
property specifying duration and delay

font2: Float -> Float -> Css.Transitions.Transition
-}
font2 : Float -> Float -> Elm.Expression
font2 font2Arg font2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float font2Arg, Elm.float font2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [font](https://developer.mozilla.org/en-US/docs/Web/CSS/font)
property specifying duration, delay and timing function

font3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
font3 : Float -> Float -> Elm.Expression -> Elm.Expression
font3 font3Arg font3Arg0 font3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float font3Arg, Elm.float font3Arg0, font3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [font-size](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size)
property specifying only the duration

fontSize: Float -> Css.Transitions.Transition
-}
fontSize : Float -> Elm.Expression
fontSize fontSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSizeArg ]


{-| Create a [`Transition`](#Transition) for the [font-size](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size)
property specifying duration and delay

fontSize2: Float -> Float -> Css.Transitions.Transition
-}
fontSize2 : Float -> Float -> Elm.Expression
fontSize2 fontSize2Arg fontSize2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSize2Arg, Elm.float fontSize2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [font-size](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size)
property specifying duration, delay and timing function

fontSize3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
fontSize3 : Float -> Float -> Elm.Expression -> Elm.Expression
fontSize3 fontSize3Arg fontSize3Arg0 fontSize3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSize3Arg, Elm.float fontSize3Arg0, fontSize3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [font-size-adjust](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size-adjust)
property specifying only the duration

fontSizeAdjust: Float -> Css.Transitions.Transition
-}
fontSizeAdjust : Float -> Elm.Expression
fontSizeAdjust fontSizeAdjustArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSizeAdjustArg ]


{-| Create a [`Transition`](#Transition) for the [font-size-adjust](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size-adjust)
property specifying duration and delay

fontSizeAdjust2: Float -> Float -> Css.Transitions.Transition
-}
fontSizeAdjust2 : Float -> Float -> Elm.Expression
fontSizeAdjust2 fontSizeAdjust2Arg fontSizeAdjust2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSizeAdjust2Arg, Elm.float fontSizeAdjust2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [font-size-adjust](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size-adjust)
property specifying duration, delay and timing function

fontSizeAdjust3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
fontSizeAdjust3 : Float -> Float -> Elm.Expression -> Elm.Expression
fontSizeAdjust3 fontSizeAdjust3Arg fontSizeAdjust3Arg0 fontSizeAdjust3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontSizeAdjust3Arg
        , Elm.float fontSizeAdjust3Arg0
        , fontSizeAdjust3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [font-size-stretch](https://developer.mozilla.org/en-US/docs/Web/CSS/font-size-stretch)
property specifying only the duration

fontStretch: Float -> Css.Transitions.Transition
-}
fontStretch : Float -> Elm.Expression
fontStretch fontStretchArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontStretchArg ]


{-| Create a [`Transition`](#Transition) for the [font-stretch](https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch)
property specifying duration and delay

fontStretch2: Float -> Float -> Css.Transitions.Transition
-}
fontStretch2 : Float -> Float -> Elm.Expression
fontStretch2 fontStretch2Arg fontStretch2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontStretch2Arg, Elm.float fontStretch2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [font-stretch](https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch)
property specifying duration, delay and timing function

fontStretch3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
fontStretch3 : Float -> Float -> Elm.Expression -> Elm.Expression
fontStretch3 fontStretch3Arg fontStretch3Arg0 fontStretch3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontStretch3Arg
        , Elm.float fontStretch3Arg0
        , fontStretch3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [font-variation-settings](https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings)
property specifying only the duration

fontVariationSettings: Float -> Css.Transitions.Transition
-}
fontVariationSettings : Float -> Elm.Expression
fontVariationSettings fontVariationSettingsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontVariationSettingsArg ]


{-| Create a [`Transition`](#Transition) for the [font-variation-settings](https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings)
property specifying duration and delay

fontVariationSettings2: Float -> Float -> Css.Transitions.Transition
-}
fontVariationSettings2 : Float -> Float -> Elm.Expression
fontVariationSettings2 fontVariationSettings2Arg fontVariationSettings2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontVariationSettings2Arg
        , Elm.float fontVariationSettings2Arg0
        ]


{-| Create a [`Transition`](#Transition) for the [font-variation-settings](https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings)
property specifying duration, delay and timing function

fontVariationSettings3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
fontVariationSettings3 : Float -> Float -> Elm.Expression -> Elm.Expression
fontVariationSettings3 fontVariationSettings3Arg fontVariationSettings3Arg0 fontVariationSettings3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontVariationSettings3Arg
        , Elm.float fontVariationSettings3Arg0
        , fontVariationSettings3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [font-weight](https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight)
property specifying only the duration

fontWeight: Float -> Css.Transitions.Transition
-}
fontWeight : Float -> Elm.Expression
fontWeight fontWeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontWeightArg ]


{-| Create a [`Transition`](#Transition) for the [font-weight](https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight)
property specifying duration and delay

fontWeight2: Float -> Float -> Css.Transitions.Transition
-}
fontWeight2 : Float -> Float -> Elm.Expression
fontWeight2 fontWeight2Arg fontWeight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontWeight2Arg, Elm.float fontWeight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [font-weight](https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight)
property specifying duration, delay and timing function

fontWeight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
fontWeight3 : Float -> Float -> Elm.Expression -> Elm.Expression
fontWeight3 fontWeight3Arg fontWeight3Arg0 fontWeight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float fontWeight3Arg, Elm.float fontWeight3Arg0, fontWeight3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [grid-column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-gap)
property specifying only the duration

gridColumnGap: Float -> Css.Transitions.Transition
-}
gridColumnGap : Float -> Elm.Expression
gridColumnGap gridColumnGapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridColumnGapArg ]


{-| Create a [`Transition`](#Transition) for the [grid-column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-gap)
property specifying duration and delay

gridColumnGap2: Float -> Float -> Css.Transitions.Transition
-}
gridColumnGap2 : Float -> Float -> Elm.Expression
gridColumnGap2 gridColumnGap2Arg gridColumnGap2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridColumnGap2Arg, Elm.float gridColumnGap2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [grid-column-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-gap)
property specifying duration, delay and timing function

gridColumnGap3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
gridColumnGap3 : Float -> Float -> Elm.Expression -> Elm.Expression
gridColumnGap3 gridColumnGap3Arg gridColumnGap3Arg0 gridColumnGap3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridColumnGap3Arg
        , Elm.float gridColumnGap3Arg0
        , gridColumnGap3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [grid-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-gap)
property specifying only the duration

gridGap: Float -> Css.Transitions.Transition
-}
gridGap : Float -> Elm.Expression
gridGap gridGapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridGapArg ]


{-| Create a [`Transition`](#Transition) for the [grid-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-gap)
property specifying duration and delay

gridGap2: Float -> Float -> Css.Transitions.Transition
-}
gridGap2 : Float -> Float -> Elm.Expression
gridGap2 gridGap2Arg gridGap2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridGap2Arg, Elm.float gridGap2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [grid-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-gap)
property specifying duration, delay and timing function

gridGap3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
gridGap3 : Float -> Float -> Elm.Expression -> Elm.Expression
gridGap3 gridGap3Arg gridGap3Arg0 gridGap3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridGap3Arg, Elm.float gridGap3Arg0, gridGap3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [grid-row-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-gap)
property specifying only the duration

gridRowGap: Float -> Css.Transitions.Transition
-}
gridRowGap : Float -> Elm.Expression
gridRowGap gridRowGapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridRowGapArg ]


{-| Create a [`Transition`](#Transition) for the [grid-row-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-gap)
property specifying duration and delay

gridRowGap2: Float -> Float -> Css.Transitions.Transition
-}
gridRowGap2 : Float -> Float -> Elm.Expression
gridRowGap2 gridRowGap2Arg gridRowGap2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridRowGap2Arg, Elm.float gridRowGap2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [grid-row-gap](https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-gap)
property specifying duration, delay and timing function

gridRowGap3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
gridRowGap3 : Float -> Float -> Elm.Expression -> Elm.Expression
gridRowGap3 gridRowGap3Arg gridRowGap3Arg0 gridRowGap3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float gridRowGap3Arg, Elm.float gridRowGap3Arg0, gridRowGap3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [height](https://developer.mozilla.org/en-US/docs/Web/CSS/height)
property specifying only the duration

height: Float -> Css.Transitions.Transition
-}
height : Float -> Elm.Expression
height heightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float heightArg ]


{-| Create a [`Transition`](#Transition) for the [height](https://developer.mozilla.org/en-US/docs/Web/CSS/height)
property specifying duration and delay

height2: Float -> Float -> Css.Transitions.Transition
-}
height2 : Float -> Float -> Elm.Expression
height2 height2Arg height2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float height2Arg, Elm.float height2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [height](https://developer.mozilla.org/en-US/docs/Web/CSS/height)
property specifying duration, delay and timing function

height3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
height3 : Float -> Float -> Elm.Expression -> Elm.Expression
height3 height3Arg height3Arg0 height3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float height3Arg, Elm.float height3Arg0, height3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [left](https://developer.mozilla.org/en-US/docs/Web/CSS/left)
property specifying only the duration

left: Float -> Css.Transitions.Transition
-}
left : Float -> Elm.Expression
left leftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float leftArg ]


{-| Create a [`Transition`](#Transition) for the [left](https://developer.mozilla.org/en-US/docs/Web/CSS/left)
property specifying duration and delay

left2: Float -> Float -> Css.Transitions.Transition
-}
left2 : Float -> Float -> Elm.Expression
left2 left2Arg left2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float left2Arg, Elm.float left2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [left](https://developer.mozilla.org/en-US/docs/Web/CSS/left)
property specifying duration, delay and timing function

left3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
left3 : Float -> Float -> Elm.Expression -> Elm.Expression
left3 left3Arg left3Arg0 left3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float left3Arg, Elm.float left3Arg0, left3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing)
property specifying only the duration

letterSpacing: Float -> Css.Transitions.Transition
-}
letterSpacing : Float -> Elm.Expression
letterSpacing letterSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float letterSpacingArg ]


{-| Create a [`Transition`](#Transition) for the [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing)
property specifying duration and delay

letterSpacing2: Float -> Float -> Css.Transitions.Transition
-}
letterSpacing2 : Float -> Float -> Elm.Expression
letterSpacing2 letterSpacing2Arg letterSpacing2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float letterSpacing2Arg, Elm.float letterSpacing2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing)
property specifying duration, delay and timing function

letterSpacing3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
letterSpacing3 : Float -> Float -> Elm.Expression -> Elm.Expression
letterSpacing3 letterSpacing3Arg letterSpacing3Arg0 letterSpacing3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float letterSpacing3Arg
        , Elm.float letterSpacing3Arg0
        , letterSpacing3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [line-height](https://developer.mozilla.org/en-US/docs/Web/CSS/line-height)
property specifying only the duration

lineHeight: Float -> Css.Transitions.Transition
-}
lineHeight : Float -> Elm.Expression
lineHeight lineHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float lineHeightArg ]


{-| Create a [`Transition`](#Transition) for the [line-height](https://developer.mozilla.org/en-US/docs/Web/CSS/line-height)
property specifying duration and delay

lineHeight2: Float -> Float -> Css.Transitions.Transition
-}
lineHeight2 : Float -> Float -> Elm.Expression
lineHeight2 lineHeight2Arg lineHeight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float lineHeight2Arg, Elm.float lineHeight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [line-height](https://developer.mozilla.org/en-US/docs/Web/CSS/line-height)
property specifying duration, delay and timing function

lineHeight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
lineHeight3 : Float -> Float -> Elm.Expression -> Elm.Expression
lineHeight3 lineHeight3Arg lineHeight3Arg0 lineHeight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float lineHeight3Arg, Elm.float lineHeight3Arg0, lineHeight3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [margin](https://developer.mozilla.org/en-US/docs/Web/CSS/margin)
property specifying only the duration

margin: Float -> Css.Transitions.Transition
-}
margin : Float -> Elm.Expression
margin marginArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginArg ]


{-| Create a [`Transition`](#Transition) for the [margin](https://developer.mozilla.org/en-US/docs/Web/CSS/margin)
property specifying duration and delay

margin2: Float -> Float -> Css.Transitions.Transition
-}
margin2 : Float -> Float -> Elm.Expression
margin2 margin2Arg margin2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float margin2Arg, Elm.float margin2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [margin](https://developer.mozilla.org/en-US/docs/Web/CSS/margin)
property specifying duration, delay and timing function

margin3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
margin3 : Float -> Float -> Elm.Expression -> Elm.Expression
margin3 margin3Arg margin3Arg0 margin3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float margin3Arg, Elm.float margin3Arg0, margin3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [margin-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom)
property specifying only the duration

marginBottom: Float -> Css.Transitions.Transition
-}
marginBottom : Float -> Elm.Expression
marginBottom marginBottomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginBottomArg ]


{-| Create a [`Transition`](#Transition) for the [margin-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom)
property specifying duration and delay

marginBottom2: Float -> Float -> Css.Transitions.Transition
-}
marginBottom2 : Float -> Float -> Elm.Expression
marginBottom2 marginBottom2Arg marginBottom2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginBottom2Arg, Elm.float marginBottom2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [margin-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom)
property specifying duration, delay and timing function

marginBottom3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
marginBottom3 : Float -> Float -> Elm.Expression -> Elm.Expression
marginBottom3 marginBottom3Arg marginBottom3Arg0 marginBottom3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginBottom3Arg
        , Elm.float marginBottom3Arg0
        , marginBottom3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [margin-left](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left)
property specifying only the duration

marginLeft: Float -> Css.Transitions.Transition
-}
marginLeft : Float -> Elm.Expression
marginLeft marginLeftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginLeftArg ]


{-| Create a [`Transition`](#Transition) for the [margin-left](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left)
property specifying duration and delay

marginLeft2: Float -> Float -> Css.Transitions.Transition
-}
marginLeft2 : Float -> Float -> Elm.Expression
marginLeft2 marginLeft2Arg marginLeft2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginLeft2Arg, Elm.float marginLeft2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [margin-left](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left)
property specifying duration, delay and timing function

marginLeft3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
marginLeft3 : Float -> Float -> Elm.Expression -> Elm.Expression
marginLeft3 marginLeft3Arg marginLeft3Arg0 marginLeft3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginLeft3Arg, Elm.float marginLeft3Arg0, marginLeft3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [margin-right](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right)
property specifying only the duration

marginRight: Float -> Css.Transitions.Transition
-}
marginRight : Float -> Elm.Expression
marginRight marginRightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginRightArg ]


{-| Create a [`Transition`](#Transition) for the [margin-right](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right)
property specifying duration and delay

marginRight2: Float -> Float -> Css.Transitions.Transition
-}
marginRight2 : Float -> Float -> Elm.Expression
marginRight2 marginRight2Arg marginRight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginRight2Arg, Elm.float marginRight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [margin-right](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right)
property specifying duration, delay and timing function

marginRight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
marginRight3 : Float -> Float -> Elm.Expression -> Elm.Expression
marginRight3 marginRight3Arg marginRight3Arg0 marginRight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginRight3Arg
        , Elm.float marginRight3Arg0
        , marginRight3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [margin-top](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top)
property specifying only the duration

marginTop: Float -> Css.Transitions.Transition
-}
marginTop : Float -> Elm.Expression
marginTop marginTopArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginTopArg ]


{-| Create a [`Transition`](#Transition) for the [margin-top](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top)
property specifying duration and delay

marginTop2: Float -> Float -> Css.Transitions.Transition
-}
marginTop2 : Float -> Float -> Elm.Expression
marginTop2 marginTop2Arg marginTop2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginTop2Arg, Elm.float marginTop2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [margin-top](https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top)
property specifying duration, delay and timing function

marginTop3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
marginTop3 : Float -> Float -> Elm.Expression -> Elm.Expression
marginTop3 marginTop3Arg marginTop3Arg0 marginTop3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float marginTop3Arg, Elm.float marginTop3Arg0, marginTop3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [mask](https://developer.mozilla.org/en-US/docs/Web/CSS/mask)
property specifying only the duration

mask: Float -> Css.Transitions.Transition
-}
mask : Float -> Elm.Expression
mask maskArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskArg ]


{-| Create a [`Transition`](#Transition) for the [mask](https://developer.mozilla.org/en-US/docs/Web/CSS/mask)
property specifying duration and delay

mask2: Float -> Float -> Css.Transitions.Transition
-}
mask2 : Float -> Float -> Elm.Expression
mask2 mask2Arg mask2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float mask2Arg, Elm.float mask2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [mask](https://developer.mozilla.org/en-US/docs/Web/CSS/mask)
property specifying duration, delay and timing function

mask3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
mask3 : Float -> Float -> Elm.Expression -> Elm.Expression
mask3 mask3Arg mask3Arg0 mask3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float mask3Arg, Elm.float mask3Arg0, mask3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [mask-position](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-position)
property specifying only the duration

maskPosition: Float -> Css.Transitions.Transition
-}
maskPosition : Float -> Elm.Expression
maskPosition maskPositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskPositionArg ]


{-| Create a [`Transition`](#Transition) for the [mask-position](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-position)
property specifying duration and delay

maskPosition2: Float -> Float -> Css.Transitions.Transition
-}
maskPosition2 : Float -> Float -> Elm.Expression
maskPosition2 maskPosition2Arg maskPosition2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskPosition2Arg, Elm.float maskPosition2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [mask-position](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-position)
property specifying duration, delay and timing function

maskPosition3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
maskPosition3 : Float -> Float -> Elm.Expression -> Elm.Expression
maskPosition3 maskPosition3Arg maskPosition3Arg0 maskPosition3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskPosition3Arg
        , Elm.float maskPosition3Arg0
        , maskPosition3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [mask-size](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-size)
property specifying only the duration

maskSize: Float -> Css.Transitions.Transition
-}
maskSize : Float -> Elm.Expression
maskSize maskSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskSizeArg ]


{-| Create a [`Transition`](#Transition) for the [mask-size](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-size)
property specifying duration and delay

maskSize2: Float -> Float -> Css.Transitions.Transition
-}
maskSize2 : Float -> Float -> Elm.Expression
maskSize2 maskSize2Arg maskSize2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskSize2Arg, Elm.float maskSize2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [mask-size](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-size)
property specifying duration, delay and timing function

maskSize3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
maskSize3 : Float -> Float -> Elm.Expression -> Elm.Expression
maskSize3 maskSize3Arg maskSize3Arg0 maskSize3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maskSize3Arg, Elm.float maskSize3Arg0, maskSize3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [max-height](https://developer.mozilla.org/en-US/docs/Web/CSS/max-height)
property specifying only the duration

maxHeight: Float -> Css.Transitions.Transition
-}
maxHeight : Float -> Elm.Expression
maxHeight maxHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxHeightArg ]


{-| Create a [`Transition`](#Transition) for the [max-height](https://developer.mozilla.org/en-US/docs/Web/CSS/max-height)
property specifying duration and delay

maxHeight2: Float -> Float -> Css.Transitions.Transition
-}
maxHeight2 : Float -> Float -> Elm.Expression
maxHeight2 maxHeight2Arg maxHeight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxHeight2Arg, Elm.float maxHeight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [max-height](https://developer.mozilla.org/en-US/docs/Web/CSS/max-height)
property specifying duration, delay and timing function

maxHeight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
maxHeight3 : Float -> Float -> Elm.Expression -> Elm.Expression
maxHeight3 maxHeight3Arg maxHeight3Arg0 maxHeight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxHeight3Arg, Elm.float maxHeight3Arg0, maxHeight3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [max-width](https://developer.mozilla.org/en-US/docs/Web/CSS/max-width)
property specifying only the duration

maxWidth: Float -> Css.Transitions.Transition
-}
maxWidth : Float -> Elm.Expression
maxWidth maxWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxWidthArg ]


{-| Create a [`Transition`](#Transition) for the [max-width](https://developer.mozilla.org/en-US/docs/Web/CSS/max-width)
property specifying duration and delay

maxWidth2: Float -> Float -> Css.Transitions.Transition
-}
maxWidth2 : Float -> Float -> Elm.Expression
maxWidth2 maxWidth2Arg maxWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxWidth2Arg, Elm.float maxWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [max-width](https://developer.mozilla.org/en-US/docs/Web/CSS/max-width)
property specifying duration, delay and timing function

maxWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
maxWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
maxWidth3 maxWidth3Arg maxWidth3Arg0 maxWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float maxWidth3Arg, Elm.float maxWidth3Arg0, maxWidth3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [min-height](https://developer.mozilla.org/en-US/docs/Web/CSS/min-height)
property specifying only the duration

minHeight: Float -> Css.Transitions.Transition
-}
minHeight : Float -> Elm.Expression
minHeight minHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minHeightArg ]


{-| Create a [`Transition`](#Transition) for the [min-height](https://developer.mozilla.org/en-US/docs/Web/CSS/min-height)
property specifying duration and delay

minHeight2: Float -> Float -> Css.Transitions.Transition
-}
minHeight2 : Float -> Float -> Elm.Expression
minHeight2 minHeight2Arg minHeight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minHeight2Arg, Elm.float minHeight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [min-height](https://developer.mozilla.org/en-US/docs/Web/CSS/min-height)
property specifying duration, delay and timing function

minHeight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
minHeight3 : Float -> Float -> Elm.Expression -> Elm.Expression
minHeight3 minHeight3Arg minHeight3Arg0 minHeight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minHeight3Arg, Elm.float minHeight3Arg0, minHeight3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [min-width](https://developer.mozilla.org/en-US/docs/Web/CSS/min-width)
property specifying only the duration

minWidth: Float -> Css.Transitions.Transition
-}
minWidth : Float -> Elm.Expression
minWidth minWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minWidthArg ]


{-| Create a [`Transition`](#Transition) for the [min-width](https://developer.mozilla.org/en-US/docs/Web/CSS/min-width)
property specifying duration and delay

minWidth2: Float -> Float -> Css.Transitions.Transition
-}
minWidth2 : Float -> Float -> Elm.Expression
minWidth2 minWidth2Arg minWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minWidth2Arg, Elm.float minWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [min-width](https://developer.mozilla.org/en-US/docs/Web/CSS/min-width)
property specifying duration, delay and timing function

minWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
minWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
minWidth3 minWidth3Arg minWidth3Arg0 minWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float minWidth3Arg, Elm.float minWidth3Arg0, minWidth3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [object-position](https://developer.mozilla.org/en-US/docs/Web/CSS/object-position)
property specifying only the duration

objectPosition: Float -> Css.Transitions.Transition
-}
objectPosition : Float -> Elm.Expression
objectPosition objectPositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float objectPositionArg ]


{-| Create a [`Transition`](#Transition) for the [object-position](https://developer.mozilla.org/en-US/docs/Web/CSS/object-position)
property specifying duration and delay

objectPosition2: Float -> Float -> Css.Transitions.Transition
-}
objectPosition2 : Float -> Float -> Elm.Expression
objectPosition2 objectPosition2Arg objectPosition2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float objectPosition2Arg, Elm.float objectPosition2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [object-position](https://developer.mozilla.org/en-US/docs/Web/CSS/object-position)
property specifying duration, delay and timing function

objectPosition3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
objectPosition3 : Float -> Float -> Elm.Expression -> Elm.Expression
objectPosition3 objectPosition3Arg objectPosition3Arg0 objectPosition3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float objectPosition3Arg
        , Elm.float objectPosition3Arg0
        , objectPosition3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [offset](https://developer.mozilla.org/en-US/docs/Web/CSS/offset)
property specifying only the duration

offset: Float -> Css.Transitions.Transition
-}
offset : Float -> Elm.Expression
offset offsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetArg ]


{-| Create a [`Transition`](#Transition) for the [offset](https://developer.mozilla.org/en-US/docs/Web/CSS/offset)
property specifying duration and delay

offset2: Float -> Float -> Css.Transitions.Transition
-}
offset2 : Float -> Float -> Elm.Expression
offset2 offset2Arg offset2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offset2Arg, Elm.float offset2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [offset](https://developer.mozilla.org/en-US/docs/Web/CSS/offset)
property specifying duration, delay and timing function

offset3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
offset3 : Float -> Float -> Elm.Expression -> Elm.Expression
offset3 offset3Arg offset3Arg0 offset3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offset3Arg, Elm.float offset3Arg0, offset3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [offset-anchor](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-anchor)
property specifying only the duration

offsetAnchor: Float -> Css.Transitions.Transition
-}
offsetAnchor : Float -> Elm.Expression
offsetAnchor offsetAnchorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetAnchorArg ]


{-| Create a [`Transition`](#Transition) for the [offset-anchor](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-anchor)
property specifying duration and delay

offsetAnchor2: Float -> Float -> Css.Transitions.Transition
-}
offsetAnchor2 : Float -> Float -> Elm.Expression
offsetAnchor2 offsetAnchor2Arg offsetAnchor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetAnchor2Arg, Elm.float offsetAnchor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [offset-anchor](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-anchor)
property specifying duration, delay and timing function

offsetAnchor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
offsetAnchor3 : Float -> Float -> Elm.Expression -> Elm.Expression
offsetAnchor3 offsetAnchor3Arg offsetAnchor3Arg0 offsetAnchor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetAnchor3Arg
        , Elm.float offsetAnchor3Arg0
        , offsetAnchor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [offset-distance](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-distance)
property specifying only the duration

offsetDistance: Float -> Css.Transitions.Transition
-}
offsetDistance : Float -> Elm.Expression
offsetDistance offsetDistanceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetDistanceArg ]


{-| Create a [`Transition`](#Transition) for the [offset-distance](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-distance)
property specifying duration and delay

offsetDistance2: Float -> Float -> Css.Transitions.Transition
-}
offsetDistance2 : Float -> Float -> Elm.Expression
offsetDistance2 offsetDistance2Arg offsetDistance2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetDistance2Arg, Elm.float offsetDistance2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [offset-distance](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-distance)
property specifying duration, delay and timing function

offsetDistance3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
offsetDistance3 : Float -> Float -> Elm.Expression -> Elm.Expression
offsetDistance3 offsetDistance3Arg offsetDistance3Arg0 offsetDistance3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetDistance3Arg
        , Elm.float offsetDistance3Arg0
        , offsetDistance3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [offset-path](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-path)
property specifying only the duration

offsetPath: Float -> Css.Transitions.Transition
-}
offsetPath : Float -> Elm.Expression
offsetPath offsetPathArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetPathArg ]


{-| Create a [`Transition`](#Transition) for the [offset-path](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-path)
property specifying duration and delay

offsetPath2: Float -> Float -> Css.Transitions.Transition
-}
offsetPath2 : Float -> Float -> Elm.Expression
offsetPath2 offsetPath2Arg offsetPath2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetPath2Arg, Elm.float offsetPath2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [offset-path](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-path)
property specifying duration, delay and timing function

offsetPath3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
offsetPath3 : Float -> Float -> Elm.Expression -> Elm.Expression
offsetPath3 offsetPath3Arg offsetPath3Arg0 offsetPath3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetPath3Arg, Elm.float offsetPath3Arg0, offsetPath3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [offset-rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-rotate)
property specifying only the duration

offsetRotate: Float -> Css.Transitions.Transition
-}
offsetRotate : Float -> Elm.Expression
offsetRotate offsetRotateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetRotateArg ]


{-| Create a [`Transition`](#Transition) for the [offset-rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-rotate)
property specifying duration and delay

offsetRotate2: Float -> Float -> Css.Transitions.Transition
-}
offsetRotate2 : Float -> Float -> Elm.Expression
offsetRotate2 offsetRotate2Arg offsetRotate2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetRotate2Arg, Elm.float offsetRotate2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [offset-rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/offset-rotate)
property specifying duration, delay and timing function

offsetRotate3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
offsetRotate3 : Float -> Float -> Elm.Expression -> Elm.Expression
offsetRotate3 offsetRotate3Arg offsetRotate3Arg0 offsetRotate3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float offsetRotate3Arg
        , Elm.float offsetRotate3Arg0
        , offsetRotate3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [opacity](https://developer.mozilla.org/en-US/docs/Web/CSS/order)
property specifying only the duration

opacity: Float -> Css.Transitions.Transition
-}
opacity : Float -> Elm.Expression
opacity opacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float opacityArg ]


{-| Create a [`Transition`](#Transition) for the [opacity](https://developer.mozilla.org/en-US/docs/Web/CSS/opacity)
property specifying duration and delay

opacity2: Float -> Float -> Css.Transitions.Transition
-}
opacity2 : Float -> Float -> Elm.Expression
opacity2 opacity2Arg opacity2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float opacity2Arg, Elm.float opacity2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [opacity](https://developer.mozilla.org/en-US/docs/Web/CSS/opacity)
property specifying duration, delay and timing function

opacity3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
opacity3 : Float -> Float -> Elm.Expression -> Elm.Expression
opacity3 opacity3Arg opacity3Arg0 opacity3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float opacity3Arg, Elm.float opacity3Arg0, opacity3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [order](https://developer.mozilla.org/en-US/docs/Web/CSS/order)
property specifying only the duration

order: Float -> Css.Transitions.Transition
-}
order : Float -> Elm.Expression
order orderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float orderArg ]


{-| Create a [`Transition`](#Transition) for the [order](https://developer.mozilla.org/en-US/docs/Web/CSS/order)
property specifying duration and delay

order2: Float -> Float -> Css.Transitions.Transition
-}
order2 : Float -> Float -> Elm.Expression
order2 order2Arg order2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float order2Arg, Elm.float order2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [order](https://developer.mozilla.org/en-US/docs/Web/CSS/order)
property specifying duration, delay and timing function

order3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
order3 : Float -> Float -> Elm.Expression -> Elm.Expression
order3 order3Arg order3Arg0 order3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float order3Arg, Elm.float order3Arg0, order3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [outline](https://developer.mozilla.org/en-US/docs/Web/CSS/outline)
property specifying only the duration

outline: Float -> Css.Transitions.Transition
-}
outline : Float -> Elm.Expression
outline outlineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineArg ]


{-| Create a [`Transition`](#Transition) for the [outline](https://developer.mozilla.org/en-US/docs/Web/CSS/outline)
property specifying duration and delay

outline2: Float -> Float -> Css.Transitions.Transition
-}
outline2 : Float -> Float -> Elm.Expression
outline2 outline2Arg outline2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outline2Arg, Elm.float outline2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [outline](https://developer.mozilla.org/en-US/docs/Web/CSS/outline)
property specifying duration, delay and timing function

outline3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
outline3 : Float -> Float -> Elm.Expression -> Elm.Expression
outline3 outline3Arg outline3Arg0 outline3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outline3Arg, Elm.float outline3Arg0, outline3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [outline-color](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color)
property specifying only the duration

outlineColor: Float -> Css.Transitions.Transition
-}
outlineColor : Float -> Elm.Expression
outlineColor outlineColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineColorArg ]


{-| Create a [`Transition`](#Transition) for the [outline-color](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color)
property specifying duration and delay

outlineColor2: Float -> Float -> Css.Transitions.Transition
-}
outlineColor2 : Float -> Float -> Elm.Expression
outlineColor2 outlineColor2Arg outlineColor2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineColor2Arg, Elm.float outlineColor2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [outline-color](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color)
property specifying duration, delay and timing function

outlineColor3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
outlineColor3 : Float -> Float -> Elm.Expression -> Elm.Expression
outlineColor3 outlineColor3Arg outlineColor3Arg0 outlineColor3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineColor3Arg
        , Elm.float outlineColor3Arg0
        , outlineColor3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [outline-offset](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset)
property specifying only the duration

outlineOffset: Float -> Css.Transitions.Transition
-}
outlineOffset : Float -> Elm.Expression
outlineOffset outlineOffsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineOffsetArg ]


{-| Create a [`Transition`](#Transition) for the [outline-offset](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset)
property specifying duration and delay

outlineOffset2: Float -> Float -> Css.Transitions.Transition
-}
outlineOffset2 : Float -> Float -> Elm.Expression
outlineOffset2 outlineOffset2Arg outlineOffset2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineOffset2Arg, Elm.float outlineOffset2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [outline-offset](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset)
property specifying duration, delay and timing function

outlineOffset3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
outlineOffset3 : Float -> Float -> Elm.Expression -> Elm.Expression
outlineOffset3 outlineOffset3Arg outlineOffset3Arg0 outlineOffset3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineOffset3Arg
        , Elm.float outlineOffset3Arg0
        , outlineOffset3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [outline-width](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width)
property specifying only the duration

outlineWidth: Float -> Css.Transitions.Transition
-}
outlineWidth : Float -> Elm.Expression
outlineWidth outlineWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineWidthArg ]


{-| Create a [`Transition`](#Transition) for the [outline-width](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width)
property specifying duration and delay

outlineWidth2: Float -> Float -> Css.Transitions.Transition
-}
outlineWidth2 : Float -> Float -> Elm.Expression
outlineWidth2 outlineWidth2Arg outlineWidth2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineWidth2Arg, Elm.float outlineWidth2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [outline-width](https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width)
property specifying duration, delay and timing function

outlineWidth3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
outlineWidth3 : Float -> Float -> Elm.Expression -> Elm.Expression
outlineWidth3 outlineWidth3Arg outlineWidth3Arg0 outlineWidth3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float outlineWidth3Arg
        , Elm.float outlineWidth3Arg0
        , outlineWidth3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [padding](https://developer.mozilla.org/en-US/docs/Web/CSS/padding)
property specifying only the duration

padding: Float -> Css.Transitions.Transition
-}
padding : Float -> Elm.Expression
padding paddingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingArg ]


{-| Create a [`Transition`](#Transition) for the [padding](https://developer.mozilla.org/en-US/docs/Web/CSS/padding)
property specifying duration and delay

padding2: Float -> Float -> Css.Transitions.Transition
-}
padding2 : Float -> Float -> Elm.Expression
padding2 padding2Arg padding2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float padding2Arg, Elm.float padding2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding](https://developer.mozilla.org/en-US/docs/Web/CSS/padding)
property specifying duration, delay and timing function

padding3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
padding3 : Float -> Float -> Elm.Expression -> Elm.Expression
padding3 padding3Arg padding3Arg0 padding3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float padding3Arg, Elm.float padding3Arg0, padding3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [padding-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom)
property specifying only the duration

paddingBottom: Float -> Css.Transitions.Transition
-}
paddingBottom : Float -> Elm.Expression
paddingBottom paddingBottomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingBottomArg ]


{-| Create a [`Transition`](#Transition) for the [padding-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom)
property specifying duration and delay

paddingBottom2: Float -> Float -> Css.Transitions.Transition
-}
paddingBottom2 : Float -> Float -> Elm.Expression
paddingBottom2 paddingBottom2Arg paddingBottom2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingBottom2Arg, Elm.float paddingBottom2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding-bottom](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom)
property specifying duration, delay and timing function

paddingBottom3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
paddingBottom3 : Float -> Float -> Elm.Expression -> Elm.Expression
paddingBottom3 paddingBottom3Arg paddingBottom3Arg0 paddingBottom3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingBottom3Arg
        , Elm.float paddingBottom3Arg0
        , paddingBottom3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [padding-left](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left)
property specifying only the duration

paddingLeft: Float -> Css.Transitions.Transition
-}
paddingLeft : Float -> Elm.Expression
paddingLeft paddingLeftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingLeftArg ]


{-| Create a [`Transition`](#Transition) for the [padding-left](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left)
property specifying duration and delay

paddingLeft2: Float -> Float -> Css.Transitions.Transition
-}
paddingLeft2 : Float -> Float -> Elm.Expression
paddingLeft2 paddingLeft2Arg paddingLeft2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingLeft2Arg, Elm.float paddingLeft2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding-left](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left)
property specifying duration, delay and timing function

paddingLeft3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
paddingLeft3 : Float -> Float -> Elm.Expression -> Elm.Expression
paddingLeft3 paddingLeft3Arg paddingLeft3Arg0 paddingLeft3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingLeft3Arg
        , Elm.float paddingLeft3Arg0
        , paddingLeft3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [padding-right](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right)
property specifying only the duration

paddingRight: Float -> Css.Transitions.Transition
-}
paddingRight : Float -> Elm.Expression
paddingRight paddingRightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingRightArg ]


{-| Create a [`Transition`](#Transition) for the [padding-right](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right)
property specifying duration and delay

paddingRight2: Float -> Float -> Css.Transitions.Transition
-}
paddingRight2 : Float -> Float -> Elm.Expression
paddingRight2 paddingRight2Arg paddingRight2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingRight2Arg, Elm.float paddingRight2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding-right](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right)
property specifying duration, delay and timing function

paddingRight3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
paddingRight3 : Float -> Float -> Elm.Expression -> Elm.Expression
paddingRight3 paddingRight3Arg paddingRight3Arg0 paddingRight3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingRight3Arg
        , Elm.float paddingRight3Arg0
        , paddingRight3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [padding-top](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top)
property specifying only the duration

paddingTop: Float -> Css.Transitions.Transition
-}
paddingTop : Float -> Elm.Expression
paddingTop paddingTopArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingTopArg ]


{-| Create a [`Transition`](#Transition) for the [padding-top](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top)
property specifying duration and delay

paddingTop2: Float -> Float -> Css.Transitions.Transition
-}
paddingTop2 : Float -> Float -> Elm.Expression
paddingTop2 paddingTop2Arg paddingTop2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingTop2Arg, Elm.float paddingTop2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding-top](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top)
property specifying duration, delay and timing function

paddingTop3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
paddingTop3 : Float -> Float -> Elm.Expression -> Elm.Expression
paddingTop3 paddingTop3Arg paddingTop3Arg0 paddingTop3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float paddingTop3Arg, Elm.float paddingTop3Arg0, paddingTop3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [right](https://developer.mozilla.org/en-US/docs/Web/CSS/right)
property specifying only the duration

right: Float -> Css.Transitions.Transition
-}
right : Float -> Elm.Expression
right rightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float rightArg ]


{-| Create a [`Transition`](#Transition) for the [right](https://developer.mozilla.org/en-US/docs/Web/CSS/right)
property specifying duration and delay

right2: Float -> Float -> Css.Transitions.Transition
-}
right2 : Float -> Float -> Elm.Expression
right2 right2Arg right2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float right2Arg, Elm.float right2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [padding-right](https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right)
property specifying duration, delay and timing function

right3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
right3 : Float -> Float -> Elm.Expression -> Elm.Expression
right3 right3Arg right3Arg0 right3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float right3Arg, Elm.float right3Arg0, right3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [tab-size](https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size)
property specifying only the duration

tabSize: Float -> Css.Transitions.Transition
-}
tabSize : Float -> Elm.Expression
tabSize tabSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float tabSizeArg ]


{-| Create a [`Transition`](#Transition) for the [tab-size](https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size)
property specifying duration and delay

tabSize2: Float -> Float -> Css.Transitions.Transition
-}
tabSize2 : Float -> Float -> Elm.Expression
tabSize2 tabSize2Arg tabSize2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float tabSize2Arg, Elm.float tabSize2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [tab-size](https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size)
property specifying duration, delay and timing function

tabSize3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
tabSize3 : Float -> Float -> Elm.Expression -> Elm.Expression
tabSize3 tabSize3Arg tabSize3Arg0 tabSize3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float tabSize3Arg, Elm.float tabSize3Arg0, tabSize3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [text-indent](https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent)
property specifying only the duration

textIndent: Float -> Css.Transitions.Transition
-}
textIndent : Float -> Elm.Expression
textIndent textIndentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textIndentArg ]


{-| Create a [`Transition`](#Transition) for the [text-indent](https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent)
property specifying duration and delay

textIndent2: Float -> Float -> Css.Transitions.Transition
-}
textIndent2 : Float -> Float -> Elm.Expression
textIndent2 textIndent2Arg textIndent2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textIndent2Arg, Elm.float textIndent2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [text-indent](https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent)
property specifying duration, delay and timing function

textIndent3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
textIndent3 : Float -> Float -> Elm.Expression -> Elm.Expression
textIndent3 textIndent3Arg textIndent3Arg0 textIndent3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textIndent3Arg, Elm.float textIndent3Arg0, textIndent3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [text-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow)
property specifying only the duration

textShadow: Float -> Css.Transitions.Transition
-}
textShadow : Float -> Elm.Expression
textShadow textShadowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textShadowArg ]


{-| Create a [`Transition`](#Transition) for the [text-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow)
property specifying duration and delay

textShadow2: Float -> Float -> Css.Transitions.Transition
-}
textShadow2 : Float -> Float -> Elm.Expression
textShadow2 textShadow2Arg textShadow2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textShadow2Arg, Elm.float textShadow2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [text-shadow](https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow)
property specifying duration, delay and timing function

textShadow3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
textShadow3 : Float -> Float -> Elm.Expression -> Elm.Expression
textShadow3 textShadow3Arg textShadow3Arg0 textShadow3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float textShadow3Arg, Elm.float textShadow3Arg0, textShadow3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [top](https://developer.mozilla.org/en-US/docs/Web/CSS/top)
property specifying only the duration

top: Float -> Css.Transitions.Transition
-}
top : Float -> Elm.Expression
top topArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float topArg ]


{-| Create a [`Transition`](#Transition) for the [top](https://developer.mozilla.org/en-US/docs/Web/CSS/top)
property specifying duration and delay

top2: Float -> Float -> Css.Transitions.Transition
-}
top2 : Float -> Float -> Elm.Expression
top2 top2Arg top2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float top2Arg, Elm.float top2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [top](https://developer.mozilla.org/en-US/docs/Web/CSS/top)
property specifying duration, delay and timing function

top3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
top3 : Float -> Float -> Elm.Expression -> Elm.Expression
top3 top3Arg top3Arg0 top3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float top3Arg, Elm.float top3Arg0, top3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [transform](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
property specifying only the duration

transform: Float -> Css.Transitions.Transition
-}
transform : Float -> Elm.Expression
transform transformArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transformArg ]


{-| Create a [`Transition`](#Transition) for the [transform](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
property specifying duration and delay

transform2: Float -> Float -> Css.Transitions.Transition
-}
transform2 : Float -> Float -> Elm.Expression
transform2 transform2Arg transform2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transform2Arg, Elm.float transform2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [transform](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
property specifying duration, delay and timing function

transform3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
transform3 : Float -> Float -> Elm.Expression -> Elm.Expression
transform3 transform3Arg transform3Arg0 transform3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transform3Arg, Elm.float transform3Arg0, transform3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)
property specifying only the duration

transformOrigin: Float -> Css.Transitions.Transition
-}
transformOrigin : Float -> Elm.Expression
transformOrigin transformOriginArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transformOriginArg ]


{-| Create a [`Transition`](#Transition) for the [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)
property specifying duration and delay

transformOrigin2: Float -> Float -> Css.Transitions.Transition
-}
transformOrigin2 : Float -> Float -> Elm.Expression
transformOrigin2 transformOrigin2Arg transformOrigin2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transformOrigin2Arg, Elm.float transformOrigin2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)
property specifying duration, delay and timing function

transformOrigin3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
transformOrigin3 : Float -> Float -> Elm.Expression -> Elm.Expression
transformOrigin3 transformOrigin3Arg transformOrigin3Arg0 transformOrigin3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float transformOrigin3Arg
        , Elm.float transformOrigin3Arg0
        , transformOrigin3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [vertical-align](https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align)
property specifying only the duration

verticalAlign: Float -> Css.Transitions.Transition
-}
verticalAlign : Float -> Elm.Expression
verticalAlign verticalAlignArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float verticalAlignArg ]


{-| Create a [`Transition`](#Transition) for the [vertical-align](https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align)
property specifying duration and delay

verticalAlign2: Float -> Float -> Css.Transitions.Transition
-}
verticalAlign2 : Float -> Float -> Elm.Expression
verticalAlign2 verticalAlign2Arg verticalAlign2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float verticalAlign2Arg, Elm.float verticalAlign2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [vertical-align](https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align)
property specifying duration, delay and timing function

verticalAlign3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
verticalAlign3 : Float -> Float -> Elm.Expression -> Elm.Expression
verticalAlign3 verticalAlign3Arg verticalAlign3Arg0 verticalAlign3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float verticalAlign3Arg
        , Elm.float verticalAlign3Arg0
        , verticalAlign3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [visibility](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility)
property specifying only the duration

visibility: Float -> Css.Transitions.Transition
-}
visibility : Float -> Elm.Expression
visibility visibilityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float visibilityArg ]


{-| Create a [`Transition`](#Transition) for the [visibility](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility)
property specifying duration and delay

visibility2: Float -> Float -> Css.Transitions.Transition
-}
visibility2 : Float -> Float -> Elm.Expression
visibility2 visibility2Arg visibility2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float visibility2Arg, Elm.float visibility2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [visibility](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility)
property specifying duration, delay and timing function

visibility3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
visibility3 : Float -> Float -> Elm.Expression -> Elm.Expression
visibility3 visibility3Arg visibility3Arg0 visibility3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float visibility3Arg, Elm.float visibility3Arg0, visibility3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [width](https://developer.mozilla.org/en-US/docs/Web/CSS/width)
property specifying only the duration

width: Float -> Css.Transitions.Transition
-}
width : Float -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float widthArg ]


{-| Create a [`Transition`](#Transition) for the [width](https://developer.mozilla.org/en-US/docs/Web/CSS/width)
property specifying duration and delay

width2: Float -> Float -> Css.Transitions.Transition
-}
width2 : Float -> Float -> Elm.Expression
width2 width2Arg width2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float width2Arg, Elm.float width2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [width](https://developer.mozilla.org/en-US/docs/Web/CSS/width)
property specifying duration, delay and timing function

width3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
width3 : Float -> Float -> Elm.Expression -> Elm.Expression
width3 width3Arg width3Arg0 width3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float width3Arg, Elm.float width3Arg0, width3Arg1 ]


{-| Create a [`Transition`](#Transition) for the [word-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing)
property specifying only the duration

wordSpacing: Float -> Css.Transitions.Transition
-}
wordSpacing : Float -> Elm.Expression
wordSpacing wordSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float wordSpacingArg ]


{-| Create a [`Transition`](#Transition) for the [word-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing)
property specifying duration and delay

wordSpacing2: Float -> Float -> Css.Transitions.Transition
-}
wordSpacing2 : Float -> Float -> Elm.Expression
wordSpacing2 wordSpacing2Arg wordSpacing2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float wordSpacing2Arg, Elm.float wordSpacing2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [word-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing)
property specifying duration, delay and timing function

wordSpacing3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
wordSpacing3 : Float -> Float -> Elm.Expression -> Elm.Expression
wordSpacing3 wordSpacing3Arg wordSpacing3Arg0 wordSpacing3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float wordSpacing3Arg
        , Elm.float wordSpacing3Arg0
        , wordSpacing3Arg1
        ]


{-| Create a [`Transition`](#Transition) for the [z-index](https://developer.mozilla.org/en-US/docs/Web/CSS/z-index)
property specifying only the duration

zIndex: Float -> Css.Transitions.Transition
-}
zIndex : Float -> Elm.Expression
zIndex zIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float zIndexArg ]


{-| Create a [`Transition`](#Transition) for the [z-index](https://developer.mozilla.org/en-US/docs/Web/CSS/z-index)
property specifying duration and delay

zIndex2: Float -> Float -> Css.Transitions.Transition
-}
zIndex2 : Float -> Float -> Elm.Expression
zIndex2 zIndex2Arg zIndex2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float zIndex2Arg, Elm.float zIndex2Arg0 ]


{-| Create a [`Transition`](#Transition) for the [z-index](https://developer.mozilla.org/en-US/docs/Web/CSS/z-index)
property specifying duration, delay and timing function

zIndex3: Float -> Float -> Css.Transitions.TimingFunction -> Css.Transitions.Transition
-}
zIndex3 : Float -> Float -> Elm.Expression -> Elm.Expression
zIndex3 zIndex3Arg zIndex3Arg0 zIndex3Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
        )
        [ Elm.float zIndex3Arg, Elm.float zIndex3Arg0, zIndex3Arg1 ]


annotation_ : { transition : Type.Annotation }
annotation_ =
    { transition = Type.namedWith [ "Css", "Transitions" ] "Transition" [] }


call_ :
    { transition : Elm.Expression -> Elm.Expression
    , cubicBezier :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , background : Elm.Expression -> Elm.Expression
    , background2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , background3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundColor : Elm.Expression -> Elm.Expression
    , backgroundColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundPosition : Elm.Expression -> Elm.Expression
    , backgroundPosition2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundPosition3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundSize : Elm.Expression -> Elm.Expression
    , backgroundSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , backgroundSize3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , border : Elm.Expression -> Elm.Expression
    , border2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , border3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottom : Elm.Expression -> Elm.Expression
    , borderBottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottom3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomColor : Elm.Expression -> Elm.Expression
    , borderBottomColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomLeftRadius : Elm.Expression -> Elm.Expression
    , borderBottomLeftRadius2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomLeftRadius3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomRightRadius : Elm.Expression -> Elm.Expression
    , borderBottomRightRadius2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomRightRadius3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomWidth : Elm.Expression -> Elm.Expression
    , borderBottomWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderBottomWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderColor : Elm.Expression -> Elm.Expression
    , borderColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeft : Elm.Expression -> Elm.Expression
    , borderLeft2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeft3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeftColor : Elm.Expression -> Elm.Expression
    , borderLeftColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeftColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeftWidth : Elm.Expression -> Elm.Expression
    , borderLeftWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderLeftWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRadius : Elm.Expression -> Elm.Expression
    , borderRadius2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRadius3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRight : Elm.Expression -> Elm.Expression
    , borderRight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRightColor : Elm.Expression -> Elm.Expression
    , borderRightColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRightColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRightWidth : Elm.Expression -> Elm.Expression
    , borderRightWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderRightWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTop : Elm.Expression -> Elm.Expression
    , borderTop2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTop3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopColor : Elm.Expression -> Elm.Expression
    , borderTopColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopLeftRadius : Elm.Expression -> Elm.Expression
    , borderTopLeftRadius2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopLeftRadius3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopRightRadius : Elm.Expression -> Elm.Expression
    , borderTopRightRadius2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopRightRadius3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopWidth : Elm.Expression -> Elm.Expression
    , borderTopWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderTopWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderWidth : Elm.Expression -> Elm.Expression
    , borderWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , borderWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , bottom : Elm.Expression -> Elm.Expression
    , bottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , bottom3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , boxShadow : Elm.Expression -> Elm.Expression
    , boxShadow2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , boxShadow3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , caretColor : Elm.Expression -> Elm.Expression
    , caretColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , caretColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , clip : Elm.Expression -> Elm.Expression
    , clip2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , clip3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , clipPath : Elm.Expression -> Elm.Expression
    , clipPath2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , clipPath3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , color : Elm.Expression -> Elm.Expression
    , color2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , color3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnCount : Elm.Expression -> Elm.Expression
    , columnCount2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnCount3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnGap : Elm.Expression -> Elm.Expression
    , columnGap2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnGap3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRule : Elm.Expression -> Elm.Expression
    , columnRule2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRule3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRuleColor : Elm.Expression -> Elm.Expression
    , columnRuleColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRuleColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRuleWidth : Elm.Expression -> Elm.Expression
    , columnRuleWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnRuleWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnWidth : Elm.Expression -> Elm.Expression
    , columnWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columnWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , columns : Elm.Expression -> Elm.Expression
    , columns2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , columns3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , filter : Elm.Expression -> Elm.Expression
    , filter2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , filter3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , flex : Elm.Expression -> Elm.Expression
    , flex2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , flex3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexBasis : Elm.Expression -> Elm.Expression
    , flexBasis2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexBasis3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexGrow : Elm.Expression -> Elm.Expression
    , flexGrow2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexGrow3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexShrink : Elm.Expression -> Elm.Expression
    , flexShrink2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , flexShrink3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , font : Elm.Expression -> Elm.Expression
    , font2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , font3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontSize : Elm.Expression -> Elm.Expression
    , fontSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontSize3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontSizeAdjust : Elm.Expression -> Elm.Expression
    , fontSizeAdjust2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontSizeAdjust3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontStretch : Elm.Expression -> Elm.Expression
    , fontStretch2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontStretch3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontVariationSettings : Elm.Expression -> Elm.Expression
    , fontVariationSettings2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontVariationSettings3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontWeight : Elm.Expression -> Elm.Expression
    , fontWeight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fontWeight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridColumnGap : Elm.Expression -> Elm.Expression
    , gridColumnGap2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridColumnGap3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridGap : Elm.Expression -> Elm.Expression
    , gridGap2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridGap3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridRowGap : Elm.Expression -> Elm.Expression
    , gridRowGap2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , gridRowGap3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , height : Elm.Expression -> Elm.Expression
    , height2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , height3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , left : Elm.Expression -> Elm.Expression
    , left2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , left3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , letterSpacing : Elm.Expression -> Elm.Expression
    , letterSpacing2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , letterSpacing3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , lineHeight : Elm.Expression -> Elm.Expression
    , lineHeight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lineHeight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , margin : Elm.Expression -> Elm.Expression
    , margin2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , margin3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginBottom : Elm.Expression -> Elm.Expression
    , marginBottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginBottom3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginLeft : Elm.Expression -> Elm.Expression
    , marginLeft2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginLeft3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginRight : Elm.Expression -> Elm.Expression
    , marginRight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginRight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginTop : Elm.Expression -> Elm.Expression
    , marginTop2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , marginTop3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , mask : Elm.Expression -> Elm.Expression
    , mask2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mask3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maskPosition : Elm.Expression -> Elm.Expression
    , maskPosition2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , maskPosition3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maskSize : Elm.Expression -> Elm.Expression
    , maskSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , maskSize3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maxHeight : Elm.Expression -> Elm.Expression
    , maxHeight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , maxHeight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maxWidth : Elm.Expression -> Elm.Expression
    , maxWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , maxWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , minHeight : Elm.Expression -> Elm.Expression
    , minHeight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , minHeight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , minWidth : Elm.Expression -> Elm.Expression
    , minWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , minWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , objectPosition : Elm.Expression -> Elm.Expression
    , objectPosition2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , objectPosition3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , offset : Elm.Expression -> Elm.Expression
    , offset2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , offset3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetAnchor : Elm.Expression -> Elm.Expression
    , offsetAnchor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetAnchor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetDistance : Elm.Expression -> Elm.Expression
    , offsetDistance2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetDistance3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetPath : Elm.Expression -> Elm.Expression
    , offsetPath2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetPath3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetRotate : Elm.Expression -> Elm.Expression
    , offsetRotate2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , offsetRotate3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , opacity : Elm.Expression -> Elm.Expression
    , opacity2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , opacity3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , order : Elm.Expression -> Elm.Expression
    , order2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , order3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , outline : Elm.Expression -> Elm.Expression
    , outline2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , outline3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineColor : Elm.Expression -> Elm.Expression
    , outlineColor2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineColor3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineOffset : Elm.Expression -> Elm.Expression
    , outlineOffset2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineOffset3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineWidth : Elm.Expression -> Elm.Expression
    , outlineWidth2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , outlineWidth3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , padding : Elm.Expression -> Elm.Expression
    , padding2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , padding3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingBottom : Elm.Expression -> Elm.Expression
    , paddingBottom2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingBottom3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingLeft : Elm.Expression -> Elm.Expression
    , paddingLeft2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingLeft3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingRight : Elm.Expression -> Elm.Expression
    , paddingRight2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingRight3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingTop : Elm.Expression -> Elm.Expression
    , paddingTop2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingTop3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , right : Elm.Expression -> Elm.Expression
    , right2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , right3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , tabSize : Elm.Expression -> Elm.Expression
    , tabSize2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tabSize3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , textIndent : Elm.Expression -> Elm.Expression
    , textIndent2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textIndent3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , textShadow : Elm.Expression -> Elm.Expression
    , textShadow2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textShadow3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , top : Elm.Expression -> Elm.Expression
    , top2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , top3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , transform : Elm.Expression -> Elm.Expression
    , transform2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , transform3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , transformOrigin : Elm.Expression -> Elm.Expression
    , transformOrigin2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , transformOrigin3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , verticalAlign : Elm.Expression -> Elm.Expression
    , verticalAlign2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , verticalAlign3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , visibility : Elm.Expression -> Elm.Expression
    , visibility2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , visibility3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , width2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , width3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , wordSpacing : Elm.Expression -> Elm.Expression
    , wordSpacing2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , wordSpacing3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , zIndex : Elm.Expression -> Elm.Expression
    , zIndex2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , zIndex3 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { transition =
        \transitionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Transitions" ]
                                        "Transition"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ transitionArg ]
    , cubicBezier =
        \cubicBezierArg cubicBezierArg0 cubicBezierArg1 cubicBezierArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "cubicBezier"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.float
                                , Type.float
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                )
                            )
                    }
                )
                [ cubicBezierArg
                , cubicBezierArg0
                , cubicBezierArg1
                , cubicBezierArg2
                ]
    , background =
        \backgroundArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "background"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundArg ]
    , background2 =
        \background2Arg background2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "background2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ background2Arg, background2Arg0 ]
    , background3 =
        \background3Arg background3Arg0 background3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "background3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ background3Arg, background3Arg0, background3Arg1 ]
    , backgroundColor =
        \backgroundColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundColorArg ]
    , backgroundColor2 =
        \backgroundColor2Arg backgroundColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundColor2Arg, backgroundColor2Arg0 ]
    , backgroundColor3 =
        \backgroundColor3Arg backgroundColor3Arg0 backgroundColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundColor3Arg
                , backgroundColor3Arg0
                , backgroundColor3Arg1
                ]
    , backgroundPosition =
        \backgroundPositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundPosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundPositionArg ]
    , backgroundPosition2 =
        \backgroundPosition2Arg backgroundPosition2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundPosition2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundPosition2Arg, backgroundPosition2Arg0 ]
    , backgroundPosition3 =
        \backgroundPosition3Arg backgroundPosition3Arg0 backgroundPosition3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundPosition3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundPosition3Arg
                , backgroundPosition3Arg0
                , backgroundPosition3Arg1
                ]
    , backgroundSize =
        \backgroundSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundSizeArg ]
    , backgroundSize2 =
        \backgroundSize2Arg backgroundSize2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundSize2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundSize2Arg, backgroundSize2Arg0 ]
    , backgroundSize3 =
        \backgroundSize3Arg backgroundSize3Arg0 backgroundSize3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "backgroundSize3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ backgroundSize3Arg, backgroundSize3Arg0, backgroundSize3Arg1 ]
    , border =
        \borderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "border"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderArg ]
    , border2 =
        \border2Arg border2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "border2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ border2Arg, border2Arg0 ]
    , border3 =
        \border3Arg border3Arg0 border3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "border3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ border3Arg, border3Arg0, border3Arg1 ]
    , borderBottom =
        \borderBottomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomArg ]
    , borderBottom2 =
        \borderBottom2Arg borderBottom2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottom2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottom2Arg, borderBottom2Arg0 ]
    , borderBottom3 =
        \borderBottom3Arg borderBottom3Arg0 borderBottom3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottom3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottom3Arg, borderBottom3Arg0, borderBottom3Arg1 ]
    , borderBottomColor =
        \borderBottomColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomColorArg ]
    , borderBottomColor2 =
        \borderBottomColor2Arg borderBottomColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomColor2Arg, borderBottomColor2Arg0 ]
    , borderBottomColor3 =
        \borderBottomColor3Arg borderBottomColor3Arg0 borderBottomColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomColor3Arg
                , borderBottomColor3Arg0
                , borderBottomColor3Arg1
                ]
    , borderBottomLeftRadius =
        \borderBottomLeftRadiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomLeftRadius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomLeftRadiusArg ]
    , borderBottomLeftRadius2 =
        \borderBottomLeftRadius2Arg borderBottomLeftRadius2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomLeftRadius2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomLeftRadius2Arg, borderBottomLeftRadius2Arg0 ]
    , borderBottomLeftRadius3 =
        \borderBottomLeftRadius3Arg borderBottomLeftRadius3Arg0 borderBottomLeftRadius3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomLeftRadius3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomLeftRadius3Arg
                , borderBottomLeftRadius3Arg0
                , borderBottomLeftRadius3Arg1
                ]
    , borderBottomRightRadius =
        \borderBottomRightRadiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomRightRadius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomRightRadiusArg ]
    , borderBottomRightRadius2 =
        \borderBottomRightRadius2Arg borderBottomRightRadius2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomRightRadius2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomRightRadius2Arg, borderBottomRightRadius2Arg0 ]
    , borderBottomRightRadius3 =
        \borderBottomRightRadius3Arg borderBottomRightRadius3Arg0 borderBottomRightRadius3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomRightRadius3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomRightRadius3Arg
                , borderBottomRightRadius3Arg0
                , borderBottomRightRadius3Arg1
                ]
    , borderBottomWidth =
        \borderBottomWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomWidthArg ]
    , borderBottomWidth2 =
        \borderBottomWidth2Arg borderBottomWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomWidth2Arg, borderBottomWidth2Arg0 ]
    , borderBottomWidth3 =
        \borderBottomWidth3Arg borderBottomWidth3Arg0 borderBottomWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderBottomWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderBottomWidth3Arg
                , borderBottomWidth3Arg0
                , borderBottomWidth3Arg1
                ]
    , borderColor =
        \borderColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderColorArg ]
    , borderColor2 =
        \borderColor2Arg borderColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderColor2Arg, borderColor2Arg0 ]
    , borderColor3 =
        \borderColor3Arg borderColor3Arg0 borderColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderColor3Arg, borderColor3Arg0, borderColor3Arg1 ]
    , borderLeft =
        \borderLeftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftArg ]
    , borderLeft2 =
        \borderLeft2Arg borderLeft2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeft2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeft2Arg, borderLeft2Arg0 ]
    , borderLeft3 =
        \borderLeft3Arg borderLeft3Arg0 borderLeft3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeft3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeft3Arg, borderLeft3Arg0, borderLeft3Arg1 ]
    , borderLeftColor =
        \borderLeftColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftColorArg ]
    , borderLeftColor2 =
        \borderLeftColor2Arg borderLeftColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftColor2Arg, borderLeftColor2Arg0 ]
    , borderLeftColor3 =
        \borderLeftColor3Arg borderLeftColor3Arg0 borderLeftColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftColor3Arg
                , borderLeftColor3Arg0
                , borderLeftColor3Arg1
                ]
    , borderLeftWidth =
        \borderLeftWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftWidthArg ]
    , borderLeftWidth2 =
        \borderLeftWidth2Arg borderLeftWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftWidth2Arg, borderLeftWidth2Arg0 ]
    , borderLeftWidth3 =
        \borderLeftWidth3Arg borderLeftWidth3Arg0 borderLeftWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderLeftWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderLeftWidth3Arg
                , borderLeftWidth3Arg0
                , borderLeftWidth3Arg1
                ]
    , borderRadius =
        \borderRadiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRadius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRadiusArg ]
    , borderRadius2 =
        \borderRadius2Arg borderRadius2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRadius2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRadius2Arg, borderRadius2Arg0 ]
    , borderRadius3 =
        \borderRadius3Arg borderRadius3Arg0 borderRadius3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRadius3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRadius3Arg, borderRadius3Arg0, borderRadius3Arg1 ]
    , borderRight =
        \borderRightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightArg ]
    , borderRight2 =
        \borderRight2Arg borderRight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRight2Arg, borderRight2Arg0 ]
    , borderRight3 =
        \borderRight3Arg borderRight3Arg0 borderRight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRight3Arg, borderRight3Arg0, borderRight3Arg1 ]
    , borderRightColor =
        \borderRightColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightColorArg ]
    , borderRightColor2 =
        \borderRightColor2Arg borderRightColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightColor2Arg, borderRightColor2Arg0 ]
    , borderRightColor3 =
        \borderRightColor3Arg borderRightColor3Arg0 borderRightColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightColor3Arg
                , borderRightColor3Arg0
                , borderRightColor3Arg1
                ]
    , borderRightWidth =
        \borderRightWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightWidthArg ]
    , borderRightWidth2 =
        \borderRightWidth2Arg borderRightWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightWidth2Arg, borderRightWidth2Arg0 ]
    , borderRightWidth3 =
        \borderRightWidth3Arg borderRightWidth3Arg0 borderRightWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderRightWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderRightWidth3Arg
                , borderRightWidth3Arg0
                , borderRightWidth3Arg1
                ]
    , borderTop =
        \borderTopArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTop"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopArg ]
    , borderTop2 =
        \borderTop2Arg borderTop2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTop2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTop2Arg, borderTop2Arg0 ]
    , borderTop3 =
        \borderTop3Arg borderTop3Arg0 borderTop3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTop3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTop3Arg, borderTop3Arg0, borderTop3Arg1 ]
    , borderTopColor =
        \borderTopColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopColorArg ]
    , borderTopColor2 =
        \borderTopColor2Arg borderTopColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopColor2Arg, borderTopColor2Arg0 ]
    , borderTopColor3 =
        \borderTopColor3Arg borderTopColor3Arg0 borderTopColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopColor3Arg, borderTopColor3Arg0, borderTopColor3Arg1 ]
    , borderTopLeftRadius =
        \borderTopLeftRadiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopLeftRadius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopLeftRadiusArg ]
    , borderTopLeftRadius2 =
        \borderTopLeftRadius2Arg borderTopLeftRadius2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopLeftRadius2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopLeftRadius2Arg, borderTopLeftRadius2Arg0 ]
    , borderTopLeftRadius3 =
        \borderTopLeftRadius3Arg borderTopLeftRadius3Arg0 borderTopLeftRadius3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopLeftRadius3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopLeftRadius3Arg
                , borderTopLeftRadius3Arg0
                , borderTopLeftRadius3Arg1
                ]
    , borderTopRightRadius =
        \borderTopRightRadiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopRightRadius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopRightRadiusArg ]
    , borderTopRightRadius2 =
        \borderTopRightRadius2Arg borderTopRightRadius2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopRightRadius2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopRightRadius2Arg, borderTopRightRadius2Arg0 ]
    , borderTopRightRadius3 =
        \borderTopRightRadius3Arg borderTopRightRadius3Arg0 borderTopRightRadius3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopRightRadius3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopRightRadius3Arg
                , borderTopRightRadius3Arg0
                , borderTopRightRadius3Arg1
                ]
    , borderTopWidth =
        \borderTopWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopWidthArg ]
    , borderTopWidth2 =
        \borderTopWidth2Arg borderTopWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopWidth2Arg, borderTopWidth2Arg0 ]
    , borderTopWidth3 =
        \borderTopWidth3Arg borderTopWidth3Arg0 borderTopWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderTopWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderTopWidth3Arg, borderTopWidth3Arg0, borderTopWidth3Arg1 ]
    , borderWidth =
        \borderWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderWidthArg ]
    , borderWidth2 =
        \borderWidth2Arg borderWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderWidth2Arg, borderWidth2Arg0 ]
    , borderWidth3 =
        \borderWidth3Arg borderWidth3Arg0 borderWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "borderWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ borderWidth3Arg, borderWidth3Arg0, borderWidth3Arg1 ]
    , bottom =
        \bottomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "bottom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ bottomArg ]
    , bottom2 =
        \bottom2Arg bottom2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "bottom2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ bottom2Arg, bottom2Arg0 ]
    , bottom3 =
        \bottom3Arg bottom3Arg0 bottom3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "bottom3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ bottom3Arg, bottom3Arg0, bottom3Arg1 ]
    , boxShadow =
        \boxShadowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "boxShadow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ boxShadowArg ]
    , boxShadow2 =
        \boxShadow2Arg boxShadow2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "boxShadow2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ boxShadow2Arg, boxShadow2Arg0 ]
    , boxShadow3 =
        \boxShadow3Arg boxShadow3Arg0 boxShadow3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "boxShadow3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ boxShadow3Arg, boxShadow3Arg0, boxShadow3Arg1 ]
    , caretColor =
        \caretColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "caretColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ caretColorArg ]
    , caretColor2 =
        \caretColor2Arg caretColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "caretColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ caretColor2Arg, caretColor2Arg0 ]
    , caretColor3 =
        \caretColor3Arg caretColor3Arg0 caretColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "caretColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ caretColor3Arg, caretColor3Arg0, caretColor3Arg1 ]
    , clip =
        \clipArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clip"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clipArg ]
    , clip2 =
        \clip2Arg clip2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clip2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clip2Arg, clip2Arg0 ]
    , clip3 =
        \clip3Arg clip3Arg0 clip3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clip3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clip3Arg, clip3Arg0, clip3Arg1 ]
    , clipPath =
        \clipPathArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clipPath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clipPathArg ]
    , clipPath2 =
        \clipPath2Arg clipPath2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clipPath2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clipPath2Arg, clipPath2Arg0 ]
    , clipPath3 =
        \clipPath3Arg clipPath3Arg0 clipPath3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "clipPath3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ clipPath3Arg, clipPath3Arg0, clipPath3Arg1 ]
    , color =
        \colorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "color"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ colorArg ]
    , color2 =
        \color2Arg color2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "color2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ color2Arg, color2Arg0 ]
    , color3 =
        \color3Arg color3Arg0 color3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "color3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ color3Arg, color3Arg0, color3Arg1 ]
    , columnCount =
        \columnCountArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnCount"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnCountArg ]
    , columnCount2 =
        \columnCount2Arg columnCount2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnCount2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnCount2Arg, columnCount2Arg0 ]
    , columnCount3 =
        \columnCount3Arg columnCount3Arg0 columnCount3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnCount3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnCount3Arg, columnCount3Arg0, columnCount3Arg1 ]
    , columnGap =
        \columnGapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnGap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnGapArg ]
    , columnGap2 =
        \columnGap2Arg columnGap2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnGap2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnGap2Arg, columnGap2Arg0 ]
    , columnGap3 =
        \columnGap3Arg columnGap3Arg0 columnGap3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnGap3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnGap3Arg, columnGap3Arg0, columnGap3Arg1 ]
    , columnRule =
        \columnRuleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRule"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleArg ]
    , columnRule2 =
        \columnRule2Arg columnRule2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRule2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRule2Arg, columnRule2Arg0 ]
    , columnRule3 =
        \columnRule3Arg columnRule3Arg0 columnRule3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRule3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRule3Arg, columnRule3Arg0, columnRule3Arg1 ]
    , columnRuleColor =
        \columnRuleColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleColorArg ]
    , columnRuleColor2 =
        \columnRuleColor2Arg columnRuleColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleColor2Arg, columnRuleColor2Arg0 ]
    , columnRuleColor3 =
        \columnRuleColor3Arg columnRuleColor3Arg0 columnRuleColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleColor3Arg
                , columnRuleColor3Arg0
                , columnRuleColor3Arg1
                ]
    , columnRuleWidth =
        \columnRuleWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleWidthArg ]
    , columnRuleWidth2 =
        \columnRuleWidth2Arg columnRuleWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleWidth2Arg, columnRuleWidth2Arg0 ]
    , columnRuleWidth3 =
        \columnRuleWidth3Arg columnRuleWidth3Arg0 columnRuleWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnRuleWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnRuleWidth3Arg
                , columnRuleWidth3Arg0
                , columnRuleWidth3Arg1
                ]
    , columnWidth =
        \columnWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnWidthArg ]
    , columnWidth2 =
        \columnWidth2Arg columnWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnWidth2Arg, columnWidth2Arg0 ]
    , columnWidth3 =
        \columnWidth3Arg columnWidth3Arg0 columnWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columnWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnWidth3Arg, columnWidth3Arg0, columnWidth3Arg1 ]
    , columns =
        \columnsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columns"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columnsArg ]
    , columns2 =
        \columns2Arg columns2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columns2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columns2Arg, columns2Arg0 ]
    , columns3 =
        \columns3Arg columns3Arg0 columns3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "columns3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ columns3Arg, columns3Arg0, columns3Arg1 ]
    , filter =
        \filterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "filter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ filterArg ]
    , filter2 =
        \filter2Arg filter2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "filter2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ filter2Arg, filter2Arg0 ]
    , filter3 =
        \filter3Arg filter3Arg0 filter3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "filter3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ filter3Arg, filter3Arg0, filter3Arg1 ]
    , flex =
        \flexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexArg ]
    , flex2 =
        \flex2Arg flex2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flex2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flex2Arg, flex2Arg0 ]
    , flex3 =
        \flex3Arg flex3Arg0 flex3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flex3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flex3Arg, flex3Arg0, flex3Arg1 ]
    , flexBasis =
        \flexBasisArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexBasis"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexBasisArg ]
    , flexBasis2 =
        \flexBasis2Arg flexBasis2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexBasis2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexBasis2Arg, flexBasis2Arg0 ]
    , flexBasis3 =
        \flexBasis3Arg flexBasis3Arg0 flexBasis3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexBasis3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexBasis3Arg, flexBasis3Arg0, flexBasis3Arg1 ]
    , flexGrow =
        \flexGrowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexGrow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexGrowArg ]
    , flexGrow2 =
        \flexGrow2Arg flexGrow2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexGrow2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexGrow2Arg, flexGrow2Arg0 ]
    , flexGrow3 =
        \flexGrow3Arg flexGrow3Arg0 flexGrow3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexGrow3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexGrow3Arg, flexGrow3Arg0, flexGrow3Arg1 ]
    , flexShrink =
        \flexShrinkArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexShrink"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexShrinkArg ]
    , flexShrink2 =
        \flexShrink2Arg flexShrink2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexShrink2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexShrink2Arg, flexShrink2Arg0 ]
    , flexShrink3 =
        \flexShrink3Arg flexShrink3Arg0 flexShrink3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "flexShrink3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ flexShrink3Arg, flexShrink3Arg0, flexShrink3Arg1 ]
    , font =
        \fontArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "font"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontArg ]
    , font2 =
        \font2Arg font2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "font2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ font2Arg, font2Arg0 ]
    , font3 =
        \font3Arg font3Arg0 font3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "font3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ font3Arg, font3Arg0, font3Arg1 ]
    , fontSize =
        \fontSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSizeArg ]
    , fontSize2 =
        \fontSize2Arg fontSize2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSize2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSize2Arg, fontSize2Arg0 ]
    , fontSize3 =
        \fontSize3Arg fontSize3Arg0 fontSize3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSize3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSize3Arg, fontSize3Arg0, fontSize3Arg1 ]
    , fontSizeAdjust =
        \fontSizeAdjustArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSizeAdjust"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSizeAdjustArg ]
    , fontSizeAdjust2 =
        \fontSizeAdjust2Arg fontSizeAdjust2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSizeAdjust2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSizeAdjust2Arg, fontSizeAdjust2Arg0 ]
    , fontSizeAdjust3 =
        \fontSizeAdjust3Arg fontSizeAdjust3Arg0 fontSizeAdjust3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontSizeAdjust3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontSizeAdjust3Arg, fontSizeAdjust3Arg0, fontSizeAdjust3Arg1 ]
    , fontStretch =
        \fontStretchArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontStretch"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontStretchArg ]
    , fontStretch2 =
        \fontStretch2Arg fontStretch2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontStretch2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontStretch2Arg, fontStretch2Arg0 ]
    , fontStretch3 =
        \fontStretch3Arg fontStretch3Arg0 fontStretch3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontStretch3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontStretch3Arg, fontStretch3Arg0, fontStretch3Arg1 ]
    , fontVariationSettings =
        \fontVariationSettingsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontVariationSettings"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontVariationSettingsArg ]
    , fontVariationSettings2 =
        \fontVariationSettings2Arg fontVariationSettings2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontVariationSettings2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontVariationSettings2Arg, fontVariationSettings2Arg0 ]
    , fontVariationSettings3 =
        \fontVariationSettings3Arg fontVariationSettings3Arg0 fontVariationSettings3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontVariationSettings3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontVariationSettings3Arg
                , fontVariationSettings3Arg0
                , fontVariationSettings3Arg1
                ]
    , fontWeight =
        \fontWeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontWeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontWeightArg ]
    , fontWeight2 =
        \fontWeight2Arg fontWeight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontWeight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontWeight2Arg, fontWeight2Arg0 ]
    , fontWeight3 =
        \fontWeight3Arg fontWeight3Arg0 fontWeight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "fontWeight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ fontWeight3Arg, fontWeight3Arg0, fontWeight3Arg1 ]
    , gridColumnGap =
        \gridColumnGapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridColumnGap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridColumnGapArg ]
    , gridColumnGap2 =
        \gridColumnGap2Arg gridColumnGap2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridColumnGap2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridColumnGap2Arg, gridColumnGap2Arg0 ]
    , gridColumnGap3 =
        \gridColumnGap3Arg gridColumnGap3Arg0 gridColumnGap3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridColumnGap3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridColumnGap3Arg, gridColumnGap3Arg0, gridColumnGap3Arg1 ]
    , gridGap =
        \gridGapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridGap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridGapArg ]
    , gridGap2 =
        \gridGap2Arg gridGap2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridGap2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridGap2Arg, gridGap2Arg0 ]
    , gridGap3 =
        \gridGap3Arg gridGap3Arg0 gridGap3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridGap3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridGap3Arg, gridGap3Arg0, gridGap3Arg1 ]
    , gridRowGap =
        \gridRowGapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridRowGap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridRowGapArg ]
    , gridRowGap2 =
        \gridRowGap2Arg gridRowGap2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridRowGap2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridRowGap2Arg, gridRowGap2Arg0 ]
    , gridRowGap3 =
        \gridRowGap3Arg gridRowGap3Arg0 gridRowGap3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "gridRowGap3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ gridRowGap3Arg, gridRowGap3Arg0, gridRowGap3Arg1 ]
    , height =
        \heightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "height"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ heightArg ]
    , height2 =
        \height2Arg height2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "height2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ height2Arg, height2Arg0 ]
    , height3 =
        \height3Arg height3Arg0 height3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "height3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ height3Arg, height3Arg0, height3Arg1 ]
    , left =
        \leftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "left"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ leftArg ]
    , left2 =
        \left2Arg left2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "left2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ left2Arg, left2Arg0 ]
    , left3 =
        \left3Arg left3Arg0 left3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "left3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ left3Arg, left3Arg0, left3Arg1 ]
    , letterSpacing =
        \letterSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "letterSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ letterSpacingArg ]
    , letterSpacing2 =
        \letterSpacing2Arg letterSpacing2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "letterSpacing2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ letterSpacing2Arg, letterSpacing2Arg0 ]
    , letterSpacing3 =
        \letterSpacing3Arg letterSpacing3Arg0 letterSpacing3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "letterSpacing3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ letterSpacing3Arg, letterSpacing3Arg0, letterSpacing3Arg1 ]
    , lineHeight =
        \lineHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "lineHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ lineHeightArg ]
    , lineHeight2 =
        \lineHeight2Arg lineHeight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "lineHeight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ lineHeight2Arg, lineHeight2Arg0 ]
    , lineHeight3 =
        \lineHeight3Arg lineHeight3Arg0 lineHeight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "lineHeight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ lineHeight3Arg, lineHeight3Arg0, lineHeight3Arg1 ]
    , margin =
        \marginArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "margin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginArg ]
    , margin2 =
        \margin2Arg margin2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "margin2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ margin2Arg, margin2Arg0 ]
    , margin3 =
        \margin3Arg margin3Arg0 margin3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "margin3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ margin3Arg, margin3Arg0, margin3Arg1 ]
    , marginBottom =
        \marginBottomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginBottom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginBottomArg ]
    , marginBottom2 =
        \marginBottom2Arg marginBottom2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginBottom2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginBottom2Arg, marginBottom2Arg0 ]
    , marginBottom3 =
        \marginBottom3Arg marginBottom3Arg0 marginBottom3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginBottom3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginBottom3Arg, marginBottom3Arg0, marginBottom3Arg1 ]
    , marginLeft =
        \marginLeftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginLeftArg ]
    , marginLeft2 =
        \marginLeft2Arg marginLeft2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginLeft2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginLeft2Arg, marginLeft2Arg0 ]
    , marginLeft3 =
        \marginLeft3Arg marginLeft3Arg0 marginLeft3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginLeft3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginLeft3Arg, marginLeft3Arg0, marginLeft3Arg1 ]
    , marginRight =
        \marginRightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginRightArg ]
    , marginRight2 =
        \marginRight2Arg marginRight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginRight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginRight2Arg, marginRight2Arg0 ]
    , marginRight3 =
        \marginRight3Arg marginRight3Arg0 marginRight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginRight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginRight3Arg, marginRight3Arg0, marginRight3Arg1 ]
    , marginTop =
        \marginTopArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginTop"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginTopArg ]
    , marginTop2 =
        \marginTop2Arg marginTop2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginTop2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginTop2Arg, marginTop2Arg0 ]
    , marginTop3 =
        \marginTop3Arg marginTop3Arg0 marginTop3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "marginTop3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ marginTop3Arg, marginTop3Arg0, marginTop3Arg1 ]
    , mask =
        \maskArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "mask"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskArg ]
    , mask2 =
        \mask2Arg mask2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "mask2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ mask2Arg, mask2Arg0 ]
    , mask3 =
        \mask3Arg mask3Arg0 mask3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "mask3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ mask3Arg, mask3Arg0, mask3Arg1 ]
    , maskPosition =
        \maskPositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskPosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskPositionArg ]
    , maskPosition2 =
        \maskPosition2Arg maskPosition2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskPosition2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskPosition2Arg, maskPosition2Arg0 ]
    , maskPosition3 =
        \maskPosition3Arg maskPosition3Arg0 maskPosition3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskPosition3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskPosition3Arg, maskPosition3Arg0, maskPosition3Arg1 ]
    , maskSize =
        \maskSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskSizeArg ]
    , maskSize2 =
        \maskSize2Arg maskSize2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskSize2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskSize2Arg, maskSize2Arg0 ]
    , maskSize3 =
        \maskSize3Arg maskSize3Arg0 maskSize3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maskSize3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maskSize3Arg, maskSize3Arg0, maskSize3Arg1 ]
    , maxHeight =
        \maxHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxHeightArg ]
    , maxHeight2 =
        \maxHeight2Arg maxHeight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxHeight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxHeight2Arg, maxHeight2Arg0 ]
    , maxHeight3 =
        \maxHeight3Arg maxHeight3Arg0 maxHeight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxHeight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxHeight3Arg, maxHeight3Arg0, maxHeight3Arg1 ]
    , maxWidth =
        \maxWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxWidthArg ]
    , maxWidth2 =
        \maxWidth2Arg maxWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxWidth2Arg, maxWidth2Arg0 ]
    , maxWidth3 =
        \maxWidth3Arg maxWidth3Arg0 maxWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "maxWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ maxWidth3Arg, maxWidth3Arg0, maxWidth3Arg1 ]
    , minHeight =
        \minHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minHeightArg ]
    , minHeight2 =
        \minHeight2Arg minHeight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minHeight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minHeight2Arg, minHeight2Arg0 ]
    , minHeight3 =
        \minHeight3Arg minHeight3Arg0 minHeight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minHeight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minHeight3Arg, minHeight3Arg0, minHeight3Arg1 ]
    , minWidth =
        \minWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minWidthArg ]
    , minWidth2 =
        \minWidth2Arg minWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minWidth2Arg, minWidth2Arg0 ]
    , minWidth3 =
        \minWidth3Arg minWidth3Arg0 minWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "minWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ minWidth3Arg, minWidth3Arg0, minWidth3Arg1 ]
    , objectPosition =
        \objectPositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "objectPosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ objectPositionArg ]
    , objectPosition2 =
        \objectPosition2Arg objectPosition2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "objectPosition2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ objectPosition2Arg, objectPosition2Arg0 ]
    , objectPosition3 =
        \objectPosition3Arg objectPosition3Arg0 objectPosition3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "objectPosition3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ objectPosition3Arg, objectPosition3Arg0, objectPosition3Arg1 ]
    , offset =
        \offsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetArg ]
    , offset2 =
        \offset2Arg offset2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offset2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offset2Arg, offset2Arg0 ]
    , offset3 =
        \offset3Arg offset3Arg0 offset3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offset3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offset3Arg, offset3Arg0, offset3Arg1 ]
    , offsetAnchor =
        \offsetAnchorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetAnchor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetAnchorArg ]
    , offsetAnchor2 =
        \offsetAnchor2Arg offsetAnchor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetAnchor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetAnchor2Arg, offsetAnchor2Arg0 ]
    , offsetAnchor3 =
        \offsetAnchor3Arg offsetAnchor3Arg0 offsetAnchor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetAnchor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetAnchor3Arg, offsetAnchor3Arg0, offsetAnchor3Arg1 ]
    , offsetDistance =
        \offsetDistanceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetDistance"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetDistanceArg ]
    , offsetDistance2 =
        \offsetDistance2Arg offsetDistance2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetDistance2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetDistance2Arg, offsetDistance2Arg0 ]
    , offsetDistance3 =
        \offsetDistance3Arg offsetDistance3Arg0 offsetDistance3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetDistance3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetDistance3Arg, offsetDistance3Arg0, offsetDistance3Arg1 ]
    , offsetPath =
        \offsetPathArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetPath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetPathArg ]
    , offsetPath2 =
        \offsetPath2Arg offsetPath2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetPath2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetPath2Arg, offsetPath2Arg0 ]
    , offsetPath3 =
        \offsetPath3Arg offsetPath3Arg0 offsetPath3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetPath3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetPath3Arg, offsetPath3Arg0, offsetPath3Arg1 ]
    , offsetRotate =
        \offsetRotateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetRotate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetRotateArg ]
    , offsetRotate2 =
        \offsetRotate2Arg offsetRotate2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetRotate2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetRotate2Arg, offsetRotate2Arg0 ]
    , offsetRotate3 =
        \offsetRotate3Arg offsetRotate3Arg0 offsetRotate3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "offsetRotate3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ offsetRotate3Arg, offsetRotate3Arg0, offsetRotate3Arg1 ]
    , opacity =
        \opacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "opacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ opacityArg ]
    , opacity2 =
        \opacity2Arg opacity2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "opacity2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ opacity2Arg, opacity2Arg0 ]
    , opacity3 =
        \opacity3Arg opacity3Arg0 opacity3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "opacity3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ opacity3Arg, opacity3Arg0, opacity3Arg1 ]
    , order =
        \orderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "order"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ orderArg ]
    , order2 =
        \order2Arg order2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "order2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ order2Arg, order2Arg0 ]
    , order3 =
        \order3Arg order3Arg0 order3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "order3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ order3Arg, order3Arg0, order3Arg1 ]
    , outline =
        \outlineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineArg ]
    , outline2 =
        \outline2Arg outline2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outline2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outline2Arg, outline2Arg0 ]
    , outline3 =
        \outline3Arg outline3Arg0 outline3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outline3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outline3Arg, outline3Arg0, outline3Arg1 ]
    , outlineColor =
        \outlineColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineColorArg ]
    , outlineColor2 =
        \outlineColor2Arg outlineColor2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineColor2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineColor2Arg, outlineColor2Arg0 ]
    , outlineColor3 =
        \outlineColor3Arg outlineColor3Arg0 outlineColor3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineColor3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineColor3Arg, outlineColor3Arg0, outlineColor3Arg1 ]
    , outlineOffset =
        \outlineOffsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineOffset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineOffsetArg ]
    , outlineOffset2 =
        \outlineOffset2Arg outlineOffset2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineOffset2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineOffset2Arg, outlineOffset2Arg0 ]
    , outlineOffset3 =
        \outlineOffset3Arg outlineOffset3Arg0 outlineOffset3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineOffset3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineOffset3Arg, outlineOffset3Arg0, outlineOffset3Arg1 ]
    , outlineWidth =
        \outlineWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineWidthArg ]
    , outlineWidth2 =
        \outlineWidth2Arg outlineWidth2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineWidth2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineWidth2Arg, outlineWidth2Arg0 ]
    , outlineWidth3 =
        \outlineWidth3Arg outlineWidth3Arg0 outlineWidth3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "outlineWidth3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ outlineWidth3Arg, outlineWidth3Arg0, outlineWidth3Arg1 ]
    , padding =
        \paddingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "padding"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingArg ]
    , padding2 =
        \padding2Arg padding2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "padding2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ padding2Arg, padding2Arg0 ]
    , padding3 =
        \padding3Arg padding3Arg0 padding3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "padding3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ padding3Arg, padding3Arg0, padding3Arg1 ]
    , paddingBottom =
        \paddingBottomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingBottom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingBottomArg ]
    , paddingBottom2 =
        \paddingBottom2Arg paddingBottom2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingBottom2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingBottom2Arg, paddingBottom2Arg0 ]
    , paddingBottom3 =
        \paddingBottom3Arg paddingBottom3Arg0 paddingBottom3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingBottom3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingBottom3Arg, paddingBottom3Arg0, paddingBottom3Arg1 ]
    , paddingLeft =
        \paddingLeftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingLeftArg ]
    , paddingLeft2 =
        \paddingLeft2Arg paddingLeft2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingLeft2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingLeft2Arg, paddingLeft2Arg0 ]
    , paddingLeft3 =
        \paddingLeft3Arg paddingLeft3Arg0 paddingLeft3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingLeft3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingLeft3Arg, paddingLeft3Arg0, paddingLeft3Arg1 ]
    , paddingRight =
        \paddingRightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingRightArg ]
    , paddingRight2 =
        \paddingRight2Arg paddingRight2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingRight2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingRight2Arg, paddingRight2Arg0 ]
    , paddingRight3 =
        \paddingRight3Arg paddingRight3Arg0 paddingRight3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingRight3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingRight3Arg, paddingRight3Arg0, paddingRight3Arg1 ]
    , paddingTop =
        \paddingTopArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingTop"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingTopArg ]
    , paddingTop2 =
        \paddingTop2Arg paddingTop2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingTop2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingTop2Arg, paddingTop2Arg0 ]
    , paddingTop3 =
        \paddingTop3Arg paddingTop3Arg0 paddingTop3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "paddingTop3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ paddingTop3Arg, paddingTop3Arg0, paddingTop3Arg1 ]
    , right =
        \rightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "right"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ rightArg ]
    , right2 =
        \right2Arg right2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "right2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ right2Arg, right2Arg0 ]
    , right3 =
        \right3Arg right3Arg0 right3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "right3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ right3Arg, right3Arg0, right3Arg1 ]
    , tabSize =
        \tabSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "tabSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ tabSizeArg ]
    , tabSize2 =
        \tabSize2Arg tabSize2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "tabSize2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ tabSize2Arg, tabSize2Arg0 ]
    , tabSize3 =
        \tabSize3Arg tabSize3Arg0 tabSize3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "tabSize3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ tabSize3Arg, tabSize3Arg0, tabSize3Arg1 ]
    , textIndent =
        \textIndentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textIndent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textIndentArg ]
    , textIndent2 =
        \textIndent2Arg textIndent2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textIndent2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textIndent2Arg, textIndent2Arg0 ]
    , textIndent3 =
        \textIndent3Arg textIndent3Arg0 textIndent3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textIndent3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textIndent3Arg, textIndent3Arg0, textIndent3Arg1 ]
    , textShadow =
        \textShadowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textShadow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textShadowArg ]
    , textShadow2 =
        \textShadow2Arg textShadow2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textShadow2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textShadow2Arg, textShadow2Arg0 ]
    , textShadow3 =
        \textShadow3Arg textShadow3Arg0 textShadow3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "textShadow3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ textShadow3Arg, textShadow3Arg0, textShadow3Arg1 ]
    , top =
        \topArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "top"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ topArg ]
    , top2 =
        \top2Arg top2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "top2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ top2Arg, top2Arg0 ]
    , top3 =
        \top3Arg top3Arg0 top3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "top3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ top3Arg, top3Arg0, top3Arg1 ]
    , transform =
        \transformArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transformArg ]
    , transform2 =
        \transform2Arg transform2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transform2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transform2Arg, transform2Arg0 ]
    , transform3 =
        \transform3Arg transform3Arg0 transform3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transform3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transform3Arg, transform3Arg0, transform3Arg1 ]
    , transformOrigin =
        \transformOriginArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transformOrigin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transformOriginArg ]
    , transformOrigin2 =
        \transformOrigin2Arg transformOrigin2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transformOrigin2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transformOrigin2Arg, transformOrigin2Arg0 ]
    , transformOrigin3 =
        \transformOrigin3Arg transformOrigin3Arg0 transformOrigin3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "transformOrigin3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ transformOrigin3Arg
                , transformOrigin3Arg0
                , transformOrigin3Arg1
                ]
    , verticalAlign =
        \verticalAlignArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "verticalAlign"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ verticalAlignArg ]
    , verticalAlign2 =
        \verticalAlign2Arg verticalAlign2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "verticalAlign2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ verticalAlign2Arg, verticalAlign2Arg0 ]
    , verticalAlign3 =
        \verticalAlign3Arg verticalAlign3Arg0 verticalAlign3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "verticalAlign3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ verticalAlign3Arg, verticalAlign3Arg0, verticalAlign3Arg1 ]
    , visibility =
        \visibilityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "visibility"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ visibilityArg ]
    , visibility2 =
        \visibility2Arg visibility2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "visibility2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ visibility2Arg, visibility2Arg0 ]
    , visibility3 =
        \visibility3Arg visibility3Arg0 visibility3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "visibility3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ visibility3Arg, visibility3Arg0, visibility3Arg1 ]
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "width"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ widthArg ]
    , width2 =
        \width2Arg width2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "width2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ width2Arg, width2Arg0 ]
    , width3 =
        \width3Arg width3Arg0 width3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "width3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ width3Arg, width3Arg0, width3Arg1 ]
    , wordSpacing =
        \wordSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "wordSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ wordSpacingArg ]
    , wordSpacing2 =
        \wordSpacing2Arg wordSpacing2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "wordSpacing2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ wordSpacing2Arg, wordSpacing2Arg0 ]
    , wordSpacing3 =
        \wordSpacing3Arg wordSpacing3Arg0 wordSpacing3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "wordSpacing3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ wordSpacing3Arg, wordSpacing3Arg0, wordSpacing3Arg1 ]
    , zIndex =
        \zIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "zIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ zIndexArg ]
    , zIndex2 =
        \zIndex2Arg zIndex2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "zIndex2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ zIndex2Arg, zIndex2Arg0 ]
    , zIndex3 =
        \zIndex3Arg zIndex3Arg0 zIndex3Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Transitions" ]
                    , name = "zIndex3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "TimingFunction"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Transitions" ]
                                    "Transition"
                                    []
                                )
                            )
                    }
                )
                [ zIndex3Arg, zIndex3Arg0, zIndex3Arg1 ]
    }


values_ :
    { transition : Elm.Expression
    , ease : Elm.Expression
    , linear : Elm.Expression
    , easeIn : Elm.Expression
    , easeOut : Elm.Expression
    , easeInOut : Elm.Expression
    , stepStart : Elm.Expression
    , stepEnd : Elm.Expression
    , cubicBezier : Elm.Expression
    , background : Elm.Expression
    , background2 : Elm.Expression
    , background3 : Elm.Expression
    , backgroundColor : Elm.Expression
    , backgroundColor2 : Elm.Expression
    , backgroundColor3 : Elm.Expression
    , backgroundPosition : Elm.Expression
    , backgroundPosition2 : Elm.Expression
    , backgroundPosition3 : Elm.Expression
    , backgroundSize : Elm.Expression
    , backgroundSize2 : Elm.Expression
    , backgroundSize3 : Elm.Expression
    , border : Elm.Expression
    , border2 : Elm.Expression
    , border3 : Elm.Expression
    , borderBottom : Elm.Expression
    , borderBottom2 : Elm.Expression
    , borderBottom3 : Elm.Expression
    , borderBottomColor : Elm.Expression
    , borderBottomColor2 : Elm.Expression
    , borderBottomColor3 : Elm.Expression
    , borderBottomLeftRadius : Elm.Expression
    , borderBottomLeftRadius2 : Elm.Expression
    , borderBottomLeftRadius3 : Elm.Expression
    , borderBottomRightRadius : Elm.Expression
    , borderBottomRightRadius2 : Elm.Expression
    , borderBottomRightRadius3 : Elm.Expression
    , borderBottomWidth : Elm.Expression
    , borderBottomWidth2 : Elm.Expression
    , borderBottomWidth3 : Elm.Expression
    , borderColor : Elm.Expression
    , borderColor2 : Elm.Expression
    , borderColor3 : Elm.Expression
    , borderLeft : Elm.Expression
    , borderLeft2 : Elm.Expression
    , borderLeft3 : Elm.Expression
    , borderLeftColor : Elm.Expression
    , borderLeftColor2 : Elm.Expression
    , borderLeftColor3 : Elm.Expression
    , borderLeftWidth : Elm.Expression
    , borderLeftWidth2 : Elm.Expression
    , borderLeftWidth3 : Elm.Expression
    , borderRadius : Elm.Expression
    , borderRadius2 : Elm.Expression
    , borderRadius3 : Elm.Expression
    , borderRight : Elm.Expression
    , borderRight2 : Elm.Expression
    , borderRight3 : Elm.Expression
    , borderRightColor : Elm.Expression
    , borderRightColor2 : Elm.Expression
    , borderRightColor3 : Elm.Expression
    , borderRightWidth : Elm.Expression
    , borderRightWidth2 : Elm.Expression
    , borderRightWidth3 : Elm.Expression
    , borderTop : Elm.Expression
    , borderTop2 : Elm.Expression
    , borderTop3 : Elm.Expression
    , borderTopColor : Elm.Expression
    , borderTopColor2 : Elm.Expression
    , borderTopColor3 : Elm.Expression
    , borderTopLeftRadius : Elm.Expression
    , borderTopLeftRadius2 : Elm.Expression
    , borderTopLeftRadius3 : Elm.Expression
    , borderTopRightRadius : Elm.Expression
    , borderTopRightRadius2 : Elm.Expression
    , borderTopRightRadius3 : Elm.Expression
    , borderTopWidth : Elm.Expression
    , borderTopWidth2 : Elm.Expression
    , borderTopWidth3 : Elm.Expression
    , borderWidth : Elm.Expression
    , borderWidth2 : Elm.Expression
    , borderWidth3 : Elm.Expression
    , bottom : Elm.Expression
    , bottom2 : Elm.Expression
    , bottom3 : Elm.Expression
    , boxShadow : Elm.Expression
    , boxShadow2 : Elm.Expression
    , boxShadow3 : Elm.Expression
    , caretColor : Elm.Expression
    , caretColor2 : Elm.Expression
    , caretColor3 : Elm.Expression
    , clip : Elm.Expression
    , clip2 : Elm.Expression
    , clip3 : Elm.Expression
    , clipPath : Elm.Expression
    , clipPath2 : Elm.Expression
    , clipPath3 : Elm.Expression
    , color : Elm.Expression
    , color2 : Elm.Expression
    , color3 : Elm.Expression
    , columnCount : Elm.Expression
    , columnCount2 : Elm.Expression
    , columnCount3 : Elm.Expression
    , columnGap : Elm.Expression
    , columnGap2 : Elm.Expression
    , columnGap3 : Elm.Expression
    , columnRule : Elm.Expression
    , columnRule2 : Elm.Expression
    , columnRule3 : Elm.Expression
    , columnRuleColor : Elm.Expression
    , columnRuleColor2 : Elm.Expression
    , columnRuleColor3 : Elm.Expression
    , columnRuleWidth : Elm.Expression
    , columnRuleWidth2 : Elm.Expression
    , columnRuleWidth3 : Elm.Expression
    , columnWidth : Elm.Expression
    , columnWidth2 : Elm.Expression
    , columnWidth3 : Elm.Expression
    , columns : Elm.Expression
    , columns2 : Elm.Expression
    , columns3 : Elm.Expression
    , filter : Elm.Expression
    , filter2 : Elm.Expression
    , filter3 : Elm.Expression
    , flex : Elm.Expression
    , flex2 : Elm.Expression
    , flex3 : Elm.Expression
    , flexBasis : Elm.Expression
    , flexBasis2 : Elm.Expression
    , flexBasis3 : Elm.Expression
    , flexGrow : Elm.Expression
    , flexGrow2 : Elm.Expression
    , flexGrow3 : Elm.Expression
    , flexShrink : Elm.Expression
    , flexShrink2 : Elm.Expression
    , flexShrink3 : Elm.Expression
    , font : Elm.Expression
    , font2 : Elm.Expression
    , font3 : Elm.Expression
    , fontSize : Elm.Expression
    , fontSize2 : Elm.Expression
    , fontSize3 : Elm.Expression
    , fontSizeAdjust : Elm.Expression
    , fontSizeAdjust2 : Elm.Expression
    , fontSizeAdjust3 : Elm.Expression
    , fontStretch : Elm.Expression
    , fontStretch2 : Elm.Expression
    , fontStretch3 : Elm.Expression
    , fontVariationSettings : Elm.Expression
    , fontVariationSettings2 : Elm.Expression
    , fontVariationSettings3 : Elm.Expression
    , fontWeight : Elm.Expression
    , fontWeight2 : Elm.Expression
    , fontWeight3 : Elm.Expression
    , gridColumnGap : Elm.Expression
    , gridColumnGap2 : Elm.Expression
    , gridColumnGap3 : Elm.Expression
    , gridGap : Elm.Expression
    , gridGap2 : Elm.Expression
    , gridGap3 : Elm.Expression
    , gridRowGap : Elm.Expression
    , gridRowGap2 : Elm.Expression
    , gridRowGap3 : Elm.Expression
    , height : Elm.Expression
    , height2 : Elm.Expression
    , height3 : Elm.Expression
    , left : Elm.Expression
    , left2 : Elm.Expression
    , left3 : Elm.Expression
    , letterSpacing : Elm.Expression
    , letterSpacing2 : Elm.Expression
    , letterSpacing3 : Elm.Expression
    , lineHeight : Elm.Expression
    , lineHeight2 : Elm.Expression
    , lineHeight3 : Elm.Expression
    , margin : Elm.Expression
    , margin2 : Elm.Expression
    , margin3 : Elm.Expression
    , marginBottom : Elm.Expression
    , marginBottom2 : Elm.Expression
    , marginBottom3 : Elm.Expression
    , marginLeft : Elm.Expression
    , marginLeft2 : Elm.Expression
    , marginLeft3 : Elm.Expression
    , marginRight : Elm.Expression
    , marginRight2 : Elm.Expression
    , marginRight3 : Elm.Expression
    , marginTop : Elm.Expression
    , marginTop2 : Elm.Expression
    , marginTop3 : Elm.Expression
    , mask : Elm.Expression
    , mask2 : Elm.Expression
    , mask3 : Elm.Expression
    , maskPosition : Elm.Expression
    , maskPosition2 : Elm.Expression
    , maskPosition3 : Elm.Expression
    , maskSize : Elm.Expression
    , maskSize2 : Elm.Expression
    , maskSize3 : Elm.Expression
    , maxHeight : Elm.Expression
    , maxHeight2 : Elm.Expression
    , maxHeight3 : Elm.Expression
    , maxWidth : Elm.Expression
    , maxWidth2 : Elm.Expression
    , maxWidth3 : Elm.Expression
    , minHeight : Elm.Expression
    , minHeight2 : Elm.Expression
    , minHeight3 : Elm.Expression
    , minWidth : Elm.Expression
    , minWidth2 : Elm.Expression
    , minWidth3 : Elm.Expression
    , objectPosition : Elm.Expression
    , objectPosition2 : Elm.Expression
    , objectPosition3 : Elm.Expression
    , offset : Elm.Expression
    , offset2 : Elm.Expression
    , offset3 : Elm.Expression
    , offsetAnchor : Elm.Expression
    , offsetAnchor2 : Elm.Expression
    , offsetAnchor3 : Elm.Expression
    , offsetDistance : Elm.Expression
    , offsetDistance2 : Elm.Expression
    , offsetDistance3 : Elm.Expression
    , offsetPath : Elm.Expression
    , offsetPath2 : Elm.Expression
    , offsetPath3 : Elm.Expression
    , offsetRotate : Elm.Expression
    , offsetRotate2 : Elm.Expression
    , offsetRotate3 : Elm.Expression
    , opacity : Elm.Expression
    , opacity2 : Elm.Expression
    , opacity3 : Elm.Expression
    , order : Elm.Expression
    , order2 : Elm.Expression
    , order3 : Elm.Expression
    , outline : Elm.Expression
    , outline2 : Elm.Expression
    , outline3 : Elm.Expression
    , outlineColor : Elm.Expression
    , outlineColor2 : Elm.Expression
    , outlineColor3 : Elm.Expression
    , outlineOffset : Elm.Expression
    , outlineOffset2 : Elm.Expression
    , outlineOffset3 : Elm.Expression
    , outlineWidth : Elm.Expression
    , outlineWidth2 : Elm.Expression
    , outlineWidth3 : Elm.Expression
    , padding : Elm.Expression
    , padding2 : Elm.Expression
    , padding3 : Elm.Expression
    , paddingBottom : Elm.Expression
    , paddingBottom2 : Elm.Expression
    , paddingBottom3 : Elm.Expression
    , paddingLeft : Elm.Expression
    , paddingLeft2 : Elm.Expression
    , paddingLeft3 : Elm.Expression
    , paddingRight : Elm.Expression
    , paddingRight2 : Elm.Expression
    , paddingRight3 : Elm.Expression
    , paddingTop : Elm.Expression
    , paddingTop2 : Elm.Expression
    , paddingTop3 : Elm.Expression
    , right : Elm.Expression
    , right2 : Elm.Expression
    , right3 : Elm.Expression
    , tabSize : Elm.Expression
    , tabSize2 : Elm.Expression
    , tabSize3 : Elm.Expression
    , textIndent : Elm.Expression
    , textIndent2 : Elm.Expression
    , textIndent3 : Elm.Expression
    , textShadow : Elm.Expression
    , textShadow2 : Elm.Expression
    , textShadow3 : Elm.Expression
    , top : Elm.Expression
    , top2 : Elm.Expression
    , top3 : Elm.Expression
    , transform : Elm.Expression
    , transform2 : Elm.Expression
    , transform3 : Elm.Expression
    , transformOrigin : Elm.Expression
    , transformOrigin2 : Elm.Expression
    , transformOrigin3 : Elm.Expression
    , verticalAlign : Elm.Expression
    , verticalAlign2 : Elm.Expression
    , verticalAlign3 : Elm.Expression
    , visibility : Elm.Expression
    , visibility2 : Elm.Expression
    , visibility3 : Elm.Expression
    , width : Elm.Expression
    , width2 : Elm.Expression
    , width3 : Elm.Expression
    , wordSpacing : Elm.Expression
    , wordSpacing2 : Elm.Expression
    , wordSpacing3 : Elm.Expression
    , zIndex : Elm.Expression
    , zIndex2 : Elm.Expression
    , zIndex3 : Elm.Expression
    }
values_ =
    { transition =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transition"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Css", "Transitions" ]
                                "Transition"
                                []
                            )
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , ease =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "ease"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , linear =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "linear"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , easeIn =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "easeIn"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , easeOut =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "easeOut"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , easeInOut =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "easeInOut"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , stepStart =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "stepStart"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , stepEnd =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "stepEnd"
            , annotation =
                Just
                    (Type.namedWith [ "Css", "Transitions" ] "TimingFunction" []
                    )
            }
    , cubicBezier =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "cubicBezier"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float, Type.float ]
                        (Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        )
                    )
            }
    , background =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , background2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , background3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "background3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundPosition =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundPosition2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundPosition3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundSize =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundSize2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , backgroundSize3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "backgroundSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , border =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , border2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , border3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "border3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottom =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottom2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottom3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomLeftRadius =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomLeftRadius2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomLeftRadius3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomLeftRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomRightRadius =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomRightRadius2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomRightRadius3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomRightRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderBottomWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderBottomWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeft =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeft2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeft3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderLeftWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderLeftWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRadius =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRadius2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRadius3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderRightWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderRightWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTop =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTop2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTop3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopLeftRadius =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopLeftRadius2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopLeftRadius3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopLeftRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopRightRadius =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopRightRadius2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopRightRadius3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopRightRadius3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderTopWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderTopWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , borderWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "borderWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , bottom =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , bottom2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , bottom3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "bottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , boxShadow =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , boxShadow2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , boxShadow3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "boxShadow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , caretColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , caretColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , caretColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "caretColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clip =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clip2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clip3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clip3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clipPath =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clipPath2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , clipPath3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "clipPath3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , color =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , color2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , color3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "color3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnCount =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnCount2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnCount3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnCount3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnGap =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnGap2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnGap3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRule =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRule2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRule3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRule3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnRuleWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnRuleWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columnWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columnWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columns =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columns2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , columns3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "columns3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , filter =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , filter2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , filter3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "filter3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flex =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flex2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flex3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flex3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexBasis =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexBasis2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexBasis3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexBasis3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexGrow =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexGrow2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexGrow3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexGrow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexShrink =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexShrink2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , flexShrink3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "flexShrink3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , font =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , font2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , font3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "font3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSize =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSize2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSize3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSizeAdjust =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSizeAdjust2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontSizeAdjust3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontSizeAdjust3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontStretch =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontStretch2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontStretch3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontStretch3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontVariationSettings =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontVariationSettings2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontVariationSettings3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontVariationSettings3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontWeight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontWeight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , fontWeight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "fontWeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridColumnGap =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridColumnGap2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridColumnGap3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridColumnGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridGap =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridGap2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridGap3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridRowGap =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridRowGap2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , gridRowGap3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "gridRowGap3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , height =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , height2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , height3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "height3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , left =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , left2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , left3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "left3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , letterSpacing =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , letterSpacing2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , letterSpacing3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "letterSpacing3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , lineHeight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , lineHeight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , lineHeight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "lineHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , margin =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , margin2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , margin3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "margin3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginBottom =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginBottom2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginBottom3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginLeft =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginLeft2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginLeft3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginRight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginRight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginRight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginTop =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginTop2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , marginTop3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "marginTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , mask =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , mask2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , mask3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "mask3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskPosition =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskPosition2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskPosition3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskSize =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskSize2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maskSize3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maskSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxHeight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxHeight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxHeight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , maxWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "maxWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minHeight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minHeight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minHeight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minHeight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , minWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "minWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , objectPosition =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , objectPosition2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , objectPosition3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "objectPosition3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offset =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offset2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offset3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offset3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetAnchor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetAnchor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetAnchor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetAnchor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetDistance =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetDistance2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetDistance3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetDistance3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetPath =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetPath2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetPath3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetPath3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetRotate =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetRotate2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , offsetRotate3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "offsetRotate3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , opacity =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , opacity2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , opacity3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "opacity3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , order =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , order2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , order3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "order3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outline =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outline2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outline3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outline3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineColor =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineColor2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineColor3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineColor3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineOffset =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineOffset2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineOffset3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineOffset3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineWidth =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineWidth2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , outlineWidth3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "outlineWidth3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , padding =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , padding2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , padding3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "padding3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingBottom =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingBottom2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingBottom3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingBottom3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingLeft =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingLeft2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingLeft3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingLeft3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingRight =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingRight2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingRight3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingRight3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingTop =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingTop2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , paddingTop3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "paddingTop3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , right =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , right2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , right3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "right3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , tabSize =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , tabSize2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , tabSize3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "tabSize3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textIndent =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textIndent2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textIndent3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textIndent3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textShadow =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textShadow2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , textShadow3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "textShadow3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , top =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , top2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , top3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "top3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transform =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transform2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transform3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transform3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transformOrigin =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transformOrigin2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , transformOrigin3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "transformOrigin3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , verticalAlign =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , verticalAlign2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , verticalAlign3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "verticalAlign3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , visibility =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , visibility2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , visibility3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "visibility3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , width =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , width2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , width3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "width3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , wordSpacing =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , wordSpacing2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , wordSpacing3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "wordSpacing3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , zIndex =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , zIndex2 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex2"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    , zIndex3 =
        Elm.value
            { importFrom = [ "Css", "Transitions" ]
            , name = "zIndex3"
            , annotation =
                Just
                    (Type.function
                        [ Type.float
                        , Type.float
                        , Type.namedWith
                            [ "Css", "Transitions" ]
                            "TimingFunction"
                            []
                        ]
                        (Type.namedWith [ "Css", "Transitions" ] "Transition" []
                        )
                    )
            }
    }


