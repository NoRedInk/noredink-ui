module Gen.Svg.Styled.Attributes exposing (accelerate, accentHeight, accumulate, additive, alignmentBaseline, allowReorder, alphabetic, amplitude, arabicForm, ascent, attributeName, attributeType, autoReverse, azimuth, baseFrequency, baseProfile, baselineShift, bbox, begin, bias, by, calcMode, call_, capHeight, class, clip, clipPath, clipPathUnits, clipRule, color, colorInterpolation, colorInterpolationFilters, colorProfile, colorRendering, contentScriptType, contentStyleType, css, cursor, cx, cy, d, decelerate, descent, diffuseConstant, direction, display, divisor, dominantBaseline, dur, dx, dy, edgeMode, elevation, enableBackground, end, exponent, externalResourcesRequired, fill, fillOpacity, fillRule, filter, filterRes, filterUnits, floodColor, floodOpacity, fontFamily, fontSize, fontSizeAdjust, fontStretch, fontStyle, fontVariant, fontWeight, format, from, fromUnstyled, fx, fy, g1, g2, glyphName, glyphOrientationHorizontal, glyphOrientationVertical, glyphRef, gradientTransform, gradientUnits, hanging, height, horizAdvX, horizOriginX, horizOriginY, id, ideographic, imageRendering, in2, in_, intercept, k, k1, k2, k3, k4, kernelMatrix, kernelUnitLength, kerning, keyPoints, keySplines, keyTimes, lang, lengthAdjust, letterSpacing, lightingColor, limitingConeAngle, local, markerEnd, markerHeight, markerMid, markerStart, markerUnits, markerWidth, mask, maskContentUnits, maskUnits, mathematical, max, media, method, min, mode, moduleName_, name, numOctaves, offset, opacity, operator, order, orient, orientation, origin, overflow, overlinePosition, overlineThickness, panose1, path, pathLength, patternContentUnits, patternTransform, patternUnits, pointOrder, pointerEvents, points, pointsAtX, pointsAtY, pointsAtZ, preserveAlpha, preserveAspectRatio, primitiveUnits, r, radius, refX, refY, renderingIntent, repeatCount, repeatDur, requiredExtensions, requiredFeatures, restart, result, rotate, rx, ry, scale, seed, shapeRendering, slope, spacing, specularConstant, specularExponent, speed, spreadMethod, startOffset, stdDeviation, stemh, stemv, stitchTiles, stopColor, stopOpacity, strikethroughPosition, strikethroughThickness, string, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeOpacity, strokeWidth, style, surfaceScale, systemLanguage, tableValues, target, targetX, targetY, textAnchor, textDecoration, textLength, textRendering, title, to, transform, type_, u1, u2, underlinePosition, underlineThickness, unicode, unicodeBidi, unicodeRange, unitsPerEm, vAlphabetic, vHanging, vIdeographic, vMathematical, values, values_, version, vertAdvY, vertOriginX, vertOriginY, viewBox, viewTarget, visibility, width, widths, wordSpacing, writingMode, x, x1, x2, xChannelSelector, xHeight, xlinkActuate, xlinkArcrole, xlinkHref, xlinkRole, xlinkShow, xlinkTitle, xlinkType, xmlBase, xmlLang, xmlSpace, y, y1, y2, yChannelSelector, z, zoomAndPan)

{-| 
@docs moduleName_, css, fromUnstyled, accentHeight, accelerate, accumulate, additive, alphabetic, allowReorder, amplitude, arabicForm, ascent, attributeName, attributeType, autoReverse, azimuth, baseFrequency, baseProfile, bbox, begin, bias, by, calcMode, capHeight, class, clipPathUnits, contentScriptType, contentStyleType, cx, cy, d, decelerate, descent, diffuseConstant, divisor, dur, dx, dy, edgeMode, elevation, end, exponent, externalResourcesRequired, filterRes, filterUnits, format, from, fx, fy, g1, g2, glyphName, glyphRef, gradientTransform, gradientUnits, hanging, height, horizAdvX, horizOriginX, horizOriginY, id, ideographic, in_, in2, intercept, k, k1, k2, k3, k4, kernelMatrix, kernelUnitLength, keyPoints, keySplines, keyTimes, lang, lengthAdjust, limitingConeAngle, local, markerHeight, markerUnits, markerWidth, maskContentUnits, maskUnits, mathematical, max, media, method, min, mode, name, numOctaves, offset, operator, order, orient, orientation, origin, overlinePosition, overlineThickness, panose1, path, pathLength, patternContentUnits, patternTransform, patternUnits, pointOrder, points, pointsAtX, pointsAtY, pointsAtZ, preserveAlpha, preserveAspectRatio, primitiveUnits, r, radius, refX, refY, renderingIntent, repeatCount, repeatDur, requiredExtensions, requiredFeatures, restart, result, rotate, rx, ry, scale, seed, slope, spacing, specularConstant, specularExponent, speed, spreadMethod, startOffset, stdDeviation, stemh, stemv, stitchTiles, strikethroughPosition, strikethroughThickness, string, style, surfaceScale, systemLanguage, tableValues, target, targetX, targetY, textLength, title, to, transform, type_, u1, u2, underlinePosition, underlineThickness, unicode, unicodeRange, unitsPerEm, vAlphabetic, vHanging, vIdeographic, vMathematical, values, version, vertAdvY, vertOriginX, vertOriginY, viewBox, viewTarget, width, widths, x, xHeight, x1, x2, xChannelSelector, xlinkActuate, xlinkArcrole, xlinkHref, xlinkRole, xlinkShow, xlinkTitle, xlinkType, xmlBase, xmlLang, xmlSpace, y, y1, y2, yChannelSelector, z, zoomAndPan, alignmentBaseline, baselineShift, clipPath, clipRule, clip, colorInterpolationFilters, colorInterpolation, colorProfile, colorRendering, color, cursor, direction, display, dominantBaseline, enableBackground, fillOpacity, fillRule, fill, filter, floodColor, floodOpacity, fontFamily, fontSizeAdjust, fontSize, fontStretch, fontStyle, fontVariant, fontWeight, glyphOrientationHorizontal, glyphOrientationVertical, imageRendering, kerning, letterSpacing, lightingColor, markerEnd, markerMid, markerStart, mask, opacity, overflow, pointerEvents, shapeRendering, stopColor, stopOpacity, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeOpacity, strokeWidth, stroke, textAnchor, textDecoration, textRendering, unicodeBidi, visibility, wordSpacing, writingMode, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Svg", "Styled", "Attributes" ]


{-| Apply styles to an element.

See the [`Css` module documentation](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css) for an overview of how to use this function.

css: List Css.Style -> Svg.Styled.Attribute msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| fromUnstyled: VirtualDom.Attribute msg -> Svg.Styled.Attribute msg -}
fromUnstyled : Elm.Expression -> Elm.Expression
fromUnstyled fromUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ fromUnstyledArg ]


{-| accentHeight: String -> Svg.Styled.Attribute msg -}
accentHeight : String -> Elm.Expression
accentHeight accentHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accentHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string accentHeightArg ]


{-| accelerate: String -> Svg.Styled.Attribute msg -}
accelerate : String -> Elm.Expression
accelerate accelerateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accelerate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string accelerateArg ]


{-| accumulate: String -> Svg.Styled.Attribute msg -}
accumulate : String -> Elm.Expression
accumulate accumulateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accumulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string accumulateArg ]


{-| additive: String -> Svg.Styled.Attribute msg -}
additive : String -> Elm.Expression
additive additiveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "additive"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string additiveArg ]


{-| alphabetic: String -> Svg.Styled.Attribute msg -}
alphabetic : String -> Elm.Expression
alphabetic alphabeticArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "alphabetic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string alphabeticArg ]


{-| allowReorder: String -> Svg.Styled.Attribute msg -}
allowReorder : String -> Elm.Expression
allowReorder allowReorderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "allowReorder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string allowReorderArg ]


{-| amplitude: String -> Svg.Styled.Attribute msg -}
amplitude : String -> Elm.Expression
amplitude amplitudeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "amplitude"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string amplitudeArg ]


{-| arabicForm: String -> Svg.Styled.Attribute msg -}
arabicForm : String -> Elm.Expression
arabicForm arabicFormArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "arabicForm"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string arabicFormArg ]


{-| ascent: String -> Svg.Styled.Attribute msg -}
ascent : String -> Elm.Expression
ascent ascentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ascent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string ascentArg ]


{-| attributeName: String -> Svg.Styled.Attribute msg -}
attributeName : String -> Elm.Expression
attributeName attributeNameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "attributeName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string attributeNameArg ]


{-| attributeType: String -> Svg.Styled.Attribute msg -}
attributeType : String -> Elm.Expression
attributeType attributeTypeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "attributeType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string attributeTypeArg ]


{-| autoReverse: String -> Svg.Styled.Attribute msg -}
autoReverse : String -> Elm.Expression
autoReverse autoReverseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "autoReverse"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string autoReverseArg ]


{-| azimuth: String -> Svg.Styled.Attribute msg -}
azimuth : String -> Elm.Expression
azimuth azimuthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "azimuth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string azimuthArg ]


{-| baseFrequency: String -> Svg.Styled.Attribute msg -}
baseFrequency : String -> Elm.Expression
baseFrequency baseFrequencyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baseFrequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string baseFrequencyArg ]


{-| baseProfile: String -> Svg.Styled.Attribute msg -}
baseProfile : String -> Elm.Expression
baseProfile baseProfileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baseProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string baseProfileArg ]


{-| bbox: String -> Svg.Styled.Attribute msg -}
bbox : String -> Elm.Expression
bbox bboxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "bbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string bboxArg ]


{-| begin: String -> Svg.Styled.Attribute msg -}
begin : String -> Elm.Expression
begin beginArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "begin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string beginArg ]


{-| bias: String -> Svg.Styled.Attribute msg -}
bias : String -> Elm.Expression
bias biasArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "bias"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string biasArg ]


{-| by: String -> Svg.Styled.Attribute msg -}
by : String -> Elm.Expression
by byArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "by"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string byArg ]


{-| calcMode: String -> Svg.Styled.Attribute msg -}
calcMode : String -> Elm.Expression
calcMode calcModeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "calcMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string calcModeArg ]


{-| capHeight: String -> Svg.Styled.Attribute msg -}
capHeight : String -> Elm.Expression
capHeight capHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "capHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string capHeightArg ]


{-| class: String -> Svg.Styled.Attribute msg -}
class : String -> Elm.Expression
class classArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string classArg ]


{-| clipPathUnits: String -> Svg.Styled.Attribute msg -}
clipPathUnits : String -> Elm.Expression
clipPathUnits clipPathUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipPathUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string clipPathUnitsArg ]


{-| contentScriptType: String -> Svg.Styled.Attribute msg -}
contentScriptType : String -> Elm.Expression
contentScriptType contentScriptTypeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "contentScriptType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string contentScriptTypeArg ]


{-| contentStyleType: String -> Svg.Styled.Attribute msg -}
contentStyleType : String -> Elm.Expression
contentStyleType contentStyleTypeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "contentStyleType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string contentStyleTypeArg ]


{-| cx: String -> Svg.Styled.Attribute msg -}
cx : String -> Elm.Expression
cx cxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string cxArg ]


{-| cy: String -> Svg.Styled.Attribute msg -}
cy : String -> Elm.Expression
cy cyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string cyArg ]


{-| d: String -> Svg.Styled.Attribute msg -}
d : String -> Elm.Expression
d dArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "d"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string dArg ]


{-| decelerate: String -> Svg.Styled.Attribute msg -}
decelerate : String -> Elm.Expression
decelerate decelerateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "decelerate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string decelerateArg ]


{-| descent: String -> Svg.Styled.Attribute msg -}
descent : String -> Elm.Expression
descent descentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "descent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string descentArg ]


{-| diffuseConstant: String -> Svg.Styled.Attribute msg -}
diffuseConstant : String -> Elm.Expression
diffuseConstant diffuseConstantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "diffuseConstant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string diffuseConstantArg ]


{-| divisor: String -> Svg.Styled.Attribute msg -}
divisor : String -> Elm.Expression
divisor divisorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "divisor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string divisorArg ]


{-| dur: String -> Svg.Styled.Attribute msg -}
dur : String -> Elm.Expression
dur durArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string durArg ]


{-| dx: String -> Svg.Styled.Attribute msg -}
dx : String -> Elm.Expression
dx dxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string dxArg ]


{-| dy: String -> Svg.Styled.Attribute msg -}
dy : String -> Elm.Expression
dy dyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string dyArg ]


{-| edgeMode: String -> Svg.Styled.Attribute msg -}
edgeMode : String -> Elm.Expression
edgeMode edgeModeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "edgeMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string edgeModeArg ]


{-| elevation: String -> Svg.Styled.Attribute msg -}
elevation : String -> Elm.Expression
elevation elevationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "elevation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string elevationArg ]


{-| end: String -> Svg.Styled.Attribute msg -}
end : String -> Elm.Expression
end endArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "end"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string endArg ]


{-| exponent: String -> Svg.Styled.Attribute msg -}
exponent : String -> Elm.Expression
exponent exponentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "exponent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string exponentArg ]


{-| externalResourcesRequired: String -> Svg.Styled.Attribute msg -}
externalResourcesRequired : String -> Elm.Expression
externalResourcesRequired externalResourcesRequiredArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "externalResourcesRequired"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string externalResourcesRequiredArg ]


{-| filterRes: String -> Svg.Styled.Attribute msg -}
filterRes : String -> Elm.Expression
filterRes filterResArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filterRes"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string filterResArg ]


{-| filterUnits: String -> Svg.Styled.Attribute msg -}
filterUnits : String -> Elm.Expression
filterUnits filterUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filterUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string filterUnitsArg ]


{-| format: String -> Svg.Styled.Attribute msg -}
format : String -> Elm.Expression
format formatArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "format"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string formatArg ]


{-| from: String -> Svg.Styled.Attribute msg -}
from : String -> Elm.Expression
from fromArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "from"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fromArg ]


{-| fx: String -> Svg.Styled.Attribute msg -}
fx : String -> Elm.Expression
fx fxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fxArg ]


{-| fy: String -> Svg.Styled.Attribute msg -}
fy : String -> Elm.Expression
fy fyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fyArg ]


{-| g1: String -> Svg.Styled.Attribute msg -}
g1 : String -> Elm.Expression
g1 g1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "g1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string g1Arg ]


{-| g2: String -> Svg.Styled.Attribute msg -}
g2 : String -> Elm.Expression
g2 g2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "g2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string g2Arg ]


{-| glyphName: String -> Svg.Styled.Attribute msg -}
glyphName : String -> Elm.Expression
glyphName glyphNameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string glyphNameArg ]


{-| glyphRef: String -> Svg.Styled.Attribute msg -}
glyphRef : String -> Elm.Expression
glyphRef glyphRefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphRef"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string glyphRefArg ]


{-| gradientTransform: String -> Svg.Styled.Attribute msg -}
gradientTransform : String -> Elm.Expression
gradientTransform gradientTransformArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "gradientTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string gradientTransformArg ]


{-| gradientUnits: String -> Svg.Styled.Attribute msg -}
gradientUnits : String -> Elm.Expression
gradientUnits gradientUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "gradientUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string gradientUnitsArg ]


{-| hanging: String -> Svg.Styled.Attribute msg -}
hanging : String -> Elm.Expression
hanging hangingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "hanging"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string hangingArg ]


{-| height: String -> Svg.Styled.Attribute msg -}
height : String -> Elm.Expression
height heightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string heightArg ]


{-| horizAdvX: String -> Svg.Styled.Attribute msg -}
horizAdvX : String -> Elm.Expression
horizAdvX horizAdvXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizAdvX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string horizAdvXArg ]


{-| horizOriginX: String -> Svg.Styled.Attribute msg -}
horizOriginX : String -> Elm.Expression
horizOriginX horizOriginXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizOriginX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string horizOriginXArg ]


{-| horizOriginY: String -> Svg.Styled.Attribute msg -}
horizOriginY : String -> Elm.Expression
horizOriginY horizOriginYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizOriginY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string horizOriginYArg ]


{-| id: String -> Svg.Styled.Attribute msg -}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| ideographic: String -> Svg.Styled.Attribute msg -}
ideographic : String -> Elm.Expression
ideographic ideographicArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ideographic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string ideographicArg ]


{-| in_: String -> Svg.Styled.Attribute msg -}
in_ : String -> Elm.Expression
in_ in_Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "in_"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string in_Arg ]


{-| in2: String -> Svg.Styled.Attribute msg -}
in2 : String -> Elm.Expression
in2 in2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "in2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string in2Arg ]


{-| intercept: String -> Svg.Styled.Attribute msg -}
intercept : String -> Elm.Expression
intercept interceptArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "intercept"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string interceptArg ]


{-| k: String -> Svg.Styled.Attribute msg -}
k : String -> Elm.Expression
k kArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string kArg ]


{-| k1: String -> Svg.Styled.Attribute msg -}
k1 : String -> Elm.Expression
k1 k1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string k1Arg ]


{-| k2: String -> Svg.Styled.Attribute msg -}
k2 : String -> Elm.Expression
k2 k2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string k2Arg ]


{-| k3: String -> Svg.Styled.Attribute msg -}
k3 : String -> Elm.Expression
k3 k3Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k3"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string k3Arg ]


{-| k4: String -> Svg.Styled.Attribute msg -}
k4 : String -> Elm.Expression
k4 k4Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k4"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string k4Arg ]


{-| kernelMatrix: String -> Svg.Styled.Attribute msg -}
kernelMatrix : String -> Elm.Expression
kernelMatrix kernelMatrixArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kernelMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string kernelMatrixArg ]


{-| kernelUnitLength: String -> Svg.Styled.Attribute msg -}
kernelUnitLength : String -> Elm.Expression
kernelUnitLength kernelUnitLengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kernelUnitLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string kernelUnitLengthArg ]


{-| keyPoints: String -> Svg.Styled.Attribute msg -}
keyPoints : String -> Elm.Expression
keyPoints keyPointsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keyPoints"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string keyPointsArg ]


{-| keySplines: String -> Svg.Styled.Attribute msg -}
keySplines : String -> Elm.Expression
keySplines keySplinesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keySplines"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string keySplinesArg ]


{-| keyTimes: String -> Svg.Styled.Attribute msg -}
keyTimes : String -> Elm.Expression
keyTimes keyTimesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keyTimes"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string keyTimesArg ]


{-| lang: String -> Svg.Styled.Attribute msg -}
lang : String -> Elm.Expression
lang langArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lang"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string langArg ]


{-| lengthAdjust: String -> Svg.Styled.Attribute msg -}
lengthAdjust : String -> Elm.Expression
lengthAdjust lengthAdjustArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lengthAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string lengthAdjustArg ]


{-| limitingConeAngle: String -> Svg.Styled.Attribute msg -}
limitingConeAngle : String -> Elm.Expression
limitingConeAngle limitingConeAngleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "limitingConeAngle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string limitingConeAngleArg ]


{-| local: String -> Svg.Styled.Attribute msg -}
local : String -> Elm.Expression
local localArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "local"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string localArg ]


{-| markerHeight: String -> Svg.Styled.Attribute msg -}
markerHeight : String -> Elm.Expression
markerHeight markerHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerHeightArg ]


{-| markerUnits: String -> Svg.Styled.Attribute msg -}
markerUnits : String -> Elm.Expression
markerUnits markerUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerUnitsArg ]


{-| markerWidth: String -> Svg.Styled.Attribute msg -}
markerWidth : String -> Elm.Expression
markerWidth markerWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerWidthArg ]


{-| maskContentUnits: String -> Svg.Styled.Attribute msg -}
maskContentUnits : String -> Elm.Expression
maskContentUnits maskContentUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "maskContentUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maskContentUnitsArg ]


{-| maskUnits: String -> Svg.Styled.Attribute msg -}
maskUnits : String -> Elm.Expression
maskUnits maskUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "maskUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maskUnitsArg ]


{-| mathematical: String -> Svg.Styled.Attribute msg -}
mathematical : String -> Elm.Expression
mathematical mathematicalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mathematical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string mathematicalArg ]


{-| max: String -> Svg.Styled.Attribute msg -}
max : String -> Elm.Expression
max maxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "max"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maxArg ]


{-| media: String -> Svg.Styled.Attribute msg -}
media : String -> Elm.Expression
media mediaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "media"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string mediaArg ]


{-| method: String -> Svg.Styled.Attribute msg -}
method : String -> Elm.Expression
method methodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "method"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string methodArg ]


{-| min: String -> Svg.Styled.Attribute msg -}
min : String -> Elm.Expression
min minArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "min"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string minArg ]


{-| mode: String -> Svg.Styled.Attribute msg -}
mode : String -> Elm.Expression
mode modeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string modeArg ]


{-| name: String -> Svg.Styled.Attribute msg -}
name : String -> Elm.Expression
name nameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "name"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nameArg ]


{-| numOctaves: String -> Svg.Styled.Attribute msg -}
numOctaves : String -> Elm.Expression
numOctaves numOctavesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "numOctaves"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string numOctavesArg ]


{-| offset: String -> Svg.Styled.Attribute msg -}
offset : String -> Elm.Expression
offset offsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "offset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string offsetArg ]


{-| operator: String -> Svg.Styled.Attribute msg -}
operator : String -> Elm.Expression
operator operatorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "operator"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string operatorArg ]


{-| order: String -> Svg.Styled.Attribute msg -}
order : String -> Elm.Expression
order orderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string orderArg ]


{-| orient: String -> Svg.Styled.Attribute msg -}
orient : String -> Elm.Expression
orient orientArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "orient"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string orientArg ]


{-| orientation: String -> Svg.Styled.Attribute msg -}
orientation : String -> Elm.Expression
orientation orientationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "orientation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string orientationArg ]


{-| origin: String -> Svg.Styled.Attribute msg -}
origin : String -> Elm.Expression
origin originArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "origin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string originArg ]


{-| overlinePosition: String -> Svg.Styled.Attribute msg -}
overlinePosition : String -> Elm.Expression
overlinePosition overlinePositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overlinePosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string overlinePositionArg ]


{-| overlineThickness: String -> Svg.Styled.Attribute msg -}
overlineThickness : String -> Elm.Expression
overlineThickness overlineThicknessArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overlineThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string overlineThicknessArg ]


{-| panose1: String -> Svg.Styled.Attribute msg -}
panose1 : String -> Elm.Expression
panose1 panose1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "panose1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string panose1Arg ]


{-| path: String -> Svg.Styled.Attribute msg -}
path : String -> Elm.Expression
path pathArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pathArg ]


{-| pathLength: String -> Svg.Styled.Attribute msg -}
pathLength : String -> Elm.Expression
pathLength pathLengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pathLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pathLengthArg ]


{-| patternContentUnits: String -> Svg.Styled.Attribute msg -}
patternContentUnits : String -> Elm.Expression
patternContentUnits patternContentUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternContentUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string patternContentUnitsArg ]


{-| patternTransform: String -> Svg.Styled.Attribute msg -}
patternTransform : String -> Elm.Expression
patternTransform patternTransformArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string patternTransformArg ]


{-| patternUnits: String -> Svg.Styled.Attribute msg -}
patternUnits : String -> Elm.Expression
patternUnits patternUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string patternUnitsArg ]


{-| pointOrder: String -> Svg.Styled.Attribute msg -}
pointOrder : String -> Elm.Expression
pointOrder pointOrderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointOrder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointOrderArg ]


{-| points: String -> Svg.Styled.Attribute msg -}
points : String -> Elm.Expression
points pointsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "points"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointsArg ]


{-| pointsAtX: String -> Svg.Styled.Attribute msg -}
pointsAtX : String -> Elm.Expression
pointsAtX pointsAtXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointsAtXArg ]


{-| pointsAtY: String -> Svg.Styled.Attribute msg -}
pointsAtY : String -> Elm.Expression
pointsAtY pointsAtYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointsAtYArg ]


{-| pointsAtZ: String -> Svg.Styled.Attribute msg -}
pointsAtZ : String -> Elm.Expression
pointsAtZ pointsAtZArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtZ"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointsAtZArg ]


{-| preserveAlpha: String -> Svg.Styled.Attribute msg -}
preserveAlpha : String -> Elm.Expression
preserveAlpha preserveAlphaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "preserveAlpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string preserveAlphaArg ]


{-| preserveAspectRatio: String -> Svg.Styled.Attribute msg -}
preserveAspectRatio : String -> Elm.Expression
preserveAspectRatio preserveAspectRatioArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "preserveAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string preserveAspectRatioArg ]


{-| primitiveUnits: String -> Svg.Styled.Attribute msg -}
primitiveUnits : String -> Elm.Expression
primitiveUnits primitiveUnitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "primitiveUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string primitiveUnitsArg ]


{-| r: String -> Svg.Styled.Attribute msg -}
r : String -> Elm.Expression
r rArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "r"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string rArg ]


{-| radius: String -> Svg.Styled.Attribute msg -}
radius : String -> Elm.Expression
radius radiusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "radius"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string radiusArg ]


{-| refX: String -> Svg.Styled.Attribute msg -}
refX : String -> Elm.Expression
refX refXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "refX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string refXArg ]


{-| refY: String -> Svg.Styled.Attribute msg -}
refY : String -> Elm.Expression
refY refYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "refY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string refYArg ]


{-| renderingIntent: String -> Svg.Styled.Attribute msg -}
renderingIntent : String -> Elm.Expression
renderingIntent renderingIntentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "renderingIntent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string renderingIntentArg ]


{-| repeatCount: String -> Svg.Styled.Attribute msg -}
repeatCount : String -> Elm.Expression
repeatCount repeatCountArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "repeatCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string repeatCountArg ]


{-| repeatDur: String -> Svg.Styled.Attribute msg -}
repeatDur : String -> Elm.Expression
repeatDur repeatDurArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "repeatDur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string repeatDurArg ]


{-| requiredExtensions: String -> Svg.Styled.Attribute msg -}
requiredExtensions : String -> Elm.Expression
requiredExtensions requiredExtensionsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "requiredExtensions"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string requiredExtensionsArg ]


{-| requiredFeatures: String -> Svg.Styled.Attribute msg -}
requiredFeatures : String -> Elm.Expression
requiredFeatures requiredFeaturesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "requiredFeatures"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string requiredFeaturesArg ]


{-| restart: String -> Svg.Styled.Attribute msg -}
restart : String -> Elm.Expression
restart restartArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "restart"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string restartArg ]


{-| result: String -> Svg.Styled.Attribute msg -}
result : String -> Elm.Expression
result resultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string resultArg ]


{-| rotate: String -> Svg.Styled.Attribute msg -}
rotate : String -> Elm.Expression
rotate rotateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "rotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string rotateArg ]


{-| rx: String -> Svg.Styled.Attribute msg -}
rx : String -> Elm.Expression
rx rxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "rx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string rxArg ]


{-| ry: String -> Svg.Styled.Attribute msg -}
ry : String -> Elm.Expression
ry ryArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string ryArg ]


{-| scale: String -> Svg.Styled.Attribute msg -}
scale : String -> Elm.Expression
scale scaleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "scale"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string scaleArg ]


{-| seed: String -> Svg.Styled.Attribute msg -}
seed : String -> Elm.Expression
seed seedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "seed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string seedArg ]


{-| slope: String -> Svg.Styled.Attribute msg -}
slope : String -> Elm.Expression
slope slopeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "slope"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string slopeArg ]


{-| spacing: String -> Svg.Styled.Attribute msg -}
spacing : String -> Elm.Expression
spacing spacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "spacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string spacingArg ]


{-| specularConstant: String -> Svg.Styled.Attribute msg -}
specularConstant : String -> Elm.Expression
specularConstant specularConstantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "specularConstant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string specularConstantArg ]


{-| specularExponent: String -> Svg.Styled.Attribute msg -}
specularExponent : String -> Elm.Expression
specularExponent specularExponentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "specularExponent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string specularExponentArg ]


{-| speed: String -> Svg.Styled.Attribute msg -}
speed : String -> Elm.Expression
speed speedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "speed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string speedArg ]


{-| spreadMethod: String -> Svg.Styled.Attribute msg -}
spreadMethod : String -> Elm.Expression
spreadMethod spreadMethodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "spreadMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string spreadMethodArg ]


{-| startOffset: String -> Svg.Styled.Attribute msg -}
startOffset : String -> Elm.Expression
startOffset startOffsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "startOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string startOffsetArg ]


{-| stdDeviation: String -> Svg.Styled.Attribute msg -}
stdDeviation : String -> Elm.Expression
stdDeviation stdDeviationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stdDeviation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stdDeviationArg ]


{-| stemh: String -> Svg.Styled.Attribute msg -}
stemh : String -> Elm.Expression
stemh stemhArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stemh"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stemhArg ]


{-| stemv: String -> Svg.Styled.Attribute msg -}
stemv : String -> Elm.Expression
stemv stemvArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stemv"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stemvArg ]


{-| stitchTiles: String -> Svg.Styled.Attribute msg -}
stitchTiles : String -> Elm.Expression
stitchTiles stitchTilesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stitchTiles"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stitchTilesArg ]


{-| strikethroughPosition: String -> Svg.Styled.Attribute msg -}
strikethroughPosition : String -> Elm.Expression
strikethroughPosition strikethroughPositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strikethroughPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strikethroughPositionArg ]


{-| strikethroughThickness: String -> Svg.Styled.Attribute msg -}
strikethroughThickness : String -> Elm.Expression
strikethroughThickness strikethroughThicknessArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strikethroughThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strikethroughThicknessArg ]


{-| string: String -> Svg.Styled.Attribute msg -}
string : String -> Elm.Expression
string stringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stringArg ]


{-| style: String -> Svg.Styled.Attribute msg -}
style : String -> Elm.Expression
style styleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string styleArg ]


{-| surfaceScale: String -> Svg.Styled.Attribute msg -}
surfaceScale : String -> Elm.Expression
surfaceScale surfaceScaleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "surfaceScale"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string surfaceScaleArg ]


{-| systemLanguage: String -> Svg.Styled.Attribute msg -}
systemLanguage : String -> Elm.Expression
systemLanguage systemLanguageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "systemLanguage"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string systemLanguageArg ]


{-| tableValues: String -> Svg.Styled.Attribute msg -}
tableValues : String -> Elm.Expression
tableValues tableValuesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "tableValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string tableValuesArg ]


{-| target: String -> Svg.Styled.Attribute msg -}
target : String -> Elm.Expression
target targetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "target"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string targetArg ]


{-| targetX: String -> Svg.Styled.Attribute msg -}
targetX : String -> Elm.Expression
targetX targetXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "targetX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string targetXArg ]


{-| targetY: String -> Svg.Styled.Attribute msg -}
targetY : String -> Elm.Expression
targetY targetYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "targetY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string targetYArg ]


{-| textLength: String -> Svg.Styled.Attribute msg -}
textLength : String -> Elm.Expression
textLength textLengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textLengthArg ]


{-| title: String -> Svg.Styled.Attribute msg -}
title : String -> Elm.Expression
title titleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "title"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string titleArg ]


{-| to: String -> Svg.Styled.Attribute msg -}
to : String -> Elm.Expression
to toArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "to"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string toArg ]


{-| transform: String -> Svg.Styled.Attribute msg -}
transform : String -> Elm.Expression
transform transformArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string transformArg ]


{-| type_: String -> Svg.Styled.Attribute msg -}
type_ : String -> Elm.Expression
type_ type_Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "type_"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string type_Arg ]


{-| u1: String -> Svg.Styled.Attribute msg -}
u1 : String -> Elm.Expression
u1 u1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "u1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string u1Arg ]


{-| u2: String -> Svg.Styled.Attribute msg -}
u2 : String -> Elm.Expression
u2 u2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "u2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string u2Arg ]


{-| underlinePosition: String -> Svg.Styled.Attribute msg -}
underlinePosition : String -> Elm.Expression
underlinePosition underlinePositionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "underlinePosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string underlinePositionArg ]


{-| underlineThickness: String -> Svg.Styled.Attribute msg -}
underlineThickness : String -> Elm.Expression
underlineThickness underlineThicknessArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "underlineThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string underlineThicknessArg ]


{-| unicode: String -> Svg.Styled.Attribute msg -}
unicode : String -> Elm.Expression
unicode unicodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string unicodeArg ]


{-| unicodeRange: String -> Svg.Styled.Attribute msg -}
unicodeRange : String -> Elm.Expression
unicodeRange unicodeRangeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicodeRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string unicodeRangeArg ]


{-| unitsPerEm: String -> Svg.Styled.Attribute msg -}
unitsPerEm : String -> Elm.Expression
unitsPerEm unitsPerEmArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unitsPerEm"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string unitsPerEmArg ]


{-| vAlphabetic: String -> Svg.Styled.Attribute msg -}
vAlphabetic : String -> Elm.Expression
vAlphabetic vAlphabeticArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vAlphabetic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vAlphabeticArg ]


{-| vHanging: String -> Svg.Styled.Attribute msg -}
vHanging : String -> Elm.Expression
vHanging vHangingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vHanging"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vHangingArg ]


{-| vIdeographic: String -> Svg.Styled.Attribute msg -}
vIdeographic : String -> Elm.Expression
vIdeographic vIdeographicArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vIdeographic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vIdeographicArg ]


{-| vMathematical: String -> Svg.Styled.Attribute msg -}
vMathematical : String -> Elm.Expression
vMathematical vMathematicalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vMathematical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vMathematicalArg ]


{-| values: String -> Svg.Styled.Attribute msg -}
values : String -> Elm.Expression
values valuesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "values"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string valuesArg ]


{-| version: String -> Svg.Styled.Attribute msg -}
version : String -> Elm.Expression
version versionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "version"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string versionArg ]


{-| vertAdvY: String -> Svg.Styled.Attribute msg -}
vertAdvY : String -> Elm.Expression
vertAdvY vertAdvYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertAdvY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vertAdvYArg ]


{-| vertOriginX: String -> Svg.Styled.Attribute msg -}
vertOriginX : String -> Elm.Expression
vertOriginX vertOriginXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertOriginX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vertOriginXArg ]


{-| vertOriginY: String -> Svg.Styled.Attribute msg -}
vertOriginY : String -> Elm.Expression
vertOriginY vertOriginYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertOriginY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string vertOriginYArg ]


{-| viewBox: String -> Svg.Styled.Attribute msg -}
viewBox : String -> Elm.Expression
viewBox viewBoxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "viewBox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string viewBoxArg ]


{-| viewTarget: String -> Svg.Styled.Attribute msg -}
viewTarget : String -> Elm.Expression
viewTarget viewTargetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "viewTarget"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string viewTargetArg ]


{-| width: String -> Svg.Styled.Attribute msg -}
width : String -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string widthArg ]


{-| widths: String -> Svg.Styled.Attribute msg -}
widths : String -> Elm.Expression
widths widthsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "widths"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string widthsArg ]


{-| x: String -> Svg.Styled.Attribute msg -}
x : String -> Elm.Expression
x xArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xArg ]


{-| xHeight: String -> Svg.Styled.Attribute msg -}
xHeight : String -> Elm.Expression
xHeight xHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xHeightArg ]


{-| x1: String -> Svg.Styled.Attribute msg -}
x1 : String -> Elm.Expression
x1 x1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string x1Arg ]


{-| x2: String -> Svg.Styled.Attribute msg -}
x2 : String -> Elm.Expression
x2 x2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string x2Arg ]


{-| xChannelSelector: String -> Svg.Styled.Attribute msg -}
xChannelSelector : String -> Elm.Expression
xChannelSelector xChannelSelectorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xChannelSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xChannelSelectorArg ]


{-| xlinkActuate: String -> Svg.Styled.Attribute msg -}
xlinkActuate : String -> Elm.Expression
xlinkActuate xlinkActuateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkActuate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkActuateArg ]


{-| xlinkArcrole: String -> Svg.Styled.Attribute msg -}
xlinkArcrole : String -> Elm.Expression
xlinkArcrole xlinkArcroleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkArcrole"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkArcroleArg ]


{-| xlinkHref: String -> Svg.Styled.Attribute msg -}
xlinkHref : String -> Elm.Expression
xlinkHref xlinkHrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkHref"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkHrefArg ]


{-| xlinkRole: String -> Svg.Styled.Attribute msg -}
xlinkRole : String -> Elm.Expression
xlinkRole xlinkRoleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkRole"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkRoleArg ]


{-| xlinkShow: String -> Svg.Styled.Attribute msg -}
xlinkShow : String -> Elm.Expression
xlinkShow xlinkShowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkShow"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkShowArg ]


{-| xlinkTitle: String -> Svg.Styled.Attribute msg -}
xlinkTitle : String -> Elm.Expression
xlinkTitle xlinkTitleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkTitleArg ]


{-| xlinkType: String -> Svg.Styled.Attribute msg -}
xlinkType : String -> Elm.Expression
xlinkType xlinkTypeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xlinkTypeArg ]


{-| xmlBase: String -> Svg.Styled.Attribute msg -}
xmlBase : String -> Elm.Expression
xmlBase xmlBaseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlBase"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xmlBaseArg ]


{-| xmlLang: String -> Svg.Styled.Attribute msg -}
xmlLang : String -> Elm.Expression
xmlLang xmlLangArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlLang"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xmlLangArg ]


{-| xmlSpace: String -> Svg.Styled.Attribute msg -}
xmlSpace : String -> Elm.Expression
xmlSpace xmlSpaceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlSpace"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string xmlSpaceArg ]


{-| y: String -> Svg.Styled.Attribute msg -}
y : String -> Elm.Expression
y yArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string yArg ]


{-| y1: String -> Svg.Styled.Attribute msg -}
y1 : String -> Elm.Expression
y1 y1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string y1Arg ]


{-| y2: String -> Svg.Styled.Attribute msg -}
y2 : String -> Elm.Expression
y2 y2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string y2Arg ]


{-| yChannelSelector: String -> Svg.Styled.Attribute msg -}
yChannelSelector : String -> Elm.Expression
yChannelSelector yChannelSelectorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "yChannelSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string yChannelSelectorArg ]


{-| z: String -> Svg.Styled.Attribute msg -}
z : String -> Elm.Expression
z zArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "z"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string zArg ]


{-| zoomAndPan: String -> Svg.Styled.Attribute msg -}
zoomAndPan : String -> Elm.Expression
zoomAndPan zoomAndPanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "zoomAndPan"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string zoomAndPanArg ]


{-| alignmentBaseline: String -> Svg.Styled.Attribute msg -}
alignmentBaseline : String -> Elm.Expression
alignmentBaseline alignmentBaselineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "alignmentBaseline"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string alignmentBaselineArg ]


{-| baselineShift: String -> Svg.Styled.Attribute msg -}
baselineShift : String -> Elm.Expression
baselineShift baselineShiftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baselineShift"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string baselineShiftArg ]


{-| clipPath: String -> Svg.Styled.Attribute msg -}
clipPath : String -> Elm.Expression
clipPath clipPathArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string clipPathArg ]


{-| clipRule: String -> Svg.Styled.Attribute msg -}
clipRule : String -> Elm.Expression
clipRule clipRuleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string clipRuleArg ]


{-| clip: String -> Svg.Styled.Attribute msg -}
clip : String -> Elm.Expression
clip clipArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clip"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string clipArg ]


{-| colorInterpolationFilters: String -> Svg.Styled.Attribute msg -}
colorInterpolationFilters : String -> Elm.Expression
colorInterpolationFilters colorInterpolationFiltersArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorInterpolationFilters"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string colorInterpolationFiltersArg ]


{-| colorInterpolation: String -> Svg.Styled.Attribute msg -}
colorInterpolation : String -> Elm.Expression
colorInterpolation colorInterpolationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorInterpolation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string colorInterpolationArg ]


{-| colorProfile: String -> Svg.Styled.Attribute msg -}
colorProfile : String -> Elm.Expression
colorProfile colorProfileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string colorProfileArg ]


{-| colorRendering: String -> Svg.Styled.Attribute msg -}
colorRendering : String -> Elm.Expression
colorRendering colorRenderingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string colorRenderingArg ]


{-| color: String -> Svg.Styled.Attribute msg -}
color : String -> Elm.Expression
color colorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string colorArg ]


{-| cursor: String -> Svg.Styled.Attribute msg -}
cursor : String -> Elm.Expression
cursor cursorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cursor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string cursorArg ]


{-| direction: String -> Svg.Styled.Attribute msg -}
direction : String -> Elm.Expression
direction directionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "direction"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string directionArg ]


{-| display: String -> Svg.Styled.Attribute msg -}
display : String -> Elm.Expression
display displayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "display"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string displayArg ]


{-| dominantBaseline: String -> Svg.Styled.Attribute msg -}
dominantBaseline : String -> Elm.Expression
dominantBaseline dominantBaselineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dominantBaseline"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string dominantBaselineArg ]


{-| enableBackground: String -> Svg.Styled.Attribute msg -}
enableBackground : String -> Elm.Expression
enableBackground enableBackgroundArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "enableBackground"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string enableBackgroundArg ]


{-| fillOpacity: String -> Svg.Styled.Attribute msg -}
fillOpacity : String -> Elm.Expression
fillOpacity fillOpacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fillOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fillOpacityArg ]


{-| fillRule: String -> Svg.Styled.Attribute msg -}
fillRule : String -> Elm.Expression
fillRule fillRuleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fillRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fillRuleArg ]


{-| fill: String -> Svg.Styled.Attribute msg -}
fill : String -> Elm.Expression
fill fillArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fill"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fillArg ]


{-| filter: String -> Svg.Styled.Attribute msg -}
filter : String -> Elm.Expression
filter filterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string filterArg ]


{-| floodColor: String -> Svg.Styled.Attribute msg -}
floodColor : String -> Elm.Expression
floodColor floodColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "floodColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string floodColorArg ]


{-| floodOpacity: String -> Svg.Styled.Attribute msg -}
floodOpacity : String -> Elm.Expression
floodOpacity floodOpacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "floodOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string floodOpacityArg ]


{-| fontFamily: String -> Svg.Styled.Attribute msg -}
fontFamily : String -> Elm.Expression
fontFamily fontFamilyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontFamily"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontFamilyArg ]


{-| fontSizeAdjust: String -> Svg.Styled.Attribute msg -}
fontSizeAdjust : String -> Elm.Expression
fontSizeAdjust fontSizeAdjustArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontSizeAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontSizeAdjustArg ]


{-| fontSize: String -> Svg.Styled.Attribute msg -}
fontSize : String -> Elm.Expression
fontSize fontSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontSizeArg ]


{-| fontStretch: String -> Svg.Styled.Attribute msg -}
fontStretch : String -> Elm.Expression
fontStretch fontStretchArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontStretch"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontStretchArg ]


{-| fontStyle: String -> Svg.Styled.Attribute msg -}
fontStyle : String -> Elm.Expression
fontStyle fontStyleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontStyle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontStyleArg ]


{-| fontVariant: String -> Svg.Styled.Attribute msg -}
fontVariant : String -> Elm.Expression
fontVariant fontVariantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontVariant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontVariantArg ]


{-| fontWeight: String -> Svg.Styled.Attribute msg -}
fontWeight : String -> Elm.Expression
fontWeight fontWeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontWeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string fontWeightArg ]


{-| glyphOrientationHorizontal: String -> Svg.Styled.Attribute msg -}
glyphOrientationHorizontal : String -> Elm.Expression
glyphOrientationHorizontal glyphOrientationHorizontalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphOrientationHorizontal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string glyphOrientationHorizontalArg ]


{-| glyphOrientationVertical: String -> Svg.Styled.Attribute msg -}
glyphOrientationVertical : String -> Elm.Expression
glyphOrientationVertical glyphOrientationVerticalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphOrientationVertical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string glyphOrientationVerticalArg ]


{-| imageRendering: String -> Svg.Styled.Attribute msg -}
imageRendering : String -> Elm.Expression
imageRendering imageRenderingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "imageRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string imageRenderingArg ]


{-| kerning: String -> Svg.Styled.Attribute msg -}
kerning : String -> Elm.Expression
kerning kerningArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kerning"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string kerningArg ]


{-| letterSpacing: String -> Svg.Styled.Attribute msg -}
letterSpacing : String -> Elm.Expression
letterSpacing letterSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string letterSpacingArg ]


{-| lightingColor: String -> Svg.Styled.Attribute msg -}
lightingColor : String -> Elm.Expression
lightingColor lightingColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lightingColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string lightingColorArg ]


{-| markerEnd: String -> Svg.Styled.Attribute msg -}
markerEnd : String -> Elm.Expression
markerEnd markerEndArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerEnd"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerEndArg ]


{-| markerMid: String -> Svg.Styled.Attribute msg -}
markerMid : String -> Elm.Expression
markerMid markerMidArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerMid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerMidArg ]


{-| markerStart: String -> Svg.Styled.Attribute msg -}
markerStart : String -> Elm.Expression
markerStart markerStartArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerStart"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string markerStartArg ]


{-| mask: String -> Svg.Styled.Attribute msg -}
mask : String -> Elm.Expression
mask maskArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maskArg ]


{-| opacity: String -> Svg.Styled.Attribute msg -}
opacity : String -> Elm.Expression
opacity opacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string opacityArg ]


{-| overflow: String -> Svg.Styled.Attribute msg -}
overflow : String -> Elm.Expression
overflow overflowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overflow"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string overflowArg ]


{-| pointerEvents: String -> Svg.Styled.Attribute msg -}
pointerEvents : String -> Elm.Expression
pointerEvents pointerEventsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointerEvents"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string pointerEventsArg ]


{-| shapeRendering: String -> Svg.Styled.Attribute msg -}
shapeRendering : String -> Elm.Expression
shapeRendering shapeRenderingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "shapeRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string shapeRenderingArg ]


{-| stopColor: String -> Svg.Styled.Attribute msg -}
stopColor : String -> Elm.Expression
stopColor stopColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stopColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stopColorArg ]


{-| stopOpacity: String -> Svg.Styled.Attribute msg -}
stopOpacity : String -> Elm.Expression
stopOpacity stopOpacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stopOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stopOpacityArg ]


{-| strokeDasharray: String -> Svg.Styled.Attribute msg -}
strokeDasharray : String -> Elm.Expression
strokeDasharray strokeDasharrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeDasharray"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeDasharrayArg ]


{-| strokeDashoffset: String -> Svg.Styled.Attribute msg -}
strokeDashoffset : String -> Elm.Expression
strokeDashoffset strokeDashoffsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeDashoffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeDashoffsetArg ]


{-| strokeLinecap: String -> Svg.Styled.Attribute msg -}
strokeLinecap : String -> Elm.Expression
strokeLinecap strokeLinecapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeLinecap"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeLinecapArg ]


{-| strokeLinejoin: String -> Svg.Styled.Attribute msg -}
strokeLinejoin : String -> Elm.Expression
strokeLinejoin strokeLinejoinArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeLinejoin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeLinejoinArg ]


{-| strokeMiterlimit: String -> Svg.Styled.Attribute msg -}
strokeMiterlimit : String -> Elm.Expression
strokeMiterlimit strokeMiterlimitArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeMiterlimit"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeMiterlimitArg ]


{-| strokeOpacity: String -> Svg.Styled.Attribute msg -}
strokeOpacity : String -> Elm.Expression
strokeOpacity strokeOpacityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeOpacityArg ]


{-| strokeWidth: String -> Svg.Styled.Attribute msg -}
strokeWidth : String -> Elm.Expression
strokeWidth strokeWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeWidthArg ]


{-| stroke: String -> Svg.Styled.Attribute msg -}
stroke : String -> Elm.Expression
stroke strokeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stroke"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string strokeArg ]


{-| textAnchor: String -> Svg.Styled.Attribute msg -}
textAnchor : String -> Elm.Expression
textAnchor textAnchorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textAnchor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textAnchorArg ]


{-| textDecoration: String -> Svg.Styled.Attribute msg -}
textDecoration : String -> Elm.Expression
textDecoration textDecorationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textDecoration"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textDecorationArg ]


{-| textRendering: String -> Svg.Styled.Attribute msg -}
textRendering : String -> Elm.Expression
textRendering textRenderingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textRenderingArg ]


{-| unicodeBidi: String -> Svg.Styled.Attribute msg -}
unicodeBidi : String -> Elm.Expression
unicodeBidi unicodeBidiArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicodeBidi"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string unicodeBidiArg ]


{-| visibility: String -> Svg.Styled.Attribute msg -}
visibility : String -> Elm.Expression
visibility visibilityArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "visibility"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string visibilityArg ]


{-| wordSpacing: String -> Svg.Styled.Attribute msg -}
wordSpacing : String -> Elm.Expression
wordSpacing wordSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string wordSpacingArg ]


{-| writingMode: String -> Svg.Styled.Attribute msg -}
writingMode : String -> Elm.Expression
writingMode writingModeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "writingMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string writingModeArg ]


call_ :
    { css : Elm.Expression -> Elm.Expression
    , fromUnstyled : Elm.Expression -> Elm.Expression
    , accentHeight : Elm.Expression -> Elm.Expression
    , accelerate : Elm.Expression -> Elm.Expression
    , accumulate : Elm.Expression -> Elm.Expression
    , additive : Elm.Expression -> Elm.Expression
    , alphabetic : Elm.Expression -> Elm.Expression
    , allowReorder : Elm.Expression -> Elm.Expression
    , amplitude : Elm.Expression -> Elm.Expression
    , arabicForm : Elm.Expression -> Elm.Expression
    , ascent : Elm.Expression -> Elm.Expression
    , attributeName : Elm.Expression -> Elm.Expression
    , attributeType : Elm.Expression -> Elm.Expression
    , autoReverse : Elm.Expression -> Elm.Expression
    , azimuth : Elm.Expression -> Elm.Expression
    , baseFrequency : Elm.Expression -> Elm.Expression
    , baseProfile : Elm.Expression -> Elm.Expression
    , bbox : Elm.Expression -> Elm.Expression
    , begin : Elm.Expression -> Elm.Expression
    , bias : Elm.Expression -> Elm.Expression
    , by : Elm.Expression -> Elm.Expression
    , calcMode : Elm.Expression -> Elm.Expression
    , capHeight : Elm.Expression -> Elm.Expression
    , class : Elm.Expression -> Elm.Expression
    , clipPathUnits : Elm.Expression -> Elm.Expression
    , contentScriptType : Elm.Expression -> Elm.Expression
    , contentStyleType : Elm.Expression -> Elm.Expression
    , cx : Elm.Expression -> Elm.Expression
    , cy : Elm.Expression -> Elm.Expression
    , d : Elm.Expression -> Elm.Expression
    , decelerate : Elm.Expression -> Elm.Expression
    , descent : Elm.Expression -> Elm.Expression
    , diffuseConstant : Elm.Expression -> Elm.Expression
    , divisor : Elm.Expression -> Elm.Expression
    , dur : Elm.Expression -> Elm.Expression
    , dx : Elm.Expression -> Elm.Expression
    , dy : Elm.Expression -> Elm.Expression
    , edgeMode : Elm.Expression -> Elm.Expression
    , elevation : Elm.Expression -> Elm.Expression
    , end : Elm.Expression -> Elm.Expression
    , exponent : Elm.Expression -> Elm.Expression
    , externalResourcesRequired : Elm.Expression -> Elm.Expression
    , filterRes : Elm.Expression -> Elm.Expression
    , filterUnits : Elm.Expression -> Elm.Expression
    , format : Elm.Expression -> Elm.Expression
    , from : Elm.Expression -> Elm.Expression
    , fx : Elm.Expression -> Elm.Expression
    , fy : Elm.Expression -> Elm.Expression
    , g1 : Elm.Expression -> Elm.Expression
    , g2 : Elm.Expression -> Elm.Expression
    , glyphName : Elm.Expression -> Elm.Expression
    , glyphRef : Elm.Expression -> Elm.Expression
    , gradientTransform : Elm.Expression -> Elm.Expression
    , gradientUnits : Elm.Expression -> Elm.Expression
    , hanging : Elm.Expression -> Elm.Expression
    , height : Elm.Expression -> Elm.Expression
    , horizAdvX : Elm.Expression -> Elm.Expression
    , horizOriginX : Elm.Expression -> Elm.Expression
    , horizOriginY : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , ideographic : Elm.Expression -> Elm.Expression
    , in_ : Elm.Expression -> Elm.Expression
    , in2 : Elm.Expression -> Elm.Expression
    , intercept : Elm.Expression -> Elm.Expression
    , k : Elm.Expression -> Elm.Expression
    , k1 : Elm.Expression -> Elm.Expression
    , k2 : Elm.Expression -> Elm.Expression
    , k3 : Elm.Expression -> Elm.Expression
    , k4 : Elm.Expression -> Elm.Expression
    , kernelMatrix : Elm.Expression -> Elm.Expression
    , kernelUnitLength : Elm.Expression -> Elm.Expression
    , keyPoints : Elm.Expression -> Elm.Expression
    , keySplines : Elm.Expression -> Elm.Expression
    , keyTimes : Elm.Expression -> Elm.Expression
    , lang : Elm.Expression -> Elm.Expression
    , lengthAdjust : Elm.Expression -> Elm.Expression
    , limitingConeAngle : Elm.Expression -> Elm.Expression
    , local : Elm.Expression -> Elm.Expression
    , markerHeight : Elm.Expression -> Elm.Expression
    , markerUnits : Elm.Expression -> Elm.Expression
    , markerWidth : Elm.Expression -> Elm.Expression
    , maskContentUnits : Elm.Expression -> Elm.Expression
    , maskUnits : Elm.Expression -> Elm.Expression
    , mathematical : Elm.Expression -> Elm.Expression
    , max : Elm.Expression -> Elm.Expression
    , media : Elm.Expression -> Elm.Expression
    , method : Elm.Expression -> Elm.Expression
    , min : Elm.Expression -> Elm.Expression
    , mode : Elm.Expression -> Elm.Expression
    , name : Elm.Expression -> Elm.Expression
    , numOctaves : Elm.Expression -> Elm.Expression
    , offset : Elm.Expression -> Elm.Expression
    , operator : Elm.Expression -> Elm.Expression
    , order : Elm.Expression -> Elm.Expression
    , orient : Elm.Expression -> Elm.Expression
    , orientation : Elm.Expression -> Elm.Expression
    , origin : Elm.Expression -> Elm.Expression
    , overlinePosition : Elm.Expression -> Elm.Expression
    , overlineThickness : Elm.Expression -> Elm.Expression
    , panose1 : Elm.Expression -> Elm.Expression
    , path : Elm.Expression -> Elm.Expression
    , pathLength : Elm.Expression -> Elm.Expression
    , patternContentUnits : Elm.Expression -> Elm.Expression
    , patternTransform : Elm.Expression -> Elm.Expression
    , patternUnits : Elm.Expression -> Elm.Expression
    , pointOrder : Elm.Expression -> Elm.Expression
    , points : Elm.Expression -> Elm.Expression
    , pointsAtX : Elm.Expression -> Elm.Expression
    , pointsAtY : Elm.Expression -> Elm.Expression
    , pointsAtZ : Elm.Expression -> Elm.Expression
    , preserveAlpha : Elm.Expression -> Elm.Expression
    , preserveAspectRatio : Elm.Expression -> Elm.Expression
    , primitiveUnits : Elm.Expression -> Elm.Expression
    , r : Elm.Expression -> Elm.Expression
    , radius : Elm.Expression -> Elm.Expression
    , refX : Elm.Expression -> Elm.Expression
    , refY : Elm.Expression -> Elm.Expression
    , renderingIntent : Elm.Expression -> Elm.Expression
    , repeatCount : Elm.Expression -> Elm.Expression
    , repeatDur : Elm.Expression -> Elm.Expression
    , requiredExtensions : Elm.Expression -> Elm.Expression
    , requiredFeatures : Elm.Expression -> Elm.Expression
    , restart : Elm.Expression -> Elm.Expression
    , result : Elm.Expression -> Elm.Expression
    , rotate : Elm.Expression -> Elm.Expression
    , rx : Elm.Expression -> Elm.Expression
    , ry : Elm.Expression -> Elm.Expression
    , scale : Elm.Expression -> Elm.Expression
    , seed : Elm.Expression -> Elm.Expression
    , slope : Elm.Expression -> Elm.Expression
    , spacing : Elm.Expression -> Elm.Expression
    , specularConstant : Elm.Expression -> Elm.Expression
    , specularExponent : Elm.Expression -> Elm.Expression
    , speed : Elm.Expression -> Elm.Expression
    , spreadMethod : Elm.Expression -> Elm.Expression
    , startOffset : Elm.Expression -> Elm.Expression
    , stdDeviation : Elm.Expression -> Elm.Expression
    , stemh : Elm.Expression -> Elm.Expression
    , stemv : Elm.Expression -> Elm.Expression
    , stitchTiles : Elm.Expression -> Elm.Expression
    , strikethroughPosition : Elm.Expression -> Elm.Expression
    , strikethroughThickness : Elm.Expression -> Elm.Expression
    , string : Elm.Expression -> Elm.Expression
    , style : Elm.Expression -> Elm.Expression
    , surfaceScale : Elm.Expression -> Elm.Expression
    , systemLanguage : Elm.Expression -> Elm.Expression
    , tableValues : Elm.Expression -> Elm.Expression
    , target : Elm.Expression -> Elm.Expression
    , targetX : Elm.Expression -> Elm.Expression
    , targetY : Elm.Expression -> Elm.Expression
    , textLength : Elm.Expression -> Elm.Expression
    , title : Elm.Expression -> Elm.Expression
    , to : Elm.Expression -> Elm.Expression
    , transform : Elm.Expression -> Elm.Expression
    , type_ : Elm.Expression -> Elm.Expression
    , u1 : Elm.Expression -> Elm.Expression
    , u2 : Elm.Expression -> Elm.Expression
    , underlinePosition : Elm.Expression -> Elm.Expression
    , underlineThickness : Elm.Expression -> Elm.Expression
    , unicode : Elm.Expression -> Elm.Expression
    , unicodeRange : Elm.Expression -> Elm.Expression
    , unitsPerEm : Elm.Expression -> Elm.Expression
    , vAlphabetic : Elm.Expression -> Elm.Expression
    , vHanging : Elm.Expression -> Elm.Expression
    , vIdeographic : Elm.Expression -> Elm.Expression
    , vMathematical : Elm.Expression -> Elm.Expression
    , values : Elm.Expression -> Elm.Expression
    , version : Elm.Expression -> Elm.Expression
    , vertAdvY : Elm.Expression -> Elm.Expression
    , vertOriginX : Elm.Expression -> Elm.Expression
    , vertOriginY : Elm.Expression -> Elm.Expression
    , viewBox : Elm.Expression -> Elm.Expression
    , viewTarget : Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , widths : Elm.Expression -> Elm.Expression
    , x : Elm.Expression -> Elm.Expression
    , xHeight : Elm.Expression -> Elm.Expression
    , x1 : Elm.Expression -> Elm.Expression
    , x2 : Elm.Expression -> Elm.Expression
    , xChannelSelector : Elm.Expression -> Elm.Expression
    , xlinkActuate : Elm.Expression -> Elm.Expression
    , xlinkArcrole : Elm.Expression -> Elm.Expression
    , xlinkHref : Elm.Expression -> Elm.Expression
    , xlinkRole : Elm.Expression -> Elm.Expression
    , xlinkShow : Elm.Expression -> Elm.Expression
    , xlinkTitle : Elm.Expression -> Elm.Expression
    , xlinkType : Elm.Expression -> Elm.Expression
    , xmlBase : Elm.Expression -> Elm.Expression
    , xmlLang : Elm.Expression -> Elm.Expression
    , xmlSpace : Elm.Expression -> Elm.Expression
    , y : Elm.Expression -> Elm.Expression
    , y1 : Elm.Expression -> Elm.Expression
    , y2 : Elm.Expression -> Elm.Expression
    , yChannelSelector : Elm.Expression -> Elm.Expression
    , z : Elm.Expression -> Elm.Expression
    , zoomAndPan : Elm.Expression -> Elm.Expression
    , alignmentBaseline : Elm.Expression -> Elm.Expression
    , baselineShift : Elm.Expression -> Elm.Expression
    , clipPath : Elm.Expression -> Elm.Expression
    , clipRule : Elm.Expression -> Elm.Expression
    , clip : Elm.Expression -> Elm.Expression
    , colorInterpolationFilters : Elm.Expression -> Elm.Expression
    , colorInterpolation : Elm.Expression -> Elm.Expression
    , colorProfile : Elm.Expression -> Elm.Expression
    , colorRendering : Elm.Expression -> Elm.Expression
    , color : Elm.Expression -> Elm.Expression
    , cursor : Elm.Expression -> Elm.Expression
    , direction : Elm.Expression -> Elm.Expression
    , display : Elm.Expression -> Elm.Expression
    , dominantBaseline : Elm.Expression -> Elm.Expression
    , enableBackground : Elm.Expression -> Elm.Expression
    , fillOpacity : Elm.Expression -> Elm.Expression
    , fillRule : Elm.Expression -> Elm.Expression
    , fill : Elm.Expression -> Elm.Expression
    , filter : Elm.Expression -> Elm.Expression
    , floodColor : Elm.Expression -> Elm.Expression
    , floodOpacity : Elm.Expression -> Elm.Expression
    , fontFamily : Elm.Expression -> Elm.Expression
    , fontSizeAdjust : Elm.Expression -> Elm.Expression
    , fontSize : Elm.Expression -> Elm.Expression
    , fontStretch : Elm.Expression -> Elm.Expression
    , fontStyle : Elm.Expression -> Elm.Expression
    , fontVariant : Elm.Expression -> Elm.Expression
    , fontWeight : Elm.Expression -> Elm.Expression
    , glyphOrientationHorizontal : Elm.Expression -> Elm.Expression
    , glyphOrientationVertical : Elm.Expression -> Elm.Expression
    , imageRendering : Elm.Expression -> Elm.Expression
    , kerning : Elm.Expression -> Elm.Expression
    , letterSpacing : Elm.Expression -> Elm.Expression
    , lightingColor : Elm.Expression -> Elm.Expression
    , markerEnd : Elm.Expression -> Elm.Expression
    , markerMid : Elm.Expression -> Elm.Expression
    , markerStart : Elm.Expression -> Elm.Expression
    , mask : Elm.Expression -> Elm.Expression
    , opacity : Elm.Expression -> Elm.Expression
    , overflow : Elm.Expression -> Elm.Expression
    , pointerEvents : Elm.Expression -> Elm.Expression
    , shapeRendering : Elm.Expression -> Elm.Expression
    , stopColor : Elm.Expression -> Elm.Expression
    , stopOpacity : Elm.Expression -> Elm.Expression
    , strokeDasharray : Elm.Expression -> Elm.Expression
    , strokeDashoffset : Elm.Expression -> Elm.Expression
    , strokeLinecap : Elm.Expression -> Elm.Expression
    , strokeLinejoin : Elm.Expression -> Elm.Expression
    , strokeMiterlimit : Elm.Expression -> Elm.Expression
    , strokeOpacity : Elm.Expression -> Elm.Expression
    , strokeWidth : Elm.Expression -> Elm.Expression
    , stroke : Elm.Expression -> Elm.Expression
    , textAnchor : Elm.Expression -> Elm.Expression
    , textDecoration : Elm.Expression -> Elm.Expression
    , textRendering : Elm.Expression -> Elm.Expression
    , unicodeBidi : Elm.Expression -> Elm.Expression
    , visibility : Elm.Expression -> Elm.Expression
    , wordSpacing : Elm.Expression -> Elm.Expression
    , writingMode : Elm.Expression -> Elm.Expression
    }
call_ =
    { css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , fromUnstyled =
        \fromUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fromUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "VirtualDom" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromUnstyledArg ]
    , accentHeight =
        \accentHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "accentHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ accentHeightArg ]
    , accelerate =
        \accelerateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "accelerate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ accelerateArg ]
    , accumulate =
        \accumulateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "accumulate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ accumulateArg ]
    , additive =
        \additiveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "additive"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ additiveArg ]
    , alphabetic =
        \alphabeticArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "alphabetic"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alphabeticArg ]
    , allowReorder =
        \allowReorderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "allowReorder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ allowReorderArg ]
    , amplitude =
        \amplitudeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "amplitude"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ amplitudeArg ]
    , arabicForm =
        \arabicFormArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "arabicForm"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ arabicFormArg ]
    , ascent =
        \ascentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "ascent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ascentArg ]
    , attributeName =
        \attributeNameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "attributeName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ attributeNameArg ]
    , attributeType =
        \attributeTypeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "attributeType"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ attributeTypeArg ]
    , autoReverse =
        \autoReverseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "autoReverse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ autoReverseArg ]
    , azimuth =
        \azimuthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "azimuth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ azimuthArg ]
    , baseFrequency =
        \baseFrequencyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "baseFrequency"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ baseFrequencyArg ]
    , baseProfile =
        \baseProfileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "baseProfile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ baseProfileArg ]
    , bbox =
        \bboxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "bbox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ bboxArg ]
    , begin =
        \beginArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "begin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ beginArg ]
    , bias =
        \biasArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "bias"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ biasArg ]
    , by =
        \byArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "by"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ byArg ]
    , calcMode =
        \calcModeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "calcMode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ calcModeArg ]
    , capHeight =
        \capHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "capHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ capHeightArg ]
    , class =
        \classArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "class"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ classArg ]
    , clipPathUnits =
        \clipPathUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "clipPathUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ clipPathUnitsArg ]
    , contentScriptType =
        \contentScriptTypeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "contentScriptType"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ contentScriptTypeArg ]
    , contentStyleType =
        \contentStyleTypeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "contentStyleType"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ contentStyleTypeArg ]
    , cx =
        \cxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "cx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cxArg ]
    , cy =
        \cyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "cy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cyArg ]
    , d =
        \dArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "d"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dArg ]
    , decelerate =
        \decelerateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "decelerate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ decelerateArg ]
    , descent =
        \descentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "descent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ descentArg ]
    , diffuseConstant =
        \diffuseConstantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "diffuseConstant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ diffuseConstantArg ]
    , divisor =
        \divisorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "divisor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ divisorArg ]
    , dur =
        \durArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "dur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ durArg ]
    , dx =
        \dxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "dx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dxArg ]
    , dy =
        \dyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "dy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dyArg ]
    , edgeMode =
        \edgeModeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "edgeMode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ edgeModeArg ]
    , elevation =
        \elevationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "elevation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ elevationArg ]
    , end =
        \endArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "end"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ endArg ]
    , exponent =
        \exponentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "exponent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ exponentArg ]
    , externalResourcesRequired =
        \externalResourcesRequiredArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "externalResourcesRequired"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ externalResourcesRequiredArg ]
    , filterRes =
        \filterResArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "filterRes"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ filterResArg ]
    , filterUnits =
        \filterUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "filterUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ filterUnitsArg ]
    , format =
        \formatArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "format"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ formatArg ]
    , from =
        \fromArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "from"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromArg ]
    , fx =
        \fxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fxArg ]
    , fy =
        \fyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fyArg ]
    , g1 =
        \g1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "g1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ g1Arg ]
    , g2 =
        \g2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "g2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ g2Arg ]
    , glyphName =
        \glyphNameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "glyphName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphNameArg ]
    , glyphRef =
        \glyphRefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "glyphRef"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphRefArg ]
    , gradientTransform =
        \gradientTransformArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "gradientTransform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ gradientTransformArg ]
    , gradientUnits =
        \gradientUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "gradientUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ gradientUnitsArg ]
    , hanging =
        \hangingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "hanging"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hangingArg ]
    , height =
        \heightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "height"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ heightArg ]
    , horizAdvX =
        \horizAdvXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "horizAdvX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ horizAdvXArg ]
    , horizOriginX =
        \horizOriginXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "horizOriginX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ horizOriginXArg ]
    , horizOriginY =
        \horizOriginYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "horizOriginY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ horizOriginYArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , ideographic =
        \ideographicArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "ideographic"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ideographicArg ]
    , in_ =
        \in_Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "in_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ in_Arg ]
    , in2 =
        \in2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "in2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ in2Arg ]
    , intercept =
        \interceptArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "intercept"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ interceptArg ]
    , k =
        \kArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "k"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ kArg ]
    , k1 =
        \k1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "k1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ k1Arg ]
    , k2 =
        \k2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "k2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ k2Arg ]
    , k3 =
        \k3Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "k3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ k3Arg ]
    , k4 =
        \k4Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "k4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ k4Arg ]
    , kernelMatrix =
        \kernelMatrixArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "kernelMatrix"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ kernelMatrixArg ]
    , kernelUnitLength =
        \kernelUnitLengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "kernelUnitLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ kernelUnitLengthArg ]
    , keyPoints =
        \keyPointsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "keyPoints"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ keyPointsArg ]
    , keySplines =
        \keySplinesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "keySplines"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ keySplinesArg ]
    , keyTimes =
        \keyTimesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "keyTimes"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ keyTimesArg ]
    , lang =
        \langArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "lang"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ langArg ]
    , lengthAdjust =
        \lengthAdjustArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "lengthAdjust"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lengthAdjustArg ]
    , limitingConeAngle =
        \limitingConeAngleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "limitingConeAngle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ limitingConeAngleArg ]
    , local =
        \localArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "local"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ localArg ]
    , markerHeight =
        \markerHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerHeightArg ]
    , markerUnits =
        \markerUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerUnitsArg ]
    , markerWidth =
        \markerWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerWidthArg ]
    , maskContentUnits =
        \maskContentUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "maskContentUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maskContentUnitsArg ]
    , maskUnits =
        \maskUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "maskUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maskUnitsArg ]
    , mathematical =
        \mathematicalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "mathematical"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mathematicalArg ]
    , max =
        \maxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "max"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maxArg ]
    , media =
        \mediaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "media"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mediaArg ]
    , method =
        \methodArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "method"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ methodArg ]
    , min =
        \minArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "min"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ minArg ]
    , mode =
        \modeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "mode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ modeArg ]
    , name =
        \nameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "name"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nameArg ]
    , numOctaves =
        \numOctavesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "numOctaves"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ numOctavesArg ]
    , offset =
        \offsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "offset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ offsetArg ]
    , operator =
        \operatorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "operator"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ operatorArg ]
    , order =
        \orderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "order"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ orderArg ]
    , orient =
        \orientArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "orient"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ orientArg ]
    , orientation =
        \orientationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "orientation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ orientationArg ]
    , origin =
        \originArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "origin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ originArg ]
    , overlinePosition =
        \overlinePositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "overlinePosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ overlinePositionArg ]
    , overlineThickness =
        \overlineThicknessArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "overlineThickness"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ overlineThicknessArg ]
    , panose1 =
        \panose1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "panose1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ panose1Arg ]
    , path =
        \pathArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "path"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pathArg ]
    , pathLength =
        \pathLengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pathLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pathLengthArg ]
    , patternContentUnits =
        \patternContentUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "patternContentUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ patternContentUnitsArg ]
    , patternTransform =
        \patternTransformArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "patternTransform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ patternTransformArg ]
    , patternUnits =
        \patternUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "patternUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ patternUnitsArg ]
    , pointOrder =
        \pointOrderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pointOrder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointOrderArg ]
    , points =
        \pointsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "points"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointsArg ]
    , pointsAtX =
        \pointsAtXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pointsAtX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointsAtXArg ]
    , pointsAtY =
        \pointsAtYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pointsAtY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointsAtYArg ]
    , pointsAtZ =
        \pointsAtZArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pointsAtZ"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointsAtZArg ]
    , preserveAlpha =
        \preserveAlphaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "preserveAlpha"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ preserveAlphaArg ]
    , preserveAspectRatio =
        \preserveAspectRatioArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "preserveAspectRatio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ preserveAspectRatioArg ]
    , primitiveUnits =
        \primitiveUnitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "primitiveUnits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ primitiveUnitsArg ]
    , r =
        \rArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "r"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rArg ]
    , radius =
        \radiusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "radius"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ radiusArg ]
    , refX =
        \refXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "refX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ refXArg ]
    , refY =
        \refYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "refY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ refYArg ]
    , renderingIntent =
        \renderingIntentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "renderingIntent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ renderingIntentArg ]
    , repeatCount =
        \repeatCountArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "repeatCount"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ repeatCountArg ]
    , repeatDur =
        \repeatDurArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "repeatDur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ repeatDurArg ]
    , requiredExtensions =
        \requiredExtensionsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "requiredExtensions"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ requiredExtensionsArg ]
    , requiredFeatures =
        \requiredFeaturesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "requiredFeatures"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ requiredFeaturesArg ]
    , restart =
        \restartArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "restart"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ restartArg ]
    , result =
        \resultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "result"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ resultArg ]
    , rotate =
        \rotateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "rotate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rotateArg ]
    , rx =
        \rxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "rx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rxArg ]
    , ry =
        \ryArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "ry"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ryArg ]
    , scale =
        \scaleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "scale"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ scaleArg ]
    , seed =
        \seedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "seed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ seedArg ]
    , slope =
        \slopeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "slope"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ slopeArg ]
    , spacing =
        \spacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "spacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spacingArg ]
    , specularConstant =
        \specularConstantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "specularConstant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ specularConstantArg ]
    , specularExponent =
        \specularExponentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "specularExponent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ specularExponentArg ]
    , speed =
        \speedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "speed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ speedArg ]
    , spreadMethod =
        \spreadMethodArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "spreadMethod"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spreadMethodArg ]
    , startOffset =
        \startOffsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "startOffset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ startOffsetArg ]
    , stdDeviation =
        \stdDeviationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stdDeviation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stdDeviationArg ]
    , stemh =
        \stemhArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stemh"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stemhArg ]
    , stemv =
        \stemvArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stemv"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stemvArg ]
    , stitchTiles =
        \stitchTilesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stitchTiles"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stitchTilesArg ]
    , strikethroughPosition =
        \strikethroughPositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strikethroughPosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strikethroughPositionArg ]
    , strikethroughThickness =
        \strikethroughThicknessArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strikethroughThickness"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strikethroughThicknessArg ]
    , string =
        \stringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "string"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stringArg ]
    , style =
        \styleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "style"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styleArg ]
    , surfaceScale =
        \surfaceScaleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "surfaceScale"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ surfaceScaleArg ]
    , systemLanguage =
        \systemLanguageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "systemLanguage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ systemLanguageArg ]
    , tableValues =
        \tableValuesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "tableValues"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tableValuesArg ]
    , target =
        \targetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "target"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ targetArg ]
    , targetX =
        \targetXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "targetX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ targetXArg ]
    , targetY =
        \targetYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "targetY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ targetYArg ]
    , textLength =
        \textLengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "textLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textLengthArg ]
    , title =
        \titleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "title"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ titleArg ]
    , to =
        \toArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "to"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toArg ]
    , transform =
        \transformArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "transform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ transformArg ]
    , type_ =
        \type_Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "type_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ type_Arg ]
    , u1 =
        \u1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "u1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ u1Arg ]
    , u2 =
        \u2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "u2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ u2Arg ]
    , underlinePosition =
        \underlinePositionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "underlinePosition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ underlinePositionArg ]
    , underlineThickness =
        \underlineThicknessArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "underlineThickness"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ underlineThicknessArg ]
    , unicode =
        \unicodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "unicode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ unicodeArg ]
    , unicodeRange =
        \unicodeRangeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "unicodeRange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ unicodeRangeArg ]
    , unitsPerEm =
        \unitsPerEmArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "unitsPerEm"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ unitsPerEmArg ]
    , vAlphabetic =
        \vAlphabeticArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vAlphabetic"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vAlphabeticArg ]
    , vHanging =
        \vHangingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vHanging"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vHangingArg ]
    , vIdeographic =
        \vIdeographicArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vIdeographic"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vIdeographicArg ]
    , vMathematical =
        \vMathematicalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vMathematical"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vMathematicalArg ]
    , values =
        \valuesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "values"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valuesArg ]
    , version =
        \versionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "version"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ versionArg ]
    , vertAdvY =
        \vertAdvYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vertAdvY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vertAdvYArg ]
    , vertOriginX =
        \vertOriginXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vertOriginX"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vertOriginXArg ]
    , vertOriginY =
        \vertOriginYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "vertOriginY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ vertOriginYArg ]
    , viewBox =
        \viewBoxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "viewBox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewBoxArg ]
    , viewTarget =
        \viewTargetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "viewTarget"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewTargetArg ]
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "width"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ widthArg ]
    , widths =
        \widthsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "widths"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ widthsArg ]
    , x =
        \xArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "x"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xArg ]
    , xHeight =
        \xHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xHeightArg ]
    , x1 =
        \x1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "x1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ x1Arg ]
    , x2 =
        \x2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "x2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ x2Arg ]
    , xChannelSelector =
        \xChannelSelectorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xChannelSelector"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xChannelSelectorArg ]
    , xlinkActuate =
        \xlinkActuateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkActuate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkActuateArg ]
    , xlinkArcrole =
        \xlinkArcroleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkArcrole"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkArcroleArg ]
    , xlinkHref =
        \xlinkHrefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkHref"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkHrefArg ]
    , xlinkRole =
        \xlinkRoleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkRole"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkRoleArg ]
    , xlinkShow =
        \xlinkShowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkShow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkShowArg ]
    , xlinkTitle =
        \xlinkTitleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkTitle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkTitleArg ]
    , xlinkType =
        \xlinkTypeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xlinkType"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xlinkTypeArg ]
    , xmlBase =
        \xmlBaseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xmlBase"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xmlBaseArg ]
    , xmlLang =
        \xmlLangArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xmlLang"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xmlLangArg ]
    , xmlSpace =
        \xmlSpaceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "xmlSpace"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ xmlSpaceArg ]
    , y =
        \yArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "y"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ yArg ]
    , y1 =
        \y1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "y1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ y1Arg ]
    , y2 =
        \y2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "y2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ y2Arg ]
    , yChannelSelector =
        \yChannelSelectorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "yChannelSelector"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ yChannelSelectorArg ]
    , z =
        \zArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "z"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ zArg ]
    , zoomAndPan =
        \zoomAndPanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "zoomAndPan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ zoomAndPanArg ]
    , alignmentBaseline =
        \alignmentBaselineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "alignmentBaseline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alignmentBaselineArg ]
    , baselineShift =
        \baselineShiftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "baselineShift"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ baselineShiftArg ]
    , clipPath =
        \clipPathArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "clipPath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ clipPathArg ]
    , clipRule =
        \clipRuleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "clipRule"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ clipRuleArg ]
    , clip =
        \clipArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "clip"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ clipArg ]
    , colorInterpolationFilters =
        \colorInterpolationFiltersArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "colorInterpolationFilters"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorInterpolationFiltersArg ]
    , colorInterpolation =
        \colorInterpolationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "colorInterpolation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorInterpolationArg ]
    , colorProfile =
        \colorProfileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "colorProfile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorProfileArg ]
    , colorRendering =
        \colorRenderingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "colorRendering"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorRenderingArg ]
    , color =
        \colorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "color"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorArg ]
    , cursor =
        \cursorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "cursor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cursorArg ]
    , direction =
        \directionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "direction"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ directionArg ]
    , display =
        \displayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "display"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ displayArg ]
    , dominantBaseline =
        \dominantBaselineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "dominantBaseline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dominantBaselineArg ]
    , enableBackground =
        \enableBackgroundArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "enableBackground"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ enableBackgroundArg ]
    , fillOpacity =
        \fillOpacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fillOpacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fillOpacityArg ]
    , fillRule =
        \fillRuleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fillRule"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fillRuleArg ]
    , fill =
        \fillArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fill"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fillArg ]
    , filter =
        \filterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "filter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ filterArg ]
    , floodColor =
        \floodColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "floodColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ floodColorArg ]
    , floodOpacity =
        \floodOpacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "floodOpacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ floodOpacityArg ]
    , fontFamily =
        \fontFamilyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontFamily"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontFamilyArg ]
    , fontSizeAdjust =
        \fontSizeAdjustArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontSizeAdjust"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontSizeAdjustArg ]
    , fontSize =
        \fontSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontSizeArg ]
    , fontStretch =
        \fontStretchArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontStretch"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontStretchArg ]
    , fontStyle =
        \fontStyleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontStyle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontStyleArg ]
    , fontVariant =
        \fontVariantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontVariant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontVariantArg ]
    , fontWeight =
        \fontWeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "fontWeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontWeightArg ]
    , glyphOrientationHorizontal =
        \glyphOrientationHorizontalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "glyphOrientationHorizontal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphOrientationHorizontalArg ]
    , glyphOrientationVertical =
        \glyphOrientationVerticalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "glyphOrientationVertical"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphOrientationVerticalArg ]
    , imageRendering =
        \imageRenderingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "imageRendering"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ imageRenderingArg ]
    , kerning =
        \kerningArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "kerning"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ kerningArg ]
    , letterSpacing =
        \letterSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "letterSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ letterSpacingArg ]
    , lightingColor =
        \lightingColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "lightingColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lightingColorArg ]
    , markerEnd =
        \markerEndArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerEnd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerEndArg ]
    , markerMid =
        \markerMidArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerMid"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerMidArg ]
    , markerStart =
        \markerStartArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "markerStart"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerStartArg ]
    , mask =
        \maskArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "mask"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maskArg ]
    , opacity =
        \opacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "opacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ opacityArg ]
    , overflow =
        \overflowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "overflow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ overflowArg ]
    , pointerEvents =
        \pointerEventsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "pointerEvents"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pointerEventsArg ]
    , shapeRendering =
        \shapeRenderingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "shapeRendering"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ shapeRenderingArg ]
    , stopColor =
        \stopColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stopColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stopColorArg ]
    , stopOpacity =
        \stopOpacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stopOpacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stopOpacityArg ]
    , strokeDasharray =
        \strokeDasharrayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeDasharray"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeDasharrayArg ]
    , strokeDashoffset =
        \strokeDashoffsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeDashoffset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeDashoffsetArg ]
    , strokeLinecap =
        \strokeLinecapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeLinecap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeLinecapArg ]
    , strokeLinejoin =
        \strokeLinejoinArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeLinejoin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeLinejoinArg ]
    , strokeMiterlimit =
        \strokeMiterlimitArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeMiterlimit"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeMiterlimitArg ]
    , strokeOpacity =
        \strokeOpacityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeOpacity"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeOpacityArg ]
    , strokeWidth =
        \strokeWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "strokeWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeWidthArg ]
    , stroke =
        \strokeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "stroke"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strokeArg ]
    , textAnchor =
        \textAnchorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "textAnchor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textAnchorArg ]
    , textDecoration =
        \textDecorationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "textDecoration"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textDecorationArg ]
    , textRendering =
        \textRenderingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "textRendering"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textRenderingArg ]
    , unicodeBidi =
        \unicodeBidiArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "unicodeBidi"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ unicodeBidiArg ]
    , visibility =
        \visibilityArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "visibility"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ visibilityArg ]
    , wordSpacing =
        \wordSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "wordSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ wordSpacingArg ]
    , writingMode =
        \writingModeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled", "Attributes" ]
                    , name = "writingMode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ writingModeArg ]
    }


values_ :
    { css : Elm.Expression
    , fromUnstyled : Elm.Expression
    , accentHeight : Elm.Expression
    , accelerate : Elm.Expression
    , accumulate : Elm.Expression
    , additive : Elm.Expression
    , alphabetic : Elm.Expression
    , allowReorder : Elm.Expression
    , amplitude : Elm.Expression
    , arabicForm : Elm.Expression
    , ascent : Elm.Expression
    , attributeName : Elm.Expression
    , attributeType : Elm.Expression
    , autoReverse : Elm.Expression
    , azimuth : Elm.Expression
    , baseFrequency : Elm.Expression
    , baseProfile : Elm.Expression
    , bbox : Elm.Expression
    , begin : Elm.Expression
    , bias : Elm.Expression
    , by : Elm.Expression
    , calcMode : Elm.Expression
    , capHeight : Elm.Expression
    , class : Elm.Expression
    , clipPathUnits : Elm.Expression
    , contentScriptType : Elm.Expression
    , contentStyleType : Elm.Expression
    , cx : Elm.Expression
    , cy : Elm.Expression
    , d : Elm.Expression
    , decelerate : Elm.Expression
    , descent : Elm.Expression
    , diffuseConstant : Elm.Expression
    , divisor : Elm.Expression
    , dur : Elm.Expression
    , dx : Elm.Expression
    , dy : Elm.Expression
    , edgeMode : Elm.Expression
    , elevation : Elm.Expression
    , end : Elm.Expression
    , exponent : Elm.Expression
    , externalResourcesRequired : Elm.Expression
    , filterRes : Elm.Expression
    , filterUnits : Elm.Expression
    , format : Elm.Expression
    , from : Elm.Expression
    , fx : Elm.Expression
    , fy : Elm.Expression
    , g1 : Elm.Expression
    , g2 : Elm.Expression
    , glyphName : Elm.Expression
    , glyphRef : Elm.Expression
    , gradientTransform : Elm.Expression
    , gradientUnits : Elm.Expression
    , hanging : Elm.Expression
    , height : Elm.Expression
    , horizAdvX : Elm.Expression
    , horizOriginX : Elm.Expression
    , horizOriginY : Elm.Expression
    , id : Elm.Expression
    , ideographic : Elm.Expression
    , in_ : Elm.Expression
    , in2 : Elm.Expression
    , intercept : Elm.Expression
    , k : Elm.Expression
    , k1 : Elm.Expression
    , k2 : Elm.Expression
    , k3 : Elm.Expression
    , k4 : Elm.Expression
    , kernelMatrix : Elm.Expression
    , kernelUnitLength : Elm.Expression
    , keyPoints : Elm.Expression
    , keySplines : Elm.Expression
    , keyTimes : Elm.Expression
    , lang : Elm.Expression
    , lengthAdjust : Elm.Expression
    , limitingConeAngle : Elm.Expression
    , local : Elm.Expression
    , markerHeight : Elm.Expression
    , markerUnits : Elm.Expression
    , markerWidth : Elm.Expression
    , maskContentUnits : Elm.Expression
    , maskUnits : Elm.Expression
    , mathematical : Elm.Expression
    , max : Elm.Expression
    , media : Elm.Expression
    , method : Elm.Expression
    , min : Elm.Expression
    , mode : Elm.Expression
    , name : Elm.Expression
    , numOctaves : Elm.Expression
    , offset : Elm.Expression
    , operator : Elm.Expression
    , order : Elm.Expression
    , orient : Elm.Expression
    , orientation : Elm.Expression
    , origin : Elm.Expression
    , overlinePosition : Elm.Expression
    , overlineThickness : Elm.Expression
    , panose1 : Elm.Expression
    , path : Elm.Expression
    , pathLength : Elm.Expression
    , patternContentUnits : Elm.Expression
    , patternTransform : Elm.Expression
    , patternUnits : Elm.Expression
    , pointOrder : Elm.Expression
    , points : Elm.Expression
    , pointsAtX : Elm.Expression
    , pointsAtY : Elm.Expression
    , pointsAtZ : Elm.Expression
    , preserveAlpha : Elm.Expression
    , preserveAspectRatio : Elm.Expression
    , primitiveUnits : Elm.Expression
    , r : Elm.Expression
    , radius : Elm.Expression
    , refX : Elm.Expression
    , refY : Elm.Expression
    , renderingIntent : Elm.Expression
    , repeatCount : Elm.Expression
    , repeatDur : Elm.Expression
    , requiredExtensions : Elm.Expression
    , requiredFeatures : Elm.Expression
    , restart : Elm.Expression
    , result : Elm.Expression
    , rotate : Elm.Expression
    , rx : Elm.Expression
    , ry : Elm.Expression
    , scale : Elm.Expression
    , seed : Elm.Expression
    , slope : Elm.Expression
    , spacing : Elm.Expression
    , specularConstant : Elm.Expression
    , specularExponent : Elm.Expression
    , speed : Elm.Expression
    , spreadMethod : Elm.Expression
    , startOffset : Elm.Expression
    , stdDeviation : Elm.Expression
    , stemh : Elm.Expression
    , stemv : Elm.Expression
    , stitchTiles : Elm.Expression
    , strikethroughPosition : Elm.Expression
    , strikethroughThickness : Elm.Expression
    , string : Elm.Expression
    , style : Elm.Expression
    , surfaceScale : Elm.Expression
    , systemLanguage : Elm.Expression
    , tableValues : Elm.Expression
    , target : Elm.Expression
    , targetX : Elm.Expression
    , targetY : Elm.Expression
    , textLength : Elm.Expression
    , title : Elm.Expression
    , to : Elm.Expression
    , transform : Elm.Expression
    , type_ : Elm.Expression
    , u1 : Elm.Expression
    , u2 : Elm.Expression
    , underlinePosition : Elm.Expression
    , underlineThickness : Elm.Expression
    , unicode : Elm.Expression
    , unicodeRange : Elm.Expression
    , unitsPerEm : Elm.Expression
    , vAlphabetic : Elm.Expression
    , vHanging : Elm.Expression
    , vIdeographic : Elm.Expression
    , vMathematical : Elm.Expression
    , values : Elm.Expression
    , version : Elm.Expression
    , vertAdvY : Elm.Expression
    , vertOriginX : Elm.Expression
    , vertOriginY : Elm.Expression
    , viewBox : Elm.Expression
    , viewTarget : Elm.Expression
    , width : Elm.Expression
    , widths : Elm.Expression
    , x : Elm.Expression
    , xHeight : Elm.Expression
    , x1 : Elm.Expression
    , x2 : Elm.Expression
    , xChannelSelector : Elm.Expression
    , xlinkActuate : Elm.Expression
    , xlinkArcrole : Elm.Expression
    , xlinkHref : Elm.Expression
    , xlinkRole : Elm.Expression
    , xlinkShow : Elm.Expression
    , xlinkTitle : Elm.Expression
    , xlinkType : Elm.Expression
    , xmlBase : Elm.Expression
    , xmlLang : Elm.Expression
    , xmlSpace : Elm.Expression
    , y : Elm.Expression
    , y1 : Elm.Expression
    , y2 : Elm.Expression
    , yChannelSelector : Elm.Expression
    , z : Elm.Expression
    , zoomAndPan : Elm.Expression
    , alignmentBaseline : Elm.Expression
    , baselineShift : Elm.Expression
    , clipPath : Elm.Expression
    , clipRule : Elm.Expression
    , clip : Elm.Expression
    , colorInterpolationFilters : Elm.Expression
    , colorInterpolation : Elm.Expression
    , colorProfile : Elm.Expression
    , colorRendering : Elm.Expression
    , color : Elm.Expression
    , cursor : Elm.Expression
    , direction : Elm.Expression
    , display : Elm.Expression
    , dominantBaseline : Elm.Expression
    , enableBackground : Elm.Expression
    , fillOpacity : Elm.Expression
    , fillRule : Elm.Expression
    , fill : Elm.Expression
    , filter : Elm.Expression
    , floodColor : Elm.Expression
    , floodOpacity : Elm.Expression
    , fontFamily : Elm.Expression
    , fontSizeAdjust : Elm.Expression
    , fontSize : Elm.Expression
    , fontStretch : Elm.Expression
    , fontStyle : Elm.Expression
    , fontVariant : Elm.Expression
    , fontWeight : Elm.Expression
    , glyphOrientationHorizontal : Elm.Expression
    , glyphOrientationVertical : Elm.Expression
    , imageRendering : Elm.Expression
    , kerning : Elm.Expression
    , letterSpacing : Elm.Expression
    , lightingColor : Elm.Expression
    , markerEnd : Elm.Expression
    , markerMid : Elm.Expression
    , markerStart : Elm.Expression
    , mask : Elm.Expression
    , opacity : Elm.Expression
    , overflow : Elm.Expression
    , pointerEvents : Elm.Expression
    , shapeRendering : Elm.Expression
    , stopColor : Elm.Expression
    , stopOpacity : Elm.Expression
    , strokeDasharray : Elm.Expression
    , strokeDashoffset : Elm.Expression
    , strokeLinecap : Elm.Expression
    , strokeLinejoin : Elm.Expression
    , strokeMiterlimit : Elm.Expression
    , strokeOpacity : Elm.Expression
    , strokeWidth : Elm.Expression
    , stroke : Elm.Expression
    , textAnchor : Elm.Expression
    , textDecoration : Elm.Expression
    , textRendering : Elm.Expression
    , unicodeBidi : Elm.Expression
    , visibility : Elm.Expression
    , wordSpacing : Elm.Expression
    , writingMode : Elm.Expression
    }
values_ =
    { css =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fromUnstyled =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , accentHeight =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accentHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , accelerate =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accelerate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , accumulate =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "accumulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , additive =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "additive"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alphabetic =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "alphabetic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , allowReorder =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "allowReorder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , amplitude =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "amplitude"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , arabicForm =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "arabicForm"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ascent =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ascent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , attributeName =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "attributeName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , attributeType =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "attributeType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , autoReverse =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "autoReverse"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , azimuth =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "azimuth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , baseFrequency =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baseFrequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , baseProfile =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baseProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , bbox =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "bbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , begin =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "begin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , bias =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "bias"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , by =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "by"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , calcMode =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "calcMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , capHeight =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "capHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , class =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , clipPathUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipPathUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , contentScriptType =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "contentScriptType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , contentStyleType =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "contentStyleType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , cx =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , cy =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , d =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "d"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , decelerate =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "decelerate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , descent =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "descent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , diffuseConstant =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "diffuseConstant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , divisor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "divisor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dur =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dx =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dy =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , edgeMode =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "edgeMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , elevation =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "elevation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , end =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "end"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , exponent =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "exponent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , externalResourcesRequired =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "externalResourcesRequired"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , filterRes =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filterRes"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , filterUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filterUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , format =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "format"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , from =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "from"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fx =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fy =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , g1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "g1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , g2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "g2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyphName =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyphRef =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphRef"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , gradientTransform =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "gradientTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , gradientUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "gradientUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hanging =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "hanging"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , height =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , horizAdvX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizAdvX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , horizOriginX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizOriginX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , horizOriginY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "horizOriginY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ideographic =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ideographic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , in_ =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "in_"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , in2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "in2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , intercept =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "intercept"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , k =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , k1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , k2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , k3 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k3"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , k4 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "k4"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , kernelMatrix =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kernelMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , kernelUnitLength =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kernelUnitLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , keyPoints =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keyPoints"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , keySplines =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keySplines"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , keyTimes =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "keyTimes"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lang =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lang"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lengthAdjust =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lengthAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , limitingConeAngle =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "limitingConeAngle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , local =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "local"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerHeight =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerWidth =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , maskContentUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "maskContentUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , maskUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "maskUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mathematical =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mathematical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , max =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "max"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , media =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "media"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , method =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "method"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , min =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "min"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mode =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , name =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "name"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , numOctaves =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "numOctaves"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , offset =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "offset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , operator =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "operator"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , order =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , orient =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "orient"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , orientation =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "orientation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , origin =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "origin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , overlinePosition =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overlinePosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , overlineThickness =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overlineThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , panose1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "panose1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , path =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pathLength =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pathLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , patternContentUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternContentUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , patternTransform =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , patternUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "patternUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointOrder =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointOrder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , points =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "points"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointsAtX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointsAtY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointsAtZ =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointsAtZ"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , preserveAlpha =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "preserveAlpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , preserveAspectRatio =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "preserveAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , primitiveUnits =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "primitiveUnits"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , r =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "r"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , radius =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "radius"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , refX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "refX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , refY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "refY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , renderingIntent =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "renderingIntent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , repeatCount =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "repeatCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , repeatDur =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "repeatDur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , requiredExtensions =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "requiredExtensions"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , requiredFeatures =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "requiredFeatures"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , restart =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "restart"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , result =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rotate =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "rotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rx =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "rx"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ry =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "ry"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , scale =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "scale"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , seed =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "seed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , slope =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "slope"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spacing =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "spacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , specularConstant =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "specularConstant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , specularExponent =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "specularExponent"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , speed =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "speed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spreadMethod =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "spreadMethod"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , startOffset =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "startOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stdDeviation =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stdDeviation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stemh =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stemh"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stemv =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stemv"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stitchTiles =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stitchTiles"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strikethroughPosition =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strikethroughPosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strikethroughThickness =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strikethroughThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , string =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , style =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , surfaceScale =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "surfaceScale"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , systemLanguage =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "systemLanguage"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tableValues =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "tableValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , target =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "target"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , targetX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "targetX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , targetY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "targetY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textLength =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , title =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "title"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , to =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "to"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , transform =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "transform"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , type_ =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "type_"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , u1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "u1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , u2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "u2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , underlinePosition =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "underlinePosition"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , underlineThickness =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "underlineThickness"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , unicode =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , unicodeRange =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicodeRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , unitsPerEm =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unitsPerEm"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vAlphabetic =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vAlphabetic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vHanging =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vHanging"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vIdeographic =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vIdeographic"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vMathematical =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vMathematical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , values =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "values"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , version =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "version"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vertAdvY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertAdvY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vertOriginX =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertOriginX"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , vertOriginY =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "vertOriginY"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewBox =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "viewBox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewTarget =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "viewTarget"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , width =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , widths =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "widths"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , x =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xHeight =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , x1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , x2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "x2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xChannelSelector =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xChannelSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkActuate =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkActuate"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkArcrole =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkArcrole"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkHref =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkHref"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkRole =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkRole"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkShow =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkShow"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkTitle =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xlinkType =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xlinkType"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xmlBase =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlBase"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xmlLang =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlLang"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , xmlSpace =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "xmlSpace"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , y =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , y1 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y1"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , y2 =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "y2"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , yChannelSelector =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "yChannelSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , z =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "z"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , zoomAndPan =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "zoomAndPan"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , alignmentBaseline =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "alignmentBaseline"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , baselineShift =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "baselineShift"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , clipPath =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , clipRule =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clipRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , clip =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "clip"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colorInterpolationFilters =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorInterpolationFilters"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colorInterpolation =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorInterpolation"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colorProfile =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colorRendering =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "colorRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , color =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , cursor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "cursor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , direction =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "direction"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , display =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "display"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dominantBaseline =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "dominantBaseline"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , enableBackground =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "enableBackground"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fillOpacity =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fillOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fillRule =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fillRule"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fill =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fill"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , filter =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , floodColor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "floodColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , floodOpacity =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "floodOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontFamily =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontFamily"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontSizeAdjust =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontSizeAdjust"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontSize =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontStretch =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontStretch"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontStyle =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontStyle"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontVariant =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontVariant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fontWeight =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "fontWeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyphOrientationHorizontal =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphOrientationHorizontal"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyphOrientationVertical =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "glyphOrientationVertical"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , imageRendering =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "imageRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , kerning =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "kerning"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , letterSpacing =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lightingColor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "lightingColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerEnd =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerEnd"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerMid =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerMid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , markerStart =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "markerStart"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mask =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , opacity =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "opacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , overflow =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "overflow"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointerEvents =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "pointerEvents"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , shapeRendering =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "shapeRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stopColor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stopColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stopOpacity =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stopOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeDasharray =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeDasharray"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeDashoffset =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeDashoffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeLinecap =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeLinecap"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeLinejoin =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeLinejoin"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeMiterlimit =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeMiterlimit"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeOpacity =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeOpacity"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strokeWidth =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "strokeWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stroke =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "stroke"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textAnchor =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textAnchor"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textDecoration =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textDecoration"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textRendering =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "textRendering"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , unicodeBidi =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "unicodeBidi"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , visibility =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "visibility"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , wordSpacing =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , writingMode =
        Elm.value
            { importFrom = [ "Svg", "Styled", "Attributes" ]
            , name = "writingMode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


