module Gen.Svg.Styled exposing (a, altGlyph, altGlyphDef, altGlyphItem, animate, animateColor, animateMotion, animateTransform, annotation_, call_, circle, clipPath, colorProfile, cursor, defs, desc, ellipse, feBlend, feColorMatrix, feComponentTransfer, feComposite, feConvolveMatrix, feDiffuseLighting, feDisplacementMap, feDistantLight, feFlood, feFuncA, feFuncB, feFuncG, feFuncR, feGaussianBlur, feImage, feMerge, feMergeNode, feMorphology, feOffset, fePointLight, feSpecularLighting, feSpotLight, feTile, feTurbulence, filter, font, foreignObject, fromUnstyled, g, glyph, glyphRef, image, line, linearGradient, map, marker, mask, metadata, moduleName_, mpath, node, path, pattern, polygon, polyline, radialGradient, rect, set, stop, style, styled, svg, switch, symbol, text, textPath, text_, title, toNonceUnstyled, toUnstyled, tref, tspan, use, values_, view)

{-| 
@docs moduleName_, styled, fromUnstyled, toUnstyled, toNonceUnstyled, text, node, map, svg, foreignObject, circle, ellipse, image, line, path, polygon, polyline, rect, use, animate, animateColor, animateMotion, animateTransform, mpath, set, desc, metadata, title, a, defs, g, marker, mask, pattern, switch, symbol, altGlyph, altGlyphDef, altGlyphItem, glyph, glyphRef, textPath, text_, tref, tspan, font, linearGradient, radialGradient, stop, feBlend, feColorMatrix, feComponentTransfer, feComposite, feConvolveMatrix, feDiffuseLighting, feDisplacementMap, feFlood, feFuncA, feFuncB, feFuncG, feFuncR, feGaussianBlur, feImage, feMerge, feMergeNode, feMorphology, feOffset, feSpecularLighting, feTile, feTurbulence, feDistantLight, fePointLight, feSpotLight, clipPath, colorProfile, cursor, filter, style, view, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Svg", "Styled" ]


{-| Takes a function that creates an element, and pre-applies styles to it.

styled: 
    (List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg)
    -> List Css.Style
    -> List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
styled :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> List Elm.Expression
    -> List Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
styled styledArg styledArg0 styledArg1 styledArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            , Type.list
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
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
        , Elm.list styledArg0
        , Elm.list styledArg1
        , Elm.list styledArg2
        ]


{-| fromUnstyled: VirtualDom.Node msg -> Svg.Styled.Svg msg -}
fromUnstyled : Elm.Expression -> Elm.Expression
fromUnstyled fromUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ fromUnstyledArg ]


{-| toUnstyled: Svg.Styled.Svg msg -> VirtualDom.Node msg -}
toUnstyled : Elm.Expression -> Elm.Expression
toUnstyled toUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "toUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ toUnstyledArg ]


{-| Similar to [`toUnstyled`](#toUnstyled), but adds a [nonce](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/nonce)
to the style tag so that it is compliant with the Content Security Policy of your website.

If you don't need a nonce, you should use [`toUnstyled`](#toUnstyled).

toNonceUnstyled: String -> Svg.Styled.Svg msg -> VirtualDom.Node msg
-}
toNonceUnstyled : String -> Elm.Expression -> Elm.Expression
toNonceUnstyled toNonceUnstyledArg toNonceUnstyledArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "toNonceUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string toNonceUnstyledArg, toNonceUnstyledArg0 ]


{-| A simple text node, no tags at all.

Warning: not to be confused with `text_` which produces the SVG `<text>` tag!

text: String -> Svg.Styled.Svg msg
-}
text : String -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textArg ]


{-| Create any SVG node. To create a `<rect>` helper function, you would write:

    rect : List (Attribute msg) -> List (Svg msg) -> Svg msg
    rect attributes children =
        node "rect" attributes children

You should always be able to use the helper functions already defined in this
library though!

node: 
    String
    -> List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
node : String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
node nodeArg nodeArg0 nodeArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nodeArg, Elm.list nodeArg0, Elm.list nodeArg1 ]


{-| Transform the messages produced by some `Svg`.

map: (a -> msg) -> Svg.Styled.Svg a -> Svg.Styled.Svg msg
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| The root `<svg>` node for any SVG scene. This example shows a scene
containing a rounded rectangle:

    import Svg exposing (..)
    import Svg.Attributes exposing (..)

    roundRect =
        svg
            [ width "120", height "120", viewBox "0 0 120 120" ]
            [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]

svg: 
    List (Html.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Html.Styled.Html msg
-}
svg : List Elm.Expression -> List Elm.Expression -> Elm.Expression
svg svgArg svgArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "svg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
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
        [ Elm.list svgArg, Elm.list svgArg0 ]


{-| foreignObject: 
    List (Svg.Styled.Attribute msg)
    -> List (Html.Styled.Html msg)
    -> Svg.Styled.Svg msg
-}
foreignObject : List Elm.Expression -> List Elm.Expression -> Elm.Expression
foreignObject foreignObjectArg foreignObjectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "foreignObject"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list foreignObjectArg, Elm.list foreignObjectArg0 ]


{-| The circle element is an SVG basic shape, used to create circles based on
a center point and a radius.

    circle [ cx "60", cy "60", r "50" ] []

circle: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
circle : List Elm.Expression -> List Elm.Expression -> Elm.Expression
circle circleArg circleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "circle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list circleArg, Elm.list circleArg0 ]


{-| ellipse: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
ellipse : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ellipse ellipseArg ellipseArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "ellipse"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list ellipseArg, Elm.list ellipseArg0 ]


{-| image: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
image : List Elm.Expression -> List Elm.Expression -> Elm.Expression
image imageArg imageArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "image"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list imageArg, Elm.list imageArg0 ]


{-| line: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
line : List Elm.Expression -> List Elm.Expression -> Elm.Expression
line lineArg lineArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "line"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list lineArg, Elm.list lineArg0 ]


{-| path: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
path : List Elm.Expression -> List Elm.Expression -> Elm.Expression
path pathArg pathArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list pathArg, Elm.list pathArg0 ]


{-| polygon: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
polygon : List Elm.Expression -> List Elm.Expression -> Elm.Expression
polygon polygonArg polygonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "polygon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list polygonArg, Elm.list polygonArg0 ]


{-| The polyline element is an SVG basic shape, used to create a series of
straight lines connecting several points. Typically a polyline is used to
create open shapes.

    polyline [ fill "none", stroke "black", points "20,100 40,60 70,80 100,20" ] []

polyline: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
polyline : List Elm.Expression -> List Elm.Expression -> Elm.Expression
polyline polylineArg polylineArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "polyline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list polylineArg, Elm.list polylineArg0 ]


{-| rect: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
rect : List Elm.Expression -> List Elm.Expression -> Elm.Expression
rect rectArg rectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "rect"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rectArg, Elm.list rectArg0 ]


{-| use: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
use : List Elm.Expression -> List Elm.Expression -> Elm.Expression
use useArg useArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "use"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list useArg, Elm.list useArg0 ]


{-| animate: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
animate : List Elm.Expression -> List Elm.Expression -> Elm.Expression
animate animateArg animateArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animate"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list animateArg, Elm.list animateArg0 ]


{-| animateColor: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
animateColor : List Elm.Expression -> List Elm.Expression -> Elm.Expression
animateColor animateColorArg animateColorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list animateColorArg, Elm.list animateColorArg0 ]


{-| animateMotion: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
animateMotion : List Elm.Expression -> List Elm.Expression -> Elm.Expression
animateMotion animateMotionArg animateMotionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list animateMotionArg, Elm.list animateMotionArg0 ]


{-| animateTransform: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
animateTransform : List Elm.Expression -> List Elm.Expression -> Elm.Expression
animateTransform animateTransformArg animateTransformArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list animateTransformArg, Elm.list animateTransformArg0 ]


{-| mpath: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
mpath : List Elm.Expression -> List Elm.Expression -> Elm.Expression
mpath mpathArg mpathArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "mpath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mpathArg, Elm.list mpathArg0 ]


{-| set: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
set : List Elm.Expression -> List Elm.Expression -> Elm.Expression
set setArg setArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "set"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list setArg, Elm.list setArg0 ]


{-| desc: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
desc : List Elm.Expression -> List Elm.Expression -> Elm.Expression
desc descArg descArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "desc"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list descArg, Elm.list descArg0 ]


{-| metadata: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
metadata : List Elm.Expression -> List Elm.Expression -> Elm.Expression
metadata metadataArg metadataArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "metadata"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list metadataArg, Elm.list metadataArg0 ]


{-| title: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
title : List Elm.Expression -> List Elm.Expression -> Elm.Expression
title titleArg titleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "title"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list titleArg, Elm.list titleArg0 ]


{-| The SVG Anchor Element defines a hyperlink.

a: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
a : List Elm.Expression -> List Elm.Expression -> Elm.Expression
a aArg aArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list aArg, Elm.list aArg0 ]


{-| defs: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
defs : List Elm.Expression -> List Elm.Expression -> Elm.Expression
defs defsArg defsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "defs"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list defsArg, Elm.list defsArg0 ]


{-| g: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
g : List Elm.Expression -> List Elm.Expression -> Elm.Expression
g gArg gArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "g"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list gArg, Elm.list gArg0 ]


{-| marker: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
marker : List Elm.Expression -> List Elm.Expression -> Elm.Expression
marker markerArg markerArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "marker"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list markerArg, Elm.list markerArg0 ]


{-| mask: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
mask : List Elm.Expression -> List Elm.Expression -> Elm.Expression
mask maskArg maskArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list maskArg, Elm.list maskArg0 ]


{-| pattern: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
pattern : List Elm.Expression -> List Elm.Expression -> Elm.Expression
pattern patternArg patternArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "pattern"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list patternArg, Elm.list patternArg0 ]


{-| switch: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
switch : List Elm.Expression -> List Elm.Expression -> Elm.Expression
switch switchArg switchArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "switch"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list switchArg, Elm.list switchArg0 ]


{-| symbol: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
symbol : List Elm.Expression -> List Elm.Expression -> Elm.Expression
symbol symbolArg symbolArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "symbol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list symbolArg, Elm.list symbolArg0 ]


{-| altGlyph: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
altGlyph : List Elm.Expression -> List Elm.Expression -> Elm.Expression
altGlyph altGlyphArg altGlyphArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list altGlyphArg, Elm.list altGlyphArg0 ]


{-| altGlyphDef: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
altGlyphDef : List Elm.Expression -> List Elm.Expression -> Elm.Expression
altGlyphDef altGlyphDefArg altGlyphDefArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyphDef"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list altGlyphDefArg, Elm.list altGlyphDefArg0 ]


{-| altGlyphItem: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
altGlyphItem : List Elm.Expression -> List Elm.Expression -> Elm.Expression
altGlyphItem altGlyphItemArg altGlyphItemArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyphItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list altGlyphItemArg, Elm.list altGlyphItemArg0 ]


{-| glyph: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
glyph : List Elm.Expression -> List Elm.Expression -> Elm.Expression
glyph glyphArg glyphArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "glyph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list glyphArg, Elm.list glyphArg0 ]


{-| glyphRef: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
glyphRef : List Elm.Expression -> List Elm.Expression -> Elm.Expression
glyphRef glyphRefArg glyphRefArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "glyphRef"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list glyphRefArg, Elm.list glyphRefArg0 ]


{-| textPath: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
textPath : List Elm.Expression -> List Elm.Expression -> Elm.Expression
textPath textPathArg textPathArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "textPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list textPathArg, Elm.list textPathArg0 ]


{-| text_: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
text_ : List Elm.Expression -> List Elm.Expression -> Elm.Expression
text_ text_Arg text_Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "text_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list text_Arg, Elm.list text_Arg0 ]


{-| tref: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
tref : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tref trefArg trefArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "tref"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list trefArg, Elm.list trefArg0 ]


{-| tspan: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
tspan : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tspan tspanArg tspanArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "tspan"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tspanArg, Elm.list tspanArg0 ]


{-| font: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
font : List Elm.Expression -> List Elm.Expression -> Elm.Expression
font fontArg fontArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "font"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list fontArg, Elm.list fontArg0 ]


{-| linearGradient: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
linearGradient : List Elm.Expression -> List Elm.Expression -> Elm.Expression
linearGradient linearGradientArg linearGradientArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "linearGradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list linearGradientArg, Elm.list linearGradientArg0 ]


{-| radialGradient: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
radialGradient : List Elm.Expression -> List Elm.Expression -> Elm.Expression
radialGradient radialGradientArg radialGradientArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "radialGradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list radialGradientArg, Elm.list radialGradientArg0 ]


{-| stop: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
stop : List Elm.Expression -> List Elm.Expression -> Elm.Expression
stop stopArg stopArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "stop"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list stopArg, Elm.list stopArg0 ]


{-| feBlend: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feBlend : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feBlend feBlendArg feBlendArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feBlend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feBlendArg, Elm.list feBlendArg0 ]


{-| feColorMatrix: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feColorMatrix : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feColorMatrix feColorMatrixArg feColorMatrixArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feColorMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feColorMatrixArg, Elm.list feColorMatrixArg0 ]


{-| feComponentTransfer: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feComponentTransfer :
    List Elm.Expression -> List Elm.Expression -> Elm.Expression
feComponentTransfer feComponentTransferArg feComponentTransferArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feComponentTransfer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feComponentTransferArg, Elm.list feComponentTransferArg0 ]


{-| feComposite: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feComposite : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feComposite feCompositeArg feCompositeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feComposite"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feCompositeArg, Elm.list feCompositeArg0 ]


{-| feConvolveMatrix: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feConvolveMatrix : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feConvolveMatrix feConvolveMatrixArg feConvolveMatrixArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feConvolveMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feConvolveMatrixArg, Elm.list feConvolveMatrixArg0 ]


{-| feDiffuseLighting: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feDiffuseLighting : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feDiffuseLighting feDiffuseLightingArg feDiffuseLightingArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDiffuseLighting"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feDiffuseLightingArg, Elm.list feDiffuseLightingArg0 ]


{-| feDisplacementMap: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feDisplacementMap : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feDisplacementMap feDisplacementMapArg feDisplacementMapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDisplacementMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feDisplacementMapArg, Elm.list feDisplacementMapArg0 ]


{-| feFlood: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feFlood : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feFlood feFloodArg feFloodArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFlood"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feFloodArg, Elm.list feFloodArg0 ]


{-| feFuncA: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feFuncA : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feFuncA feFuncAArg feFuncAArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncA"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feFuncAArg, Elm.list feFuncAArg0 ]


{-| feFuncB: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feFuncB : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feFuncB feFuncBArg feFuncBArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncB"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feFuncBArg, Elm.list feFuncBArg0 ]


{-| feFuncG: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feFuncG : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feFuncG feFuncGArg feFuncGArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncG"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feFuncGArg, Elm.list feFuncGArg0 ]


{-| feFuncR: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feFuncR : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feFuncR feFuncRArg feFuncRArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncR"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feFuncRArg, Elm.list feFuncRArg0 ]


{-| feGaussianBlur: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feGaussianBlur : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feGaussianBlur feGaussianBlurArg feGaussianBlurArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feGaussianBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feGaussianBlurArg, Elm.list feGaussianBlurArg0 ]


{-| feImage: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feImage : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feImage feImageArg feImageArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feImage"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feImageArg, Elm.list feImageArg0 ]


{-| feMerge: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feMerge : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feMerge feMergeArg feMergeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMerge"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feMergeArg, Elm.list feMergeArg0 ]


{-| feMergeNode: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feMergeNode : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feMergeNode feMergeNodeArg feMergeNodeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMergeNode"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feMergeNodeArg, Elm.list feMergeNodeArg0 ]


{-| feMorphology: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feMorphology : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feMorphology feMorphologyArg feMorphologyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMorphology"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feMorphologyArg, Elm.list feMorphologyArg0 ]


{-| feOffset: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feOffset : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feOffset feOffsetArg feOffsetArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feOffsetArg, Elm.list feOffsetArg0 ]


{-| feSpecularLighting: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feSpecularLighting :
    List Elm.Expression -> List Elm.Expression -> Elm.Expression
feSpecularLighting feSpecularLightingArg feSpecularLightingArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feSpecularLighting"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feSpecularLightingArg, Elm.list feSpecularLightingArg0 ]


{-| feTile: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feTile : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feTile feTileArg feTileArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feTile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feTileArg, Elm.list feTileArg0 ]


{-| feTurbulence: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feTurbulence : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feTurbulence feTurbulenceArg feTurbulenceArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feTurbulence"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feTurbulenceArg, Elm.list feTurbulenceArg0 ]


{-| feDistantLight: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feDistantLight : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feDistantLight feDistantLightArg feDistantLightArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDistantLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feDistantLightArg, Elm.list feDistantLightArg0 ]


{-| fePointLight: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
fePointLight : List Elm.Expression -> List Elm.Expression -> Elm.Expression
fePointLight fePointLightArg fePointLightArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "fePointLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list fePointLightArg, Elm.list fePointLightArg0 ]


{-| feSpotLight: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
feSpotLight : List Elm.Expression -> List Elm.Expression -> Elm.Expression
feSpotLight feSpotLightArg feSpotLightArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feSpotLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list feSpotLightArg, Elm.list feSpotLightArg0 ]


{-| clipPath: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
clipPath : List Elm.Expression -> List Elm.Expression -> Elm.Expression
clipPath clipPathArg clipPathArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list clipPathArg, Elm.list clipPathArg0 ]


{-| colorProfile: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
colorProfile : List Elm.Expression -> List Elm.Expression -> Elm.Expression
colorProfile colorProfileArg colorProfileArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "colorProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list colorProfileArg, Elm.list colorProfileArg0 ]


{-| cursor: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
cursor : List Elm.Expression -> List Elm.Expression -> Elm.Expression
cursor cursorArg cursorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "cursor"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cursorArg, Elm.list cursorArg0 ]


{-| filter: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
filter : List Elm.Expression -> List Elm.Expression -> Elm.Expression
filter filterArg filterArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list filterArg, Elm.list filterArg0 ]


{-| style: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
style : List Elm.Expression -> List Elm.Expression -> Elm.Expression
style styleArg styleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list styleArg, Elm.list styleArg0 ]


{-| view: 
    List (Svg.Styled.Attribute msg)
    -> List (Svg.Styled.Svg msg)
    -> Svg.Styled.Svg msg
-}
view : List Elm.Expression -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list viewArg, Elm.list viewArg0 ]


annotation_ :
    { svg : Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { svg =
        \svgArg0 ->
            Type.alias
                moduleName_
                "Svg"
                [ svgArg0 ]
                (Type.namedWith
                    [ "VirtualDom", "Styled" ]
                    "Node"
                    [ Type.var "msg" ]
                )
    , attribute =
        \attributeArg0 ->
            Type.alias
                moduleName_
                "Attribute"
                [ attributeArg0 ]
                (Type.namedWith
                    [ "VirtualDom", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
    }


call_ :
    { styled :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , fromUnstyled : Elm.Expression -> Elm.Expression
    , toUnstyled : Elm.Expression -> Elm.Expression
    , toNonceUnstyled : Elm.Expression -> Elm.Expression -> Elm.Expression
    , text : Elm.Expression -> Elm.Expression
    , node :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , svg : Elm.Expression -> Elm.Expression -> Elm.Expression
    , foreignObject : Elm.Expression -> Elm.Expression -> Elm.Expression
    , circle : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ellipse : Elm.Expression -> Elm.Expression -> Elm.Expression
    , image : Elm.Expression -> Elm.Expression -> Elm.Expression
    , line : Elm.Expression -> Elm.Expression -> Elm.Expression
    , path : Elm.Expression -> Elm.Expression -> Elm.Expression
    , polygon : Elm.Expression -> Elm.Expression -> Elm.Expression
    , polyline : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rect : Elm.Expression -> Elm.Expression -> Elm.Expression
    , use : Elm.Expression -> Elm.Expression -> Elm.Expression
    , animate : Elm.Expression -> Elm.Expression -> Elm.Expression
    , animateColor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , animateMotion : Elm.Expression -> Elm.Expression -> Elm.Expression
    , animateTransform : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mpath : Elm.Expression -> Elm.Expression -> Elm.Expression
    , set : Elm.Expression -> Elm.Expression -> Elm.Expression
    , desc : Elm.Expression -> Elm.Expression -> Elm.Expression
    , metadata : Elm.Expression -> Elm.Expression -> Elm.Expression
    , title : Elm.Expression -> Elm.Expression -> Elm.Expression
    , a : Elm.Expression -> Elm.Expression -> Elm.Expression
    , defs : Elm.Expression -> Elm.Expression -> Elm.Expression
    , g : Elm.Expression -> Elm.Expression -> Elm.Expression
    , marker : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mask : Elm.Expression -> Elm.Expression -> Elm.Expression
    , pattern : Elm.Expression -> Elm.Expression -> Elm.Expression
    , switch : Elm.Expression -> Elm.Expression -> Elm.Expression
    , symbol : Elm.Expression -> Elm.Expression -> Elm.Expression
    , altGlyph : Elm.Expression -> Elm.Expression -> Elm.Expression
    , altGlyphDef : Elm.Expression -> Elm.Expression -> Elm.Expression
    , altGlyphItem : Elm.Expression -> Elm.Expression -> Elm.Expression
    , glyph : Elm.Expression -> Elm.Expression -> Elm.Expression
    , glyphRef : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textPath : Elm.Expression -> Elm.Expression -> Elm.Expression
    , text_ : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tref : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tspan : Elm.Expression -> Elm.Expression -> Elm.Expression
    , font : Elm.Expression -> Elm.Expression -> Elm.Expression
    , linearGradient : Elm.Expression -> Elm.Expression -> Elm.Expression
    , radialGradient : Elm.Expression -> Elm.Expression -> Elm.Expression
    , stop : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feBlend : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feColorMatrix : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feComponentTransfer : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feComposite : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feConvolveMatrix : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feDiffuseLighting : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feDisplacementMap : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feFlood : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feFuncA : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feFuncB : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feFuncG : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feFuncR : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feGaussianBlur : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feImage : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feMerge : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feMergeNode : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feMorphology : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feOffset : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feSpecularLighting : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feTile : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feTurbulence : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feDistantLight : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fePointLight : Elm.Expression -> Elm.Expression -> Elm.Expression
    , feSpotLight : Elm.Expression -> Elm.Expression -> Elm.Expression
    , clipPath : Elm.Expression -> Elm.Expression -> Elm.Expression
    , colorProfile : Elm.Expression -> Elm.Expression -> Elm.Expression
    , cursor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , filter : Elm.Expression -> Elm.Expression -> Elm.Expression
    , style : Elm.Expression -> Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { styled =
        \styledArg styledArg0 styledArg1 styledArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "styled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Svg", "Styled" ]
                                            "Attribute"
                                            [ Type.var "msg" ]
                                        )
                                    , Type.list
                                        (Type.namedWith
                                            [ "Svg", "Styled" ]
                                            "Svg"
                                            [ Type.var "msg" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styledArg, styledArg0, styledArg1, styledArg2 ]
    , fromUnstyled =
        \fromUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "fromUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "VirtualDom" ]
                                    "Node"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromUnstyledArg ]
    , toUnstyled =
        \toUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "toUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom" ]
                                    "Node"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toUnstyledArg ]
    , toNonceUnstyled =
        \toNonceUnstyledArg toNonceUnstyledArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "toNonceUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "VirtualDom" ]
                                    "Node"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toNonceUnstyledArg, toNonceUnstyledArg0 ]
    , text =
        \textArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textArg ]
    , node =
        \nodeArg nodeArg0 nodeArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "node"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nodeArg, nodeArg0, nodeArg1 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.var "msg")
                                , Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , svg =
        \svgArg svgArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "svg"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
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
                [ svgArg, svgArg0 ]
    , foreignObject =
        \foreignObjectArg foreignObjectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "foreignObject"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ foreignObjectArg, foreignObjectArg0 ]
    , circle =
        \circleArg circleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "circle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ circleArg, circleArg0 ]
    , ellipse =
        \ellipseArg ellipseArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "ellipse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ellipseArg, ellipseArg0 ]
    , image =
        \imageArg imageArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "image"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ imageArg, imageArg0 ]
    , line =
        \lineArg lineArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "line"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lineArg, lineArg0 ]
    , path =
        \pathArg pathArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "path"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pathArg, pathArg0 ]
    , polygon =
        \polygonArg polygonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "polygon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ polygonArg, polygonArg0 ]
    , polyline =
        \polylineArg polylineArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "polyline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ polylineArg, polylineArg0 ]
    , rect =
        \rectArg rectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "rect"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rectArg, rectArg0 ]
    , use =
        \useArg useArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "use"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ useArg, useArg0 ]
    , animate =
        \animateArg animateArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "animate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ animateArg, animateArg0 ]
    , animateColor =
        \animateColorArg animateColorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "animateColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ animateColorArg, animateColorArg0 ]
    , animateMotion =
        \animateMotionArg animateMotionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "animateMotion"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ animateMotionArg, animateMotionArg0 ]
    , animateTransform =
        \animateTransformArg animateTransformArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "animateTransform"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ animateTransformArg, animateTransformArg0 ]
    , mpath =
        \mpathArg mpathArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "mpath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mpathArg, mpathArg0 ]
    , set =
        \setArg setArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "set"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ setArg, setArg0 ]
    , desc =
        \descArg descArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "desc"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ descArg, descArg0 ]
    , metadata =
        \metadataArg metadataArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "metadata"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ metadataArg, metadataArg0 ]
    , title =
        \titleArg titleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "title"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ titleArg, titleArg0 ]
    , a =
        \aArg aArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "a"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ aArg, aArg0 ]
    , defs =
        \defsArg defsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "defs"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ defsArg, defsArg0 ]
    , g =
        \gArg gArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "g"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ gArg, gArg0 ]
    , marker =
        \markerArg markerArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "marker"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markerArg, markerArg0 ]
    , mask =
        \maskArg maskArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "mask"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maskArg, maskArg0 ]
    , pattern =
        \patternArg patternArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "pattern"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ patternArg, patternArg0 ]
    , switch =
        \switchArg switchArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "switch"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ switchArg, switchArg0 ]
    , symbol =
        \symbolArg symbolArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "symbol"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ symbolArg, symbolArg0 ]
    , altGlyph =
        \altGlyphArg altGlyphArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "altGlyph"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ altGlyphArg, altGlyphArg0 ]
    , altGlyphDef =
        \altGlyphDefArg altGlyphDefArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "altGlyphDef"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ altGlyphDefArg, altGlyphDefArg0 ]
    , altGlyphItem =
        \altGlyphItemArg altGlyphItemArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "altGlyphItem"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ altGlyphItemArg, altGlyphItemArg0 ]
    , glyph =
        \glyphArg glyphArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "glyph"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphArg, glyphArg0 ]
    , glyphRef =
        \glyphRefArg glyphRefArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "glyphRef"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glyphRefArg, glyphRefArg0 ]
    , textPath =
        \textPathArg textPathArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "textPath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textPathArg, textPathArg0 ]
    , text_ =
        \text_Arg text_Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "text_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ text_Arg, text_Arg0 ]
    , tref =
        \trefArg trefArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "tref"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ trefArg, trefArg0 ]
    , tspan =
        \tspanArg tspanArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "tspan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tspanArg, tspanArg0 ]
    , font =
        \fontArg fontArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "font"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fontArg, fontArg0 ]
    , linearGradient =
        \linearGradientArg linearGradientArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "linearGradient"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linearGradientArg, linearGradientArg0 ]
    , radialGradient =
        \radialGradientArg radialGradientArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "radialGradient"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ radialGradientArg, radialGradientArg0 ]
    , stop =
        \stopArg stopArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "stop"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stopArg, stopArg0 ]
    , feBlend =
        \feBlendArg feBlendArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feBlend"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feBlendArg, feBlendArg0 ]
    , feColorMatrix =
        \feColorMatrixArg feColorMatrixArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feColorMatrix"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feColorMatrixArg, feColorMatrixArg0 ]
    , feComponentTransfer =
        \feComponentTransferArg feComponentTransferArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feComponentTransfer"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feComponentTransferArg, feComponentTransferArg0 ]
    , feComposite =
        \feCompositeArg feCompositeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feComposite"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feCompositeArg, feCompositeArg0 ]
    , feConvolveMatrix =
        \feConvolveMatrixArg feConvolveMatrixArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feConvolveMatrix"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feConvolveMatrixArg, feConvolveMatrixArg0 ]
    , feDiffuseLighting =
        \feDiffuseLightingArg feDiffuseLightingArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feDiffuseLighting"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feDiffuseLightingArg, feDiffuseLightingArg0 ]
    , feDisplacementMap =
        \feDisplacementMapArg feDisplacementMapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feDisplacementMap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feDisplacementMapArg, feDisplacementMapArg0 ]
    , feFlood =
        \feFloodArg feFloodArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feFlood"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feFloodArg, feFloodArg0 ]
    , feFuncA =
        \feFuncAArg feFuncAArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feFuncA"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feFuncAArg, feFuncAArg0 ]
    , feFuncB =
        \feFuncBArg feFuncBArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feFuncB"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feFuncBArg, feFuncBArg0 ]
    , feFuncG =
        \feFuncGArg feFuncGArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feFuncG"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feFuncGArg, feFuncGArg0 ]
    , feFuncR =
        \feFuncRArg feFuncRArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feFuncR"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feFuncRArg, feFuncRArg0 ]
    , feGaussianBlur =
        \feGaussianBlurArg feGaussianBlurArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feGaussianBlur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feGaussianBlurArg, feGaussianBlurArg0 ]
    , feImage =
        \feImageArg feImageArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feImage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feImageArg, feImageArg0 ]
    , feMerge =
        \feMergeArg feMergeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feMerge"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feMergeArg, feMergeArg0 ]
    , feMergeNode =
        \feMergeNodeArg feMergeNodeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feMergeNode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feMergeNodeArg, feMergeNodeArg0 ]
    , feMorphology =
        \feMorphologyArg feMorphologyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feMorphology"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feMorphologyArg, feMorphologyArg0 ]
    , feOffset =
        \feOffsetArg feOffsetArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feOffset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feOffsetArg, feOffsetArg0 ]
    , feSpecularLighting =
        \feSpecularLightingArg feSpecularLightingArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feSpecularLighting"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feSpecularLightingArg, feSpecularLightingArg0 ]
    , feTile =
        \feTileArg feTileArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feTile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feTileArg, feTileArg0 ]
    , feTurbulence =
        \feTurbulenceArg feTurbulenceArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feTurbulence"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feTurbulenceArg, feTurbulenceArg0 ]
    , feDistantLight =
        \feDistantLightArg feDistantLightArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feDistantLight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feDistantLightArg, feDistantLightArg0 ]
    , fePointLight =
        \fePointLightArg fePointLightArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "fePointLight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fePointLightArg, fePointLightArg0 ]
    , feSpotLight =
        \feSpotLightArg feSpotLightArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "feSpotLight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ feSpotLightArg, feSpotLightArg0 ]
    , clipPath =
        \clipPathArg clipPathArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "clipPath"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ clipPathArg, clipPathArg0 ]
    , colorProfile =
        \colorProfileArg colorProfileArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "colorProfile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorProfileArg, colorProfileArg0 ]
    , cursor =
        \cursorArg cursorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "cursor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cursorArg, cursorArg0 ]
    , filter =
        \filterArg filterArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "filter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ filterArg, filterArg0 ]
    , style =
        \styleArg styleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "style"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styleArg, styleArg0 ]
    , view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Svg", "Styled" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Svg", "Styled" ]
                                        "Svg"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    }


values_ :
    { styled : Elm.Expression
    , fromUnstyled : Elm.Expression
    , toUnstyled : Elm.Expression
    , toNonceUnstyled : Elm.Expression
    , text : Elm.Expression
    , node : Elm.Expression
    , map : Elm.Expression
    , svg : Elm.Expression
    , foreignObject : Elm.Expression
    , circle : Elm.Expression
    , ellipse : Elm.Expression
    , image : Elm.Expression
    , line : Elm.Expression
    , path : Elm.Expression
    , polygon : Elm.Expression
    , polyline : Elm.Expression
    , rect : Elm.Expression
    , use : Elm.Expression
    , animate : Elm.Expression
    , animateColor : Elm.Expression
    , animateMotion : Elm.Expression
    , animateTransform : Elm.Expression
    , mpath : Elm.Expression
    , set : Elm.Expression
    , desc : Elm.Expression
    , metadata : Elm.Expression
    , title : Elm.Expression
    , a : Elm.Expression
    , defs : Elm.Expression
    , g : Elm.Expression
    , marker : Elm.Expression
    , mask : Elm.Expression
    , pattern : Elm.Expression
    , switch : Elm.Expression
    , symbol : Elm.Expression
    , altGlyph : Elm.Expression
    , altGlyphDef : Elm.Expression
    , altGlyphItem : Elm.Expression
    , glyph : Elm.Expression
    , glyphRef : Elm.Expression
    , textPath : Elm.Expression
    , text_ : Elm.Expression
    , tref : Elm.Expression
    , tspan : Elm.Expression
    , font : Elm.Expression
    , linearGradient : Elm.Expression
    , radialGradient : Elm.Expression
    , stop : Elm.Expression
    , feBlend : Elm.Expression
    , feColorMatrix : Elm.Expression
    , feComponentTransfer : Elm.Expression
    , feComposite : Elm.Expression
    , feConvolveMatrix : Elm.Expression
    , feDiffuseLighting : Elm.Expression
    , feDisplacementMap : Elm.Expression
    , feFlood : Elm.Expression
    , feFuncA : Elm.Expression
    , feFuncB : Elm.Expression
    , feFuncG : Elm.Expression
    , feFuncR : Elm.Expression
    , feGaussianBlur : Elm.Expression
    , feImage : Elm.Expression
    , feMerge : Elm.Expression
    , feMergeNode : Elm.Expression
    , feMorphology : Elm.Expression
    , feOffset : Elm.Expression
    , feSpecularLighting : Elm.Expression
    , feTile : Elm.Expression
    , feTurbulence : Elm.Expression
    , feDistantLight : Elm.Expression
    , fePointLight : Elm.Expression
    , feSpotLight : Elm.Expression
    , clipPath : Elm.Expression
    , colorProfile : Elm.Expression
    , cursor : Elm.Expression
    , filter : Elm.Expression
    , style : Elm.Expression
    , view : Elm.Expression
    }
values_ =
    { styled =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            , Type.list
                                (Type.namedWith
                                    [ "Svg", "Styled" ]
                                    "Svg"
                                    [ Type.var "msg" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fromUnstyled =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , toUnstyled =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "toUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , toNonceUnstyled =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "toNonceUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "VirtualDom" ]
                            "Node"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , text =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , node =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , svg =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "svg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
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
    , foreignObject =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "foreignObject"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , circle =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "circle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ellipse =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "ellipse"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , image =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "image"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , line =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "line"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , path =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , polygon =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "polygon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , polyline =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "polyline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rect =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "rect"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , use =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "use"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , animate =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animate"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , animateColor =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , animateMotion =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateMotion"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , animateTransform =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "animateTransform"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mpath =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "mpath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , set =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "set"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , desc =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "desc"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , metadata =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "metadata"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , title =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "title"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , a =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , defs =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "defs"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , g =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "g"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , marker =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "marker"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mask =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "mask"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pattern =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "pattern"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , switch =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "switch"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , symbol =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "symbol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , altGlyph =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , altGlyphDef =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyphDef"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , altGlyphItem =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "altGlyphItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyph =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "glyph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glyphRef =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "glyphRef"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textPath =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "textPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , text_ =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "text_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tref =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "tref"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tspan =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "tspan"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , font =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "font"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , linearGradient =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "linearGradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , radialGradient =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "radialGradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , stop =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "stop"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feBlend =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feBlend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feColorMatrix =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feColorMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feComponentTransfer =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feComponentTransfer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feComposite =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feComposite"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feConvolveMatrix =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feConvolveMatrix"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feDiffuseLighting =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDiffuseLighting"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feDisplacementMap =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDisplacementMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feFlood =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFlood"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feFuncA =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncA"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feFuncB =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncB"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feFuncG =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncG"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feFuncR =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feFuncR"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feGaussianBlur =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feGaussianBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feImage =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feImage"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feMerge =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMerge"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feMergeNode =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMergeNode"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feMorphology =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feMorphology"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feOffset =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feOffset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feSpecularLighting =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feSpecularLighting"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feTile =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feTile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feTurbulence =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feTurbulence"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feDistantLight =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feDistantLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fePointLight =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "fePointLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , feSpotLight =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "feSpotLight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , clipPath =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "clipPath"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colorProfile =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "colorProfile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , cursor =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "cursor"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , filter =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , style =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Svg", "Styled" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Svg", "Styled" ]
                                "Svg"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Svg", "Styled" ]
                            "Svg"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


