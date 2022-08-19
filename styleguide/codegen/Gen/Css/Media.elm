module Gen.Css.Media exposing (all, annotation_, anyHover, anyPointer, aspectRatio, bits, call_, canHover, coarse, color, colorGamut, colorIndex, dpcm, dpi, dppx, enabled, fast, fine, grid, height, hover, initialOnly, interlace, landscape, make_, maxAspectRatio, maxColor, maxColorIndex, maxHeight, maxMonochrome, maxResolution, maxWidth, minAspectRatio, minColor, minColorIndex, minHeight, minMonochrome, minResolution, minWidth, moduleName_, monochrome, not, only, optionalPaged, orientation, overflowBlock, overflowInline, p3, paged, pointer, portrait, print, progressive, ratio, rec2020, resolution, scan, screen, scripting, slow, speech, srgb, update, values_, width, withMedia, withMediaQuery)

{-| 
@docs moduleName_, withMedia, withMediaQuery, all, only, not, screen, print, speech, minWidth, width, maxWidth, minHeight, height, maxHeight, ratio, minAspectRatio, aspectRatio, maxAspectRatio, landscape, portrait, orientation, dpi, dpcm, dppx, minResolution, resolution, maxResolution, progressive, interlace, scan, grid, slow, fast, update, paged, optionalPaged, overflowBlock, overflowInline, bits, minColor, color, maxColor, minMonochrome, monochrome, maxMonochrome, minColorIndex, colorIndex, maxColorIndex, srgb, p3, rec2020, colorGamut, fine, coarse, pointer, anyPointer, canHover, hover, anyHover, initialOnly, enabled, scripting, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Css", "Media" ]


{-| Combines media queries that are nested under selectors into a `@media` rule.

    css
        [ withMedia [ only screen [ Media.minWidth (px 300), Media.maxWidth (px 800) ] ]
            [ Css.maxWidth (px 300) ]

The above code translates into the following CSS.

```css
@media only screen and (min-width: 300px) and (max-width: 800px) {
    ._c9f0fd {
        max-width: 300px;
    }
}
```

withMedia: List Css.Media.MediaQuery -> List Css.Style -> Css.Style
-}
withMedia : List Elm.Expression -> List Elm.Expression -> Elm.Expression
withMedia withMediaArg withMediaArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "withMedia"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list withMediaArg, Elm.list withMediaArg0 ]


{-| Manually specify a `@media` rule that is nested under an element or class
using a List of strings.

    body
        [ withMediaQuery [ "screen and (min-width: 320px)", "screen and (max-height: 400px)" ]
            [ fontSize (px 14 px) ]
        ]

The above code translates into the following CSS.

```css
@media screen and (min-width: 320px), screen and (max-height: 400px) {
    body {
        font-size: 14px;
    }
}
```

withMediaQuery: List String -> List Css.Style -> Css.Style
-}
withMediaQuery : List String -> List Elm.Expression -> Elm.Expression
withMediaQuery withMediaQueryArg withMediaQueryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "withMediaQuery"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
        )
        [ Elm.list (List.map Elm.string withMediaQueryArg)
        , Elm.list withMediaQueryArg0
        ]


{-| Build a media query that will match all media types.

The supplied `expressions` are combined with `and`.

    media [ all [ color, landscape ] ]
        [ body [ Css.color (hex "ff0000") ] ]

The above code translates into the following CSS.

```css
@media (color) and (landscape) {
    body {
        color: #ff0000;
    }
}
```

all: List Css.Media.Expression -> Css.Media.MediaQuery
-}
all : List Elm.Expression -> Elm.Expression
all allArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
        )
        [ Elm.list allArg ]


{-| Build a media query matching a single media type.

    media [ only screen [ minWidth (px 320), portrait ] ]
        [ body [ Css.color (hex "ff0000") ] ]

The above code translates into the following CSS.

```css
@media only screen and (min-width: 320px) and (portrait) {
    body {
        color: #ff0000;
    }
}
```

only: Css.Media.MediaType -> List Css.Media.Expression -> Css.Media.MediaQuery
-}
only : Elm.Expression -> List Elm.Expression -> Elm.Expression
only onlyArg onlyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "only"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaType" []
                        , Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
        )
        [ onlyArg, Elm.list onlyArg0 ]


{-| Build a negated media query.

    media [ not screen [] ]
        [ body [ Css.color (hex "ff0000") ] ]

The above code translates into the following CSS.

```css
@media not screen {
    body {
        color: #ff0000;
    }
}
```

not: Css.Media.MediaType -> List Css.Media.Expression -> Css.Media.MediaQuery
-}
not : Elm.Expression -> List Elm.Expression -> Elm.Expression
not notArg notArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "not"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaType" []
                        , Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
        )
        [ notArg, Elm.list notArg0 ]


{-| Media type for any device not matched by print or speech.

    media (and screen (maxWidth (px 600)) [ Css.class mobileNav display none ]

screen: Css.Media.MediaType
-}
screen : Elm.Expression
screen =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "screen"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
        }


{-| Media type for printers

    media print [ a [ color (hex 0), textDecoration none ] ]

print: Css.Media.MediaType
-}
print : Elm.Expression
print =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "print"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
        }


{-| Media type for screenreaders and similar devices that read out a page

    media (not speech) [ Css.class screenReaderOnly [ display none ] ]

speech: Css.Media.MediaType
-}
speech : Elm.Expression
speech =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "speech"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
        }


{-| Media feature [`min-width`](https://drafts.csswg.org/mediaqueries/#width)
Queries the width of the output device.

    media (Media.minWidth (px 600)) [ Css.class Container [ Css.maxWidth (px 500) ] ]

minWidth: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
minWidth : Elm.Expression -> Elm.Expression
minWidth minWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minWidthArg ]


{-| Media feature [`width`](https://drafts.csswg.org/mediaqueries/#width)

    media (Media.width (px 200)) [ ... ]

width: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
width : Elm.Expression -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ widthArg ]


{-| Media feature [`max-width`](https://drafts.csswg.org/mediaqueries/#width)

    media (Media.maxWidth (px 800)) [ Css.class MobileNav [ display none ] ]

maxWidth: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
maxWidth : Elm.Expression -> Elm.Expression
maxWidth maxWidthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxWidthArg ]


{-| Media feature [`min-height`](https://drafts.csswg.org/mediaqueries/#height)

    media (Media.minHeight (px 400)) [ Css.class TopBanner [ display block ] ]

minHeight: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
minHeight : Elm.Expression -> Elm.Expression
minHeight minHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minHeightArg ]


{-| Media feature [`height`](https://drafts.csswg.org/mediaqueries/#height)

height: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
height : Elm.Expression -> Elm.Expression
height heightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ heightArg ]


{-| Media feature [`max-height`](https://drafts.csswg.org/mediaqueries/#height)

    media (Media.maxHeight (px 399)) [ Css.class TopBanner [ display none ] ]

maxHeight: Css.Media.AbsoluteLength compatible -> Css.Media.Expression
-}
maxHeight : Elm.Expression -> Elm.Expression
maxHeight maxHeightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxHeightArg ]


{-| Create a ratio.

    --a ratio of 4/3
    ratio 4 3

ratio: Int -> Int -> Css.Media.Ratio
-}
ratio : Int -> Int -> Elm.Expression
ratio ratioArg ratioArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "ratio"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Css", "Media" ] "Ratio" [])
                    )
            }
        )
        [ Elm.int ratioArg, Elm.int ratioArg0 ]


{-| Media feature [`min-aspect-ratio`](https://drafts.csswg.org/mediaqueries/#aspect-ratio)

    media (minAspectRatio (ratio 1 1)) [ ... ]

minAspectRatio: Css.Media.Ratio -> Css.Media.Expression
-}
minAspectRatio : Elm.Expression -> Elm.Expression
minAspectRatio minAspectRatioArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minAspectRatioArg ]


{-| Media feature [`aspect-ratio`](https://drafts.csswg.org/mediaqueries/#aspect-ratio)

    media (aspectRatio (ratio 16 10)) [ ... ]

aspectRatio: Css.Media.Ratio -> Css.Media.Expression
-}
aspectRatio : Elm.Expression -> Elm.Expression
aspectRatio aspectRatioArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "aspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ aspectRatioArg ]


{-| Media feature [`max-aspect-ratio`](https://drafts.csswg.org/mediaqueries/#aspect-ratio)

    media (maxAspectRatio (ratio 16 9)) [ ... ]

maxAspectRatio: Css.Media.Ratio -> Css.Media.Expression
-}
maxAspectRatio : Elm.Expression -> Elm.Expression
maxAspectRatio maxAspectRatioArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxAspectRatioArg ]


{-| CSS value [`landscape`](https://drafts.csswg.org/mediaqueries/#valdef-media-orientation-portrait)

landscape: Css.Media.Landscape
-}
landscape : Elm.Expression
landscape =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "landscape"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Landscape" [])
        }


{-| CSS value [`portrait`](https://drafts.csswg.org/mediaqueries/#valdef-media-orientation-portrait)

portrait: Css.Media.Portrait
-}
portrait : Elm.Expression
portrait =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "portrait"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Portrait" [])
        }


{-| Media feature [`orientation`](https://drafts.csswg.org/mediaqueries/#orientation).
Accepts `portrait` or `landscape`.

orientation: Css.Media.Orientation a -> Css.Media.Expression
-}
orientation : Elm.Expression -> Elm.Expression
orientation orientationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "orientation"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "Orientation"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ orientationArg ]


{-| `dpi`: Dots per inch. <https://www.w3.org/TR/css3-values/#resolution-value>

    dpi 166

dpi: Float -> Css.Media.Resolution
-}
dpi : Float -> Elm.Expression
dpi dpiArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dpi"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
        )
        [ Elm.float dpiArg ]


{-| `dpcm`: Dots per centimeter. <https://www.w3.org/TR/css3-values/#resolution-value>

    dpcm 65

dpcm: Float -> Css.Media.Resolution
-}
dpcm : Float -> Elm.Expression
dpcm dpcmArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dpcm"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
        )
        [ Elm.float dpcmArg ]


{-| `dppx`: Dots per pixel. <https://www.w3.org/TR/css3-values/#resolution-value>

    dppx 1.5

dppx: Float -> Css.Media.Resolution
-}
dppx : Float -> Elm.Expression
dppx dppxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dppx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
        )
        [ Elm.float dppxArg ]


{-| Media feature [`min-resolution`](https://drafts.csswg.org/mediaqueries/#resolution).
Describes the resolution of the output device.

    media (minResolution (dpi 600)) [ Css.class HiResImg [ display block ] ]

minResolution: Css.Media.Resolution -> Css.Media.Expression
-}
minResolution : Elm.Expression -> Elm.Expression
minResolution minResolutionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minResolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minResolutionArg ]


{-| Media feature [`resolution`](https://drafts.csswg.org/mediaqueries/#resolution)
Describes the resolution of the output device.

    media (resolution (dppx 2)) [ img [ width (pct 50) ] ]

resolution: Css.Media.Resolution -> Css.Media.Expression
-}
resolution : Elm.Expression -> Elm.Expression
resolution resolutionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "resolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ resolutionArg ]


{-| Media feature [`max-resolution`](https://drafts.csswg.org/mediaqueries/#resolution)
Describes the resolution of the output device.

    media (maxResolution (dpcm 65)) [ Css.class HiResImg [ display none ] ]

maxResolution: Css.Media.Resolution -> Css.Media.Expression
-}
maxResolution : Elm.Expression -> Elm.Expression
maxResolution maxResolutionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxResolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxResolutionArg ]


{-| CSS value [`progressive`](https://drafts.csswg.org/mediaqueries/#valdef-media-scan-progressive)

progressive: Css.Media.Progressive
-}
progressive : Elm.Expression
progressive =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "progressive"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Progressive" [])
        }


{-| CSS value [`interlace`](https://drafts.csswg.org/mediaqueries/#valdef-media-scan-interlace)

interlace: Css.Media.Interlace
-}
interlace : Elm.Expression
interlace =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "interlace"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Interlace" [])
        }


{-| Media feature [`scan`](https://drafts.csswg.org/mediaqueries/#scan).
Queries scanning process of the device. Accepts `innterlace` (some TVs) or `progressive` (most things).

scan: Css.Media.ScanningProcess a -> Css.Media.Expression
-}
scan : Elm.Expression -> Elm.Expression
scan scanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "scan"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ScanningProcess"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ scanArg ]


{-| Media feature [`grid`](https://drafts.csswg.org/mediaqueries/#grid).
Queries whether the output device is a grid or bitmap.

grid: Css.Media.Expression
-}
grid : Elm.Expression
grid =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "grid"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
        }


{-| CSS value [`slow`](https://drafts.csswg.org/mediaqueries/#valdef-media-update-slow)

slow: Css.Media.Slow
-}
slow : Elm.Expression
slow =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "slow"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Slow" [])
        }


{-| CSS value [`fast`](https://drafts.csswg.org/mediaqueries/#valdef-media-update-fast)

fast: Css.Media.Fast
-}
fast : Elm.Expression
fast =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "fast"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Fast" [])
        }


{-| Media feature [`update`](https://drafts.csswg.org/mediaqueries/#update)
The update frequency of the device. Accepts `none`, `slow`, or `fast`

update: Css.Media.UpdateFrequency a -> Css.Media.Expression
-}
update : Elm.Expression -> Elm.Expression
update updateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "UpdateFrequency"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ updateArg ]


{-| CSS value [`paged`](https://drafts.csswg.org/mediaqueries/#valdef-media-overflow-block-paged)

paged: Css.Media.Paged
-}
paged : Elm.Expression
paged =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "paged"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Paged" [])
        }


{-| CSS value [`optional-paged`](https://drafts.csswg.org/mediaqueries/#valdef-media-overflow-block-optional-paged)

optionalPaged: Css.Media.OptionalPaged
-}
optionalPaged : Elm.Expression
optionalPaged =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "optionalPaged"
        , annotation =
            Just (Type.namedWith [ "Css", "Media" ] "OptionalPaged" [])
        }


{-| Media feature [`overflow-block`](https://drafts.csswg.org/mediaqueries/#overflow-block)
Describes the behavior of the device when content overflows the initial containing block in the block axis.

overflowBlock: Css.Media.BlockAxisOverflow a -> Css.Media.Expression
-}
overflowBlock : Elm.Expression -> Elm.Expression
overflowBlock overflowBlockArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "overflowBlock"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "BlockAxisOverflow"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ overflowBlockArg ]


{-| Media feature [`overflow-inline`](https://drafts.csswg.org/mediaqueries/#overflow-inline).
Describes the behavior of the device when content overflows the initial containing block in the inline axis.

overflowInline: Css.Media.InlineAxisOverflow a -> Css.Media.Expression
-}
overflowInline : Elm.Expression -> Elm.Expression
overflowInline overflowInlineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "overflowInline"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "InlineAxisOverflow"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ overflowInlineArg ]


{-| Get a bumber of bits

    bits 8

bits: Int -> Css.Media.Bits
-}
bits : Int -> Elm.Expression
bits bitsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "bits"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Css", "Media" ] "Bits" [])
                    )
            }
        )
        [ Elm.int bitsArg ]


{-| Media Feature [`min-nncolor`](https://drafts.csswg.org/mediaqueries/#color)
Queries the user agent's bits per color channel

    media (screen (minColor (bits 256))) [ a [ Css.color (hex "D9534F") ] ]

minColor: Css.Media.Bits -> Css.Media.Expression
-}
minColor : Elm.Expression -> Elm.Expression
minColor minColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minColorArg ]


{-| Media feature [`color`](https://drafts.csswg.org/mediaqueries/#color)

    media (not color) [ body [ Css.color (hex "000000") ] ]

color: Css.Media.Expression
-}
color : Elm.Expression
color =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "color"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
        }


{-| Media feature [`max-color`](https://drafts.csswg.org/mediaqueries/#color)
Queries the user agent's bits per color channel

    media (and screen (maxColor (bits 8))) [ a [ Css.color (hex "FF0000") ] ]

maxColor: Css.Media.Bits -> Css.Media.Expression
-}
maxColor : Elm.Expression -> Elm.Expression
maxColor maxColorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxColorArg ]


{-| Media Feature [`min-monochrome`](https://drafts.csswg.org/mediaqueries/#monochrome)

minMonochrome: Css.Media.Bits -> Css.Media.Expression
-}
minMonochrome : Elm.Expression -> Elm.Expression
minMonochrome minMonochromeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minMonochrome"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minMonochromeArg ]


{-| Media feature [`monochrome`](https://drafts.csswg.org/mediaqueries/#monochrome)

    media [ monochrome ] [ body [ Css.color (hex "000000") ] ]

monochrome: Css.Media.Expression
-}
monochrome : Elm.Expression
monochrome =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "monochrome"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
        }


{-| Media feature [`max-monochrome`](https://drafts.csswg.org/mediaqueries/#monochrome)

maxMonochrome: Css.Media.Bits -> Css.Media.Expression
-}
maxMonochrome : Elm.Expression -> Elm.Expression
maxMonochrome maxMonochromeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxMonochrome"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxMonochromeArg ]


{-| Media Feature [`min-color-index`](https://drafts.csswg.org/mediaqueries/nn#color-index)
Queries the number of colors in the user agent's color lookup table.

    media (and screen (minColorIndex (int 16777216))) [ a [ Css.color (hex "D9534F") ] ]

minColorIndex: Css.Structure.Number a -> Css.Media.Expression
-}
minColorIndex : Elm.Expression -> Elm.Expression
minColorIndex minColorIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minColorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ minColorIndexArg ]


{-| Media feature [`color-index`](https://drafts.csswg.org/mediaqueries/#color-index)
Queries the number of colors in the user agent's color lookup table.

    media (and screen (colorIndex (int 16777216))) [ a [ Css.color (hex "D9534F") ] ]

colorIndex: Css.Structure.Number a -> Css.Media.Expression
-}
colorIndex : Elm.Expression -> Elm.Expression
colorIndex colorIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "colorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ colorIndexArg ]


{-| Media feature [`max-color-index`](https://drafts.csswg.org/mediaqueries/#color-index).
Queries the number of colors in the user agent's color lookup table.

    media (and screen (maxColorIndex (int 256))) [ a [ Css.color (hex "FF0000") ] ]

maxColorIndex: Css.Structure.Number a -> Css.Media.Expression
-}
maxColorIndex : Elm.Expression -> Elm.Expression
maxColorIndex maxColorIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxColorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ maxColorIndexArg ]


{-| CSS value [`srgb`](https://drafts.csswg.org/mediaqueries/#valdef-media-color-gamut-srgb)

srgb: Css.Media.SRGB
-}
srgb : Elm.Expression
srgb =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "srgb"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "SRGB" [])
        }


{-| CSS value [`p3`](https://drafts.csswg.org/mediaqueries/#valdef-media-color-gamut-p3)

p3: Css.Media.P3
-}
p3 : Elm.Expression
p3 =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "p3"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "P3" [])
        }


{-| CSS value [`rec2020`](https://drafts.csswg.org/mediaqueries/#valdef-media-color-gamut-rec2020)

rec2020: Css.Media.Rec2020
-}
rec2020 : Elm.Expression
rec2020 =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "rec2020"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Rec2020" [])
        }


{-| Media feature [`color-gamut`](https://drafts.csswg.org/mediaqueries/#color-gamut).
Describes the approximate range of colors supported by the user agent and device.

    media (and screen (colorGamut rec2020)) [ Css.class HiColorImg [ display block ] ]

colorGamut: Css.Media.ColorGamut a -> Css.Media.Expression
-}
colorGamut : Elm.Expression -> Elm.Expression
colorGamut colorGamutArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "colorGamut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ColorGamut"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ colorGamutArg ]


{-| CSS Value [`fine`](https://drafts.csswg.org/mediaqueries/#valdef-media-pointer-fine)

fine: Css.Media.Fine
-}
fine : Elm.Expression
fine =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "fine"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Fine" [])
        }


{-| CSS Value [`coarse`](https://drafts.csswg.org/mediaqueries/#valdef-media-pointer-coarse)

coarse: Css.Media.Coarse
-}
coarse : Elm.Expression
coarse =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "coarse"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Coarse" [])
        }


{-| Media feature [`pointer`](https://drafts.csswg.org/mediaqueries/#pointer)
Queries the presence and accuracy of a pointing device, such as a mouse, touchscreen, or Wii remote.
Reflects the capabilities of the primary input mechanism.
Accepts `none`, `fine`, and `coarse`.

    media (Media.pointer coarse) [ a [ display block, Css.height (px 24) ] ]

pointer: Css.Media.PointerDevice a -> Css.Media.Expression
-}
pointer : Elm.Expression -> Elm.Expression
pointer pointerArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "pointer"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "PointerDevice"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ pointerArg ]


{-| Media feature [`any-pointer`](https://drafts.csswg.org/mediaqueries/#any-input)
Queries the presence and accuracy of a pointing device, such as a mouse, touchscreen, or Wii remote.
Reflects the capabilities of the most capable input mechanism.
Accepts `none`, `fine`, and `coarse`.

    media (anyPointer coarse) [ a [ display block, Css.height (px 24) ] ]

anyPointer: Css.Media.PointerDevice a -> Css.Media.Expression
-}
anyPointer : Elm.Expression -> Elm.Expression
anyPointer anyPointerArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "anyPointer"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "PointerDevice"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ anyPointerArg ]


{-| The value [`hover`](https://drafts.csswg.org/mediaqueries/#valdef-media-hover-hover).
Named `canHover` to avoid conflict with the media feature of the same name

canHover: Css.Media.CanHover
-}
canHover : Elm.Expression
canHover =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "canHover"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "CanHover" [])
        }


{-| Media feature [`hover`](https://drafts.csswg.org/mediaqueries/#hover).
Queries the if the user agent's primary input mechanism has the ability to hover over elements.
Accepts `none` or `canHover`.

    media (Media.hover canHover) [ a [ Css.hover [ textDecoration underline ] ] ]

hover: Css.Media.HoverCapability a -> Css.Media.Expression
-}
hover : Elm.Expression -> Elm.Expression
hover hoverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "hover"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "HoverCapability"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ hoverArg ]


{-| Media feature [`any-hover`](https://drafts.csswg.org/mediaqueries/#any-input)
Queries the if any of user agent's input mechanisms have the ability to hover over elements
Accepts `none` or `canHover`.

    media (anyHover canHover) [ a [ Css.hover [ textDecoration underline ] ] ]

anyHover: Css.Media.HoverCapability a -> Css.Media.Expression
-}
anyHover : Elm.Expression -> Elm.Expression
anyHover anyHoverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "anyHover"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "HoverCapability"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ anyHoverArg ]


{-| CSS value [`initial-only`](https://drafts.csswg.org/mediaqueries/#valdef-media-scripting-initial-only).

initialOnly: Css.Media.InitialOnly
-}
initialOnly : Elm.Expression
initialOnly =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "initialOnly"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "InitialOnly" [])
        }


{-| CSS value [`enabled`](https://drafts.csswg.org/mediaqueries/#valdef-media-scripting-enabled).

enabled: Css.Media.Enabled
-}
enabled : Elm.Expression
enabled =
    Elm.value
        { importFrom = [ "Css", "Media" ]
        , name = "enabled"
        , annotation = Just (Type.namedWith [ "Css", "Media" ] "Enabled" [])
        }


{-| The [`scripting`](https://drafts.csswg.org/mediaqueries/#scripting) media feature
for querying the user agents support for scripting languages like JavaScript.
Accepts `none`, `initialOnly`, and `enabled`.

    media (scripting none) [ Css.class NoScript [ display block ] ]

scripting: Css.Media.ScriptingSupport a -> Css.Media.Expression
-}
scripting : Elm.Expression -> Elm.Expression
scripting scriptingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "scripting"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ScriptingSupport"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
        )
        [ scriptingArg ]


annotation_ :
    { mediaQuery : Type.Annotation
    , mediaType : Type.Annotation
    , expression : Type.Annotation
    , ratio : Type.Annotation
    , landscape : Type.Annotation
    , portrait : Type.Annotation
    , resolution : Type.Annotation
    , progressive : Type.Annotation
    , interlace : Type.Annotation
    , slow : Type.Annotation
    , fast : Type.Annotation
    , paged : Type.Annotation
    , optionalPaged : Type.Annotation
    , bits : Type.Annotation
    , sRGB : Type.Annotation
    , p3 : Type.Annotation
    , rec2020 : Type.Annotation
    , fine : Type.Annotation
    , coarse : Type.Annotation
    , canHover : Type.Annotation
    , initialOnly : Type.Annotation
    , enabled : Type.Annotation
    }
annotation_ =
    { mediaQuery =
        Type.alias
            moduleName_
            "MediaQuery"
            []
            (Type.namedWith [ "Css", "Structure" ] "MediaQuery" [])
    , mediaType =
        Type.alias
            moduleName_
            "MediaType"
            []
            (Type.namedWith [ "Css", "Structure" ] "MediaType" [])
    , expression =
        Type.alias
            moduleName_
            "Expression"
            []
            (Type.namedWith [ "Css", "Structure" ] "MediaExpression" [])
    , ratio =
        Type.alias
            moduleName_
            "Ratio"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "ratio"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , landscape =
        Type.alias
            moduleName_
            "Landscape"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "orientation"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , portrait =
        Type.alias
            moduleName_
            "Portrait"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "orientation"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , resolution =
        Type.alias
            moduleName_
            "Resolution"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "resolution"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , progressive =
        Type.alias
            moduleName_
            "Progressive"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "scanningProcess"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , interlace =
        Type.alias
            moduleName_
            "Interlace"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "scanningProcess"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , slow =
        Type.alias
            moduleName_
            "Slow"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "updateFrequency"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , fast =
        Type.alias
            moduleName_
            "Fast"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "updateFrequency"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , paged =
        Type.alias
            moduleName_
            "Paged"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "blockAxisOverflow"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , optionalPaged =
        Type.alias
            moduleName_
            "OptionalPaged"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "blockAxisOverflow"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , bits =
        Type.alias
            moduleName_
            "Bits"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "bits"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , sRGB =
        Type.alias
            moduleName_
            "SRGB"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "colorGamut"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , p3 =
        Type.alias
            moduleName_
            "P3"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "colorGamut"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , rec2020 =
        Type.alias
            moduleName_
            "Rec2020"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "colorGamut"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , fine =
        Type.alias
            moduleName_
            "Fine"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "pointerDevice"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , coarse =
        Type.alias
            moduleName_
            "Coarse"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "pointerDevice"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , canHover =
        Type.alias
            moduleName_
            "CanHover"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "hoverCapability"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , initialOnly =
        Type.alias
            moduleName_
            "InitialOnly"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "scriptingSupport"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    , enabled =
        Type.alias
            moduleName_
            "Enabled"
            []
            (Type.record
                [ ( "value", Type.string )
                , ( "scriptingSupport"
                  , Type.namedWith [ "Css", "Structure" ] "Compatible" []
                  )
                ]
            )
    }


make_ :
    { ratio :
        { value : Elm.Expression, ratio : Elm.Expression } -> Elm.Expression
    , landscape :
        { value : Elm.Expression, orientation : Elm.Expression }
        -> Elm.Expression
    , portrait :
        { value : Elm.Expression, orientation : Elm.Expression }
        -> Elm.Expression
    , resolution :
        { value : Elm.Expression, resolution : Elm.Expression }
        -> Elm.Expression
    , progressive :
        { value : Elm.Expression, scanningProcess : Elm.Expression }
        -> Elm.Expression
    , interlace :
        { value : Elm.Expression, scanningProcess : Elm.Expression }
        -> Elm.Expression
    , slow :
        { value : Elm.Expression, updateFrequency : Elm.Expression }
        -> Elm.Expression
    , fast :
        { value : Elm.Expression, updateFrequency : Elm.Expression }
        -> Elm.Expression
    , paged :
        { value : Elm.Expression, blockAxisOverflow : Elm.Expression }
        -> Elm.Expression
    , optionalPaged :
        { value : Elm.Expression, blockAxisOverflow : Elm.Expression }
        -> Elm.Expression
    , bits : { value : Elm.Expression, bits : Elm.Expression } -> Elm.Expression
    , sRGB :
        { value : Elm.Expression, colorGamut : Elm.Expression }
        -> Elm.Expression
    , p3 :
        { value : Elm.Expression, colorGamut : Elm.Expression }
        -> Elm.Expression
    , rec2020 :
        { value : Elm.Expression, colorGamut : Elm.Expression }
        -> Elm.Expression
    , fine :
        { value : Elm.Expression, pointerDevice : Elm.Expression }
        -> Elm.Expression
    , coarse :
        { value : Elm.Expression, pointerDevice : Elm.Expression }
        -> Elm.Expression
    , canHover :
        { value : Elm.Expression, hoverCapability : Elm.Expression }
        -> Elm.Expression
    , initialOnly :
        { value : Elm.Expression, scriptingSupport : Elm.Expression }
        -> Elm.Expression
    , enabled :
        { value : Elm.Expression, scriptingSupport : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { ratio =
        \ratio_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Ratio"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "ratio"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" ratio_args.value
                    , Tuple.pair "ratio" ratio_args.ratio
                    ]
                )
    , landscape =
        \landscape_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Landscape"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "orientation"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" landscape_args.value
                    , Tuple.pair "orientation" landscape_args.orientation
                    ]
                )
    , portrait =
        \portrait_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Portrait"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "orientation"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" portrait_args.value
                    , Tuple.pair "orientation" portrait_args.orientation
                    ]
                )
    , resolution =
        \resolution_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Resolution"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "resolution"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" resolution_args.value
                    , Tuple.pair "resolution" resolution_args.resolution
                    ]
                )
    , progressive =
        \progressive_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Progressive"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "scanningProcess"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" progressive_args.value
                    , Tuple.pair
                        "scanningProcess"
                        progressive_args.scanningProcess
                    ]
                )
    , interlace =
        \interlace_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Interlace"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "scanningProcess"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" interlace_args.value
                    , Tuple.pair
                        "scanningProcess"
                        interlace_args.scanningProcess
                    ]
                )
    , slow =
        \slow_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Slow"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "updateFrequency"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" slow_args.value
                    , Tuple.pair "updateFrequency" slow_args.updateFrequency
                    ]
                )
    , fast =
        \fast_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Fast"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "updateFrequency"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" fast_args.value
                    , Tuple.pair "updateFrequency" fast_args.updateFrequency
                    ]
                )
    , paged =
        \paged_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Paged"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "blockAxisOverflow"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" paged_args.value
                    , Tuple.pair
                        "blockAxisOverflow"
                        paged_args.blockAxisOverflow
                    ]
                )
    , optionalPaged =
        \optionalPaged_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "OptionalPaged"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "blockAxisOverflow"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" optionalPaged_args.value
                    , Tuple.pair
                        "blockAxisOverflow"
                        optionalPaged_args.blockAxisOverflow
                    ]
                )
    , bits =
        \bits_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Bits"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "bits"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" bits_args.value
                    , Tuple.pair "bits" bits_args.bits
                    ]
                )
    , sRGB =
        \sRGB_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "SRGB"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "colorGamut"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" sRGB_args.value
                    , Tuple.pair "colorGamut" sRGB_args.colorGamut
                    ]
                )
    , p3 =
        \p3_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "P3"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "colorGamut"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" p3_args.value
                    , Tuple.pair "colorGamut" p3_args.colorGamut
                    ]
                )
    , rec2020 =
        \rec2020_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Rec2020"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "colorGamut"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" rec2020_args.value
                    , Tuple.pair "colorGamut" rec2020_args.colorGamut
                    ]
                )
    , fine =
        \fine_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Fine"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "pointerDevice"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" fine_args.value
                    , Tuple.pair "pointerDevice" fine_args.pointerDevice
                    ]
                )
    , coarse =
        \coarse_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Coarse"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "pointerDevice"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" coarse_args.value
                    , Tuple.pair "pointerDevice" coarse_args.pointerDevice
                    ]
                )
    , canHover =
        \canHover_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "CanHover"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "hoverCapability"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" canHover_args.value
                    , Tuple.pair "hoverCapability" canHover_args.hoverCapability
                    ]
                )
    , initialOnly =
        \initialOnly_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "InitialOnly"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "scriptingSupport"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" initialOnly_args.value
                    , Tuple.pair
                        "scriptingSupport"
                        initialOnly_args.scriptingSupport
                    ]
                )
    , enabled =
        \enabled_args ->
            Elm.withType
                (Type.alias
                    [ "Css", "Media" ]
                    "Enabled"
                    []
                    (Type.record
                        [ ( "value", Type.string )
                        , ( "scriptingSupport"
                          , Type.namedWith
                                [ "Css", "Structure" ]
                                "Compatible"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" enabled_args.value
                    , Tuple.pair
                        "scriptingSupport"
                        enabled_args.scriptingSupport
                    ]
                )
    }


call_ :
    { withMedia : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withMediaQuery : Elm.Expression -> Elm.Expression -> Elm.Expression
    , all : Elm.Expression -> Elm.Expression
    , only : Elm.Expression -> Elm.Expression -> Elm.Expression
    , not : Elm.Expression -> Elm.Expression -> Elm.Expression
    , minWidth : Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , maxWidth : Elm.Expression -> Elm.Expression
    , minHeight : Elm.Expression -> Elm.Expression
    , height : Elm.Expression -> Elm.Expression
    , maxHeight : Elm.Expression -> Elm.Expression
    , ratio : Elm.Expression -> Elm.Expression -> Elm.Expression
    , minAspectRatio : Elm.Expression -> Elm.Expression
    , aspectRatio : Elm.Expression -> Elm.Expression
    , maxAspectRatio : Elm.Expression -> Elm.Expression
    , orientation : Elm.Expression -> Elm.Expression
    , dpi : Elm.Expression -> Elm.Expression
    , dpcm : Elm.Expression -> Elm.Expression
    , dppx : Elm.Expression -> Elm.Expression
    , minResolution : Elm.Expression -> Elm.Expression
    , resolution : Elm.Expression -> Elm.Expression
    , maxResolution : Elm.Expression -> Elm.Expression
    , scan : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression
    , overflowBlock : Elm.Expression -> Elm.Expression
    , overflowInline : Elm.Expression -> Elm.Expression
    , bits : Elm.Expression -> Elm.Expression
    , minColor : Elm.Expression -> Elm.Expression
    , maxColor : Elm.Expression -> Elm.Expression
    , minMonochrome : Elm.Expression -> Elm.Expression
    , maxMonochrome : Elm.Expression -> Elm.Expression
    , minColorIndex : Elm.Expression -> Elm.Expression
    , colorIndex : Elm.Expression -> Elm.Expression
    , maxColorIndex : Elm.Expression -> Elm.Expression
    , colorGamut : Elm.Expression -> Elm.Expression
    , pointer : Elm.Expression -> Elm.Expression
    , anyPointer : Elm.Expression -> Elm.Expression
    , hover : Elm.Expression -> Elm.Expression
    , anyHover : Elm.Expression -> Elm.Expression
    , scripting : Elm.Expression -> Elm.Expression
    }
call_ =
    { withMedia =
        \withMediaArg withMediaArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "withMedia"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Media" ]
                                        "MediaQuery"
                                        []
                                    )
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ withMediaArg, withMediaArg0 ]
    , withMediaQuery =
        \withMediaQueryArg withMediaQueryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "withMediaQuery"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith [ "Css" ] "Style" [])
                            )
                    }
                )
                [ withMediaQueryArg, withMediaQueryArg0 ]
    , all =
        \allArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "all"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Media" ]
                                        "Expression"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaQuery"
                                    []
                                )
                            )
                    }
                )
                [ allArg ]
    , only =
        \onlyArg onlyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "only"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaType"
                                    []
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Media" ]
                                        "Expression"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaQuery"
                                    []
                                )
                            )
                    }
                )
                [ onlyArg, onlyArg0 ]
    , not =
        \notArg notArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "not"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaType"
                                    []
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Media" ]
                                        "Expression"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "MediaQuery"
                                    []
                                )
                            )
                    }
                )
                [ notArg, notArg0 ]
    , minWidth =
        \minWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minWidthArg ]
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "width"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ widthArg ]
    , maxWidth =
        \maxWidthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxWidth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxWidthArg ]
    , minHeight =
        \minHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minHeightArg ]
    , height =
        \heightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "height"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ heightArg ]
    , maxHeight =
        \maxHeightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxHeight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "AbsoluteLength"
                                    [ Type.var "compatible" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxHeightArg ]
    , ratio =
        \ratioArg ratioArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "ratio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith [ "Css", "Media" ] "Ratio" [])
                            )
                    }
                )
                [ ratioArg, ratioArg0 ]
    , minAspectRatio =
        \minAspectRatioArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minAspectRatio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minAspectRatioArg ]
    , aspectRatio =
        \aspectRatioArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "aspectRatio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ aspectRatioArg ]
    , maxAspectRatio =
        \maxAspectRatioArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxAspectRatio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxAspectRatioArg ]
    , orientation =
        \orientationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "orientation"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "Orientation"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ orientationArg ]
    , dpi =
        \dpiArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "dpi"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                )
                            )
                    }
                )
                [ dpiArg ]
    , dpcm =
        \dpcmArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "dpcm"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                )
                            )
                    }
                )
                [ dpcmArg ]
    , dppx =
        \dppxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "dppx"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                )
                            )
                    }
                )
                [ dppxArg ]
    , minResolution =
        \minResolutionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minResolution"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minResolutionArg ]
    , resolution =
        \resolutionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "resolution"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ resolutionArg ]
    , maxResolution =
        \maxResolutionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxResolution"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "Resolution"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxResolutionArg ]
    , scan =
        \scanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "scan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "ScanningProcess"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ scanArg ]
    , update =
        \updateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "update"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "UpdateFrequency"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ updateArg ]
    , overflowBlock =
        \overflowBlockArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "overflowBlock"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "BlockAxisOverflow"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ overflowBlockArg ]
    , overflowInline =
        \overflowInlineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "overflowInline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "InlineAxisOverflow"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ overflowInlineArg ]
    , bits =
        \bitsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "bits"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Css", "Media" ] "Bits" [])
                            )
                    }
                )
                [ bitsArg ]
    , minColor =
        \minColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minColorArg ]
    , maxColor =
        \maxColorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxColor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxColorArg ]
    , minMonochrome =
        \minMonochromeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minMonochrome"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minMonochromeArg ]
    , maxMonochrome =
        \maxMonochromeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxMonochrome"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxMonochromeArg ]
    , minColorIndex =
        \minColorIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "minColorIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Number"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ minColorIndexArg ]
    , colorIndex =
        \colorIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "colorIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Number"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ colorIndexArg ]
    , maxColorIndex =
        \maxColorIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "maxColorIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Structure" ]
                                    "Number"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ maxColorIndexArg ]
    , colorGamut =
        \colorGamutArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "colorGamut"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "ColorGamut"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ colorGamutArg ]
    , pointer =
        \pointerArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "pointer"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "PointerDevice"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ pointerArg ]
    , anyPointer =
        \anyPointerArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "anyPointer"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "PointerDevice"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ anyPointerArg ]
    , hover =
        \hoverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "hover"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "HoverCapability"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ hoverArg ]
    , anyHover =
        \anyHoverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "anyHover"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "HoverCapability"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ anyHoverArg ]
    , scripting =
        \scriptingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Media" ]
                    , name = "scripting"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Css", "Media" ]
                                    "ScriptingSupport"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Css", "Media" ]
                                    "Expression"
                                    []
                                )
                            )
                    }
                )
                [ scriptingArg ]
    }


values_ :
    { withMedia : Elm.Expression
    , withMediaQuery : Elm.Expression
    , all : Elm.Expression
    , only : Elm.Expression
    , not : Elm.Expression
    , screen : Elm.Expression
    , print : Elm.Expression
    , speech : Elm.Expression
    , minWidth : Elm.Expression
    , width : Elm.Expression
    , maxWidth : Elm.Expression
    , minHeight : Elm.Expression
    , height : Elm.Expression
    , maxHeight : Elm.Expression
    , ratio : Elm.Expression
    , minAspectRatio : Elm.Expression
    , aspectRatio : Elm.Expression
    , maxAspectRatio : Elm.Expression
    , landscape : Elm.Expression
    , portrait : Elm.Expression
    , orientation : Elm.Expression
    , dpi : Elm.Expression
    , dpcm : Elm.Expression
    , dppx : Elm.Expression
    , minResolution : Elm.Expression
    , resolution : Elm.Expression
    , maxResolution : Elm.Expression
    , progressive : Elm.Expression
    , interlace : Elm.Expression
    , scan : Elm.Expression
    , grid : Elm.Expression
    , slow : Elm.Expression
    , fast : Elm.Expression
    , update : Elm.Expression
    , paged : Elm.Expression
    , optionalPaged : Elm.Expression
    , overflowBlock : Elm.Expression
    , overflowInline : Elm.Expression
    , bits : Elm.Expression
    , minColor : Elm.Expression
    , color : Elm.Expression
    , maxColor : Elm.Expression
    , minMonochrome : Elm.Expression
    , monochrome : Elm.Expression
    , maxMonochrome : Elm.Expression
    , minColorIndex : Elm.Expression
    , colorIndex : Elm.Expression
    , maxColorIndex : Elm.Expression
    , srgb : Elm.Expression
    , p3 : Elm.Expression
    , rec2020 : Elm.Expression
    , colorGamut : Elm.Expression
    , fine : Elm.Expression
    , coarse : Elm.Expression
    , pointer : Elm.Expression
    , anyPointer : Elm.Expression
    , canHover : Elm.Expression
    , hover : Elm.Expression
    , anyHover : Elm.Expression
    , initialOnly : Elm.Expression
    , enabled : Elm.Expression
    , scripting : Elm.Expression
    }
values_ =
    { withMedia =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "withMedia"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , withMediaQuery =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "withMediaQuery"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css" ] "Style" [])
                    )
            }
    , all =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
    , only =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "only"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaType" []
                        , Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
    , not =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "not"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "MediaType" []
                        , Type.list
                            (Type.namedWith [ "Css", "Media" ] "Expression" [])
                        ]
                        (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                    )
            }
    , screen =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "screen"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
            }
    , print =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "print"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
            }
    , speech =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "speech"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "MediaType" [])
            }
    , minWidth =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , width =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , maxWidth =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxWidth"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , minHeight =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , height =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , maxHeight =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxHeight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "AbsoluteLength"
                            [ Type.var "compatible" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , ratio =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "ratio"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Css", "Media" ] "Ratio" [])
                    )
            }
    , minAspectRatio =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , aspectRatio =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "aspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , maxAspectRatio =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxAspectRatio"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Ratio" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , landscape =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "landscape"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Landscape" [])
            }
    , portrait =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "portrait"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Portrait" [])
            }
    , orientation =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "orientation"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "Orientation"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , dpi =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dpi"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
    , dpcm =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dpcm"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
    , dppx =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "dppx"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Css", "Media" ] "Resolution" [])
                    )
            }
    , minResolution =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minResolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , resolution =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "resolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , maxResolution =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxResolution"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Resolution" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , progressive =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "progressive"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Progressive" [])
            }
    , interlace =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "interlace"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Interlace" [])
            }
    , scan =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "scan"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ScanningProcess"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , grid =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "grid"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
            }
    , slow =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "slow"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Slow" [])
            }
    , fast =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "fast"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Fast" [])
            }
    , update =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "UpdateFrequency"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , paged =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "paged"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Paged" [])
            }
    , optionalPaged =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "optionalPaged"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "OptionalPaged" [])
            }
    , overflowBlock =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "overflowBlock"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "BlockAxisOverflow"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , overflowInline =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "overflowInline"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "InlineAxisOverflow"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , bits =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "bits"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Css", "Media" ] "Bits" [])
                    )
            }
    , minColor =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , color =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "color"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
            }
    , maxColor =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxColor"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , minMonochrome =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minMonochrome"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , monochrome =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "monochrome"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "Expression" [])
            }
    , maxMonochrome =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxMonochrome"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Css", "Media" ] "Bits" [] ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , minColorIndex =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "minColorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , colorIndex =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "colorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , maxColorIndex =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "maxColorIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Structure" ]
                            "Number"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , srgb =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "srgb"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "SRGB" [])
            }
    , p3 =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "p3"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "P3" [])
            }
    , rec2020 =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "rec2020"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Rec2020" [])
            }
    , colorGamut =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "colorGamut"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ColorGamut"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , fine =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "fine"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Fine" [])
            }
    , coarse =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "coarse"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Coarse" [])
            }
    , pointer =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "pointer"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "PointerDevice"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , anyPointer =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "anyPointer"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "PointerDevice"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , canHover =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "canHover"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "CanHover" [])
            }
    , hover =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "hover"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "HoverCapability"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , anyHover =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "anyHover"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "HoverCapability"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    , initialOnly =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "initialOnly"
            , annotation =
                Just (Type.namedWith [ "Css", "Media" ] "InitialOnly" [])
            }
    , enabled =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "enabled"
            , annotation = Just (Type.namedWith [ "Css", "Media" ] "Enabled" [])
            }
    , scripting =
        Elm.value
            { importFrom = [ "Css", "Media" ]
            , name = "scripting"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Css", "Media" ]
                            "ScriptingSupport"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Css", "Media" ] "Expression" [])
                    )
            }
    }


