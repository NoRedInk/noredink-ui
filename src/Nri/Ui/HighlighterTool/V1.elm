module Nri.Ui.HighlighterTool.V1 exposing
    ( Tool(..)
    , EraserModel, buildEraser
    , MarkerModel, buildMarker, buildMarkerWithBorder, buildMarkerWithoutRounding
    )

{-|


### Patch changes

  - change the high-contrast styles to be border-based instead of background-color based
  - adds buildMarkerWithoutRounding for inline comment styling
  - changes the 4px of side padding on the marker to 2px of padding+negative margin

@docs Tool
@docs EraserModel, buildEraser
@docs MarkerModel, buildMarker, buildMarkerWithBorder, buildMarkerWithoutRounding

-}

import Css
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-| Tool that can be used on a highlighter
-}
type Tool kind
    = Marker (MarkerModel kind)
    | Eraser EraserModel


{-| EraserModel
-}
type alias EraserModel =
    { hoverClass : List Css.Style
    , hintClass : List Css.Style
    , startGroupClass : List Css.Style
    , endGroupClass : List Css.Style
    }


{-| The default eraser.
-}
buildEraser : EraserModel
buildEraser =
    let
        eraserStyles : List Css.Style
        eraserStyles =
            [ Css.opacity (Css.num 0.4) ]
    in
    { hoverClass = eraserStyles
    , hintClass = eraserStyles
    , startGroupClass = eraserStyles
    , endGroupClass = eraserStyles
    }


{-| A marker is used to highlight highlightables.
A user can define a `kind`, which contains the meaning of a selection.
Marker is also used to define styling of a selection.
-}
type alias MarkerModel kind =
    { hoverClass : List Css.Style
    , hintClass : List Css.Style
    , startGroupClass : List Css.Style
    , endGroupClass : List Css.Style
    , highlightClass : List Css.Style
    , hoverHighlightClass : List Css.Style
    , kind : kind
    , name : Maybe String
    }


{-| -}
buildMarker :
    { highlightColor : Css.Color
    , hoverColor : Css.Color
    , hoverHighlightColor : Css.Color
    , kind : kind
    , name : Maybe String
    }
    -> MarkerModel kind
buildMarker { highlightColor, hoverColor, hoverHighlightColor, kind, name } =
    { hoverClass = hoverStyles hoverColor
    , hintClass = hoverStyles hoverColor
    , startGroupClass = startGroupStyles
    , endGroupClass = endGroupStyles
    , highlightClass = highlightStyles highlightColor
    , hoverHighlightClass = highlightStyles hoverHighlightColor
    , kind = kind
    , name = name
    }


startGroupStyles : List Css.Style
startGroupStyles =
    [ MediaQuery.highContrastMode
        [ Css.property "border-left" "2px solid Mark"
        ]
    , Css.paddingLeft (Css.px paddingSize)
    , Css.marginLeft (Css.px -paddingSize)
    , Css.borderTopLeftRadius (Css.px 4)
    , Css.borderBottomLeftRadius (Css.px 4)
    ]


endGroupStyles : List Css.Style
endGroupStyles =
    [ MediaQuery.highContrastMode
        [ Css.property "border-right" "2px solid Mark"
        ]
    , Css.paddingRight (Css.px paddingSize)
    , Css.marginRight (Css.px -paddingSize)
    , Css.borderTopRightRadius (Css.px 4)
    , Css.borderBottomRightRadius (Css.px 4)
    ]


paddingSize : Float
paddingSize =
    2


highlightStyles : Css.Color -> List Css.Style
highlightStyles color =
    List.append
        sharedStyles
        [ Css.backgroundColor color
        , Css.boxShadow5 Css.zero (Css.px 1) Css.zero Css.zero Colors.gray75
        ]


sharedStyles : List Css.Style
sharedStyles =
    [ Css.paddingTop (Css.px 4)
    , Css.paddingBottom (Css.px 3)
    , Css.property "transition" "background-color 0.4s, box-shadow 0.4s"
    , MediaQuery.highContrastMode
        [ Css.property "color" "CanvasText"
        , Css.property "border-top" "2px solid Mark"
        , Css.property "border-bottom" "2px solid Mark"
        ]
    ]


hoverStyles : Css.Color -> List Css.Style
hoverStyles color =
    List.append
        sharedStyles
        [ Css.boxShadow5 Css.zero Css.zero (Css.px 10) (Css.px 2) color
        , Css.important (Css.backgroundColor color)
        , MediaQuery.highContrastMode
            [ Css.property "border-color" "Highlight" |> Css.important
            ]
        ]


{-| Typically, this marker is only used for static highlighters.
-}
buildMarkerWithBorder :
    { highlightColor : Css.Color
    , kind : kind
    , name : Maybe String
    }
    -> MarkerModel kind
buildMarkerWithBorder { highlightColor, kind, name } =
    let
        sharedStylesWithBorder =
            Css.batch
                [ Css.padding2 (Css.px 6) Css.zero
                , Css.lineHeight (Css.em 2.5)
                , MediaQuery.highContrastMode
                    [ Css.property "border-color" "Mark"
                    , Css.property "color" "CanvasText"
                    , Css.borderWidth (Css.px 2)
                    ]
                ]
    in
    { hoverClass = []
    , hintClass = []
    , startGroupClass =
        [ sharedStylesWithBorder
        , Css.borderBottomLeftRadius (Css.px 8)
        , Css.borderTopLeftRadius (Css.px 8)
        , Css.borderTop3 (Css.px 1) Css.solid Colors.gray45
        , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray45
        , Css.borderLeft3 (Css.px 1) Css.solid Colors.gray45
        ]
    , endGroupClass =
        [ sharedStylesWithBorder
        , Css.borderBottomRightRadius (Css.px 8)
        , Css.borderTopRightRadius (Css.px 8)
        , Css.borderTop3 (Css.px 1) Css.solid Colors.gray45
        , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray45
        , Css.borderRight3 (Css.px 1) Css.solid Colors.gray45
        ]
    , highlightClass =
        [ sharedStylesWithBorder
        , Css.backgroundColor highlightColor
        , Css.borderTop3 (Css.px 1) Css.solid Colors.gray45
        , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray45
        ]
    , hoverHighlightClass = []
    , kind = kind
    , name = name
    }


{-| Typically, this style of marker is used for inline comments.
-}
buildMarkerWithoutRounding :
    { highlightColor : Css.Color
    , hoverColor : Css.Color
    , hoverHighlightColor : Css.Color
    , kind : kind
    , name : Maybe String
    }
    -> MarkerModel kind
buildMarkerWithoutRounding { highlightColor, hoverColor, hoverHighlightColor, kind, name } =
    { hoverClass = squareHoverStyles hoverColor
    , hintClass = squareHoverStyles hoverColor
    , startGroupClass =
        [ MediaQuery.highContrastMode
            [ Css.property "border-left" "2px solid Mark"
            ]
        ]
    , endGroupClass =
        [ MediaQuery.highContrastMode
            [ Css.property "border-right" "2px solid Mark"
            ]
        ]
    , highlightClass = squareHighlightStyles highlightColor
    , hoverHighlightClass = squareHighlightStyles hoverHighlightColor
    , kind = kind
    , name = name
    }


squareHighlightStyles : Css.Color -> List Css.Style
squareHighlightStyles color =
    List.append
        squareSharedStyles
        [ Css.backgroundColor color
        , Css.boxShadow5 Css.zero (Css.px 1) Css.zero Css.zero Colors.gray75
        ]


squareSharedStyles : List Css.Style
squareSharedStyles =
    [ Css.paddingTop (Css.px 4)
    , Css.paddingBottom (Css.px 3)
    , MediaQuery.highContrastMode
        [ Css.property "color" "CanvasText"
        , Css.property "border-top" "2px solid Mark"
        , Css.property "border-bottom" "2px solid Mark"
        ]
    ]


squareHoverStyles : Css.Color -> List Css.Style
squareHoverStyles color =
    List.append squareSharedStyles [ Css.important (Css.backgroundColor color) ]
