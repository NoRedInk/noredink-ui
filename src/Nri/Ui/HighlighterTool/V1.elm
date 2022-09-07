module Nri.Ui.HighlighterTool.V1 exposing
    ( Tool(..), EraserModel, MarkerModel
    , buildMarker
    )

{-|

@docs Tool, EraserModel, MarkerModel
@docs buildMarker

-}

import Css
import Nri.Ui.Colors.V1 as Colors


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
    , rounded : Bool
    }
    -> MarkerModel kind
buildMarker { highlightColor, hoverColor, hoverHighlightColor, kind, rounded, name } =
    { hoverClass = hoverStyles hoverColor
    , hintClass = hoverStyles hoverColor
    , startGroupClass = startGroupStyles rounded
    , endGroupClass = endGroupStyles rounded
    , highlightClass = highlightStyles highlightColor
    , hoverHighlightClass = highlightStyles hoverHighlightColor
    , kind = kind
    , name = name
    }


startGroupStyles : Bool -> List Css.Style
startGroupStyles rounded =
    Css.paddingLeft (Css.px 4)
        :: (if rounded then
                [ Css.borderTopLeftRadius (Css.px 4)
                , Css.borderBottomLeftRadius (Css.px 4)
                ]

            else
                []
           )


endGroupStyles : Bool -> List Css.Style
endGroupStyles rounded =
    Css.paddingRight (Css.px 4)
        :: (if rounded then
                [ Css.borderTopRightRadius (Css.px 4)
                , Css.borderBottomRightRadius (Css.px 4)
                ]

            else
                []
           )


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
    ]


hoverStyles : Css.Color -> List Css.Style
hoverStyles color =
    List.append
        sharedStyles
        [ Css.important (Css.backgroundColor color)

        -- The Highlighter applies both these styles and the startGroup and
        -- endGroup styles. Here we disable the left and the right padding
        -- because otherwise it would cause the text to move around.
        , Css.important (Css.paddingLeft Css.zero)
        , Css.important (Css.paddingRight Css.zero)
        ]
