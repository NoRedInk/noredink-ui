module Nri.Outline.Types exposing (..)

{-|

@docs BaseConfig, CustomConfig, Evaluation, ExtraContenRowConfig, OutlineLayout, OutlinePanelConfig, RowConfig, RowModifier, RowVerticalAlignment, RowsContent, Hierarchy

-}

import Html exposing (Html)
import Nri.Palette as Palette


{-| -}
type alias BaseConfig msg extras =
    { extras
        | content : Html msg
        , palette : Palette.Palette
    }


{-| -}
type alias CustomConfig msg =
    BaseConfig msg
        (RowsContent msg
            { verticalAlign : RowVerticalAlignment
            , extraContent : Maybe (Html msg)
            , modifyRow : RowModifier
            , hasDashedBorder : Bool
            }
        )


{-| -}
type alias RowConfig msg =
    BaseConfig msg (RowsContent msg {})


{-| -}
type alias RowsContent msg extras =
    { extras | rows : List (OutlineLayout msg) }


{-| -}
type alias ExtraContenRowConfig msg =
    BaseConfig msg
        { rows : List (OutlineLayout msg)
        , extraContent : Html msg
        }


{-| -}
type alias OutlinePanelConfig msg =
    { title : String
    , content : Html msg
    , hasDashedBorder : Bool
    , modifyRow : RowModifier
    , palette : Palette.Palette
    }


{-| This type also is used to give nice errors when passing invalid items to the rows field
-}
type OutlineLayout msg
    = Row (CustomConfig msg)
    | KeyedRow String (CustomConfig msg)


{-| -}
type RowModifier
    = NodeEvaluated Evaluation
    | Normal


{-| The vertical alignment of the outline horizontal bar
-}
type RowVerticalAlignment
    = TopAlign
    | MiddleAlign


{-| -}
type Evaluation
    = Good
    | Bad


{-| -}
type Hierarchy
    = Root
    | Child
