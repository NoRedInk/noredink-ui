module Nri.Ui.Outline.V1 exposing
    ( Outline
    , view
    , row, rowWithExtraContent, customRow
    )

{-| A nestable layout that can be themed.

@docs Outline
@docs view


## Rows

@docs row, rowWithExtraContent, customRow

-}

import Css exposing (Color)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Outline.RowTheme exposing (RowTheme)
import Nri.Outline.Utils as Utils


{-| -}
type Outline msg
    = Outline
        { extraContent : Maybe { border : Maybe Color, content : Html msg }
        , rows : List (Outline msg)
        , title : Maybe String
        , content : Html msg
        , palette : RowTheme
        }


{-|

    import Html.Styled exposing (..)
    import Nri.Outline as Outline

    main : Html msg
    main =
        Outline.view []

-}
view : List (Outline msg) -> Html msg
view rows =
    Html.Styled.ul
        [ Html.Styled.Attributes.css
            [ Css.listStyle Css.none
            , Css.margin4 (Css.px 10) Css.zero Css.zero Css.zero
            , Css.padding Css.zero
            ]
        ]
        (viewRows Utils.Root rows)


viewRows : Utils.Hierarchy -> List (Outline msg) -> List (Html msg)
viewRows hierarchy rows =
    let
        orderedNodeColors =
            (List.map (\(Outline { palette }) -> Just palette.border) rows ++ [ Nothing ])
                |> List.drop 1
    in
    List.map2 (viewRow hierarchy) orderedNodeColors rows


viewRow : Utils.Hierarchy -> Maybe Css.Color -> Outline msg -> Html msg
viewRow hierarchy nextNodeColor (Outline config) =
    Utils.viewRow hierarchy nextNodeColor config <|
        Html.Styled.ul
            [ css
                [ Css.marginLeft (Css.px 25)
                , Css.paddingLeft (Css.px 25)
                , Css.paddingTop (Css.px 25)
                , Css.listStyleType Css.none
                ]
            ]
            (viewRows Utils.Child config.rows)


{-|

    import Html.Styled exposing (..)
    import Nri.Outline as Outline exposing (Outline)

    myRow : Outline msg
    myRow =
        Outline.row
            { title = Just "My node"
            , content = text "This is my content"
            , palette = RowTheme.red
            , rows = []
            }

-}
row :
    { title : Maybe String
    , content : Html msg
    , palette : RowTheme
    , rows : List (Outline msg)
    }
    -> Outline msg
row config =
    Outline
        { title = config.title
        , content = config.content
        , palette = config.palette
        , rows = config.rows
        , extraContent = Nothing
        }


{-| -}
customRow :
    { extraContent : Maybe { border : Maybe Color, content : Html msg }
    , rows : List (Outline msg)
    , title : Maybe String
    , content : Html msg
    , palette : RowTheme
    }
    -> Outline msg
customRow config =
    Outline config


{-| -}
rowWithExtraContent :
    { rows : List (Outline msg)
    , extraContent : { border : Maybe Color, content : Html msg }
    , title : Maybe String
    , content : Html msg
    , palette : RowTheme
    }
    -> Outline msg
rowWithExtraContent config =
    Outline
        { title = config.title
        , content = config.content
        , palette = config.palette
        , rows = config.rows
        , extraContent = Just config.extraContent
        }
