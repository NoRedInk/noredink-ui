module Nri.Outline.Utils exposing (..)

{-|

@docs RowNodeType, keyedRowToHtml, outlinePanel, paletteOfFirst, rawRow, rawRowWithExtraContent, rowToHtml, toHtml, toKeyedHtml, toRowNodeType, viewExtraContent, viewRow, withEvaluated, withTitle

-}

import Css exposing (..)
import Html exposing (..)
import Html.Attributes
import Html.Keyed
import Nri.Outline.Styles as Styles exposing (styles)
import Nri.Outline.Types as Types
import Nri.Palette as Palette


{-| -}
type RowNodeType msg
    = Tag (Html msg)
    | KeyedTag ( String, Html msg )


{-| -}
withTitle : String -> Types.OutlineLayout msg -> Types.OutlineLayout msg
withTitle title taggedRow =
    let
        updatedConfig config =
            { config
                | content =
                    outlinePanel
                        { title = title
                        , content = config.content
                        , hasDashedBorder = config.hasDashedBorder
                        , palette = config.palette
                        , modifyRow = config.modifyRow
                        }
                , verticalAlign = Types.TopAlign
            }
    in
    case taggedRow of
        Types.Row config ->
            Types.Row <| updatedConfig config

        Types.KeyedRow key config ->
            Types.KeyedRow key <| updatedConfig config


{-| -}
withEvaluated : Types.Evaluation -> Types.OutlineLayout msg -> Types.OutlineLayout msg
withEvaluated evaluation taggedRow =
    {- TODO: this should render with styles like a row with title, but hide the title.
       Currently, the developer and use this without a title. If that happens, then
       this has no affect on the row
    -}
    case taggedRow of
        Types.Row config ->
            Types.Row { config | modifyRow = Types.NodeEvaluated evaluation }

        Types.KeyedRow key config ->
            Types.KeyedRow key { config | modifyRow = Types.NodeEvaluated evaluation }


{-| -}
outlinePanel : Types.OutlinePanelConfig msg -> Html msg
outlinePanel config =
    div []
        [ div
            [ styles.class
                [ Styles.RowPanelTitle
                , case config.palette.name of
                    Palette.Gray ->
                        Styles.RowPanelTitleGray

                    Palette.DarkGray ->
                        Styles.RowPanelTitleDarkGray

                    Palette.Blue ->
                        Styles.RowPanelTitleBlue

                    Palette.DarkBlue ->
                        Styles.RowPanelTitleDarkBlue

                    Palette.Purple ->
                        Styles.RowPanelTitlePurple

                    Palette.Turquoise ->
                        Styles.RowPanelTitleTurquoise

                    Palette.Red ->
                        Styles.RowPanelTitleRed

                    Palette.Green ->
                        Styles.RowPanelTitleGreen

                    Palette.White ->
                        Styles.RowPanelTitleWhite

                    Palette.Cornflower ->
                        Styles.RowPanelTitleCornflower

                    Palette.Aqua ->
                        Styles.RowPanelTitleAqua
                ]
            ]
            [ Html.text config.title ]
        , div
            [ styles.classList
                [ ( Styles.RowPanelContent, True )
                , ( Styles.RowPanelContentDashed, config.hasDashedBorder )
                , ( case config.modifyRow of
                        Types.NodeEvaluated Types.Good ->
                            Styles.RowPanelContentGreen

                        Types.NodeEvaluated Types.Bad ->
                            Styles.RowPanelContentPurple

                        _ ->
                            case config.palette.name of
                                Palette.Gray ->
                                    Styles.RowPanelContentGray

                                Palette.DarkGray ->
                                    Styles.RowPanelContentDarkGray

                                Palette.Blue ->
                                    Styles.RowPanelContentBlue

                                Palette.DarkBlue ->
                                    Styles.RowPanelContentDarkBlue

                                Palette.Purple ->
                                    Styles.RowPanelContentPurple

                                Palette.Turquoise ->
                                    Styles.RowPanelContentTurquoise

                                Palette.Red ->
                                    Styles.RowPanelContentRed

                                Palette.Green ->
                                    Styles.RowPanelContentGreen

                                Palette.White ->
                                    Styles.RowPanelContentWhite

                                Palette.Cornflower ->
                                    Styles.RowPanelContentCornflower

                                Palette.Aqua ->
                                    Styles.RowPanelContentAqua
                  , True
                  )
                ]
            ]
            [ config.content ]
        ]


{-| -}
toRowNodeType : Types.Hierarchy -> Types.OutlineLayout msg -> RowNodeType msg
toRowNodeType hierarchy layout =
    case layout of
        Types.Row config ->
            Tag <|
                viewRow
                    hierarchy
                    config
                    (ul
                        [ styles.class [ Styles.RowChildren ] ]
                        (List.map (rowToHtml Types.Child) config.rows)
                    )

        Types.KeyedRow key config ->
            KeyedTag
                ( key
                , viewRow
                    hierarchy
                    config
                    (Html.Keyed.node "ul"
                        [ styles.class [ Styles.RowChildren ] ]
                        (List.map (keyedRowToHtml Types.Child) config.rows)
                    )
                )


{-| -}
viewRow : Types.Hierarchy -> Types.CustomConfig msg -> Html msg -> Html msg
viewRow hierarchy config children =
    let
        extraContentPalette =
            paletteOfFirst config.rows

        rowAttrs =
            [ styles.classList
                [ ( Styles.Row, True )
                , ( Styles.ChildRow, hierarchy == Types.Child )
                , ( case config.palette.name of
                        Palette.Blue ->
                            Styles.RowBlue

                        Palette.Gray ->
                            Styles.RowGray

                        Palette.DarkGray ->
                            Styles.RowDarkGray

                        Palette.DarkBlue ->
                            Styles.RowDarkBlue

                        Palette.Purple ->
                            Styles.RowPurple

                        Palette.Turquoise ->
                            Styles.RowTurquoise

                        Palette.Red ->
                            Styles.RowRed

                        Palette.Green ->
                            Styles.RowGreen

                        Palette.White ->
                            Styles.RowWhite

                        Palette.Cornflower ->
                            Styles.RowCornflower

                        Palette.Aqua ->
                            Styles.RowAqua
                  , True
                  )
                ]

            -- , OutlineCss.modifierClassFromAlign config.verticalAlign
            ]
    in
    li
        rowAttrs
        [ div
            [ styles.classList
                [ ( Styles.RowContent, True )
                , ( Styles.ChildRowContent, hierarchy == Types.Child )
                , ( case config.palette.name of
                        Palette.Blue ->
                            Styles.ChildRowContentBlue

                        Palette.Gray ->
                            Styles.ChildRowContentGray

                        Palette.DarkGray ->
                            Styles.ChildRowContentDarkGray

                        Palette.DarkBlue ->
                            Styles.ChildRowContentDarkBlue

                        Palette.Purple ->
                            Styles.ChildRowContentPurple

                        Palette.Turquoise ->
                            Styles.ChildRowContentTurquoise

                        Palette.Red ->
                            Styles.ChildRowContentRed

                        Palette.Green ->
                            Styles.ChildRowContentGreen

                        Palette.White ->
                            Styles.ChildRowContentWhite

                        Palette.Cornflower ->
                            Styles.ChildRowContentCornflower

                        Palette.Aqua ->
                            Styles.ChildRowContentAqua
                  , True
                  )
                ]
            ]
            [ config.content
            , Maybe.map (viewExtraContent extraContentPalette) config.extraContent
                |> Maybe.withDefault (Html.text "")
            ]
        , if List.isEmpty config.rows then
            Html.text ""
          else
            children
        ]


{-| -}
viewExtraContent : Maybe Palette.Palette -> Html msg -> Html msg
viewExtraContent palette content =
    let
        {- Styles are inline here to accomodate the calculations we must do
           to correctly format extra content. This is based off if there are any child rows
           following the extra content. If there are, we add the padding and border styles.
           If there are not, we have no styling.
        -}
        styles =
            (Css.asPairs >> Html.Attributes.style) <|
                case palette of
                    Just { border } ->
                        [ marginLeft (px 15)
                        , borderLeft3 (px 1) solid border
                        , paddingLeft (px 15)
                        ]

                    Nothing ->
                        []
    in
    div [ styles ] [ content ]


{-| When the row has extra content, the palette from the first child needs to be known in
order to have the correct border on the left of the extra content. This ensures we have
a consistent styling on the outline borders.
-}
paletteOfFirst : List (Types.OutlineLayout msg) -> Maybe Palette.Palette
paletteOfFirst rows =
    rows
        |> List.head
        |> Maybe.map
            (\row ->
                case row of
                    Types.Row config ->
                        config.palette

                    Types.KeyedRow _ config ->
                        config.palette
            )


{-| -}
rowToHtml : Types.Hierarchy -> Types.OutlineLayout msg -> Html msg
rowToHtml hierarchy =
    toRowNodeType hierarchy >> toHtml


{-| -}
keyedRowToHtml : Types.Hierarchy -> Types.OutlineLayout msg -> ( String, Html msg )
keyedRowToHtml hierarchy =
    toRowNodeType hierarchy >> toKeyedHtml


{-| -}
toHtml : RowNodeType msg -> Html msg
toHtml row =
    case row of
        Tag html ->
            html

        KeyedTag ( _, html ) ->
            html


{-| -}
toKeyedHtml : RowNodeType msg -> ( String, Html msg )
toKeyedHtml row =
    case row of
        Tag html ->
            ( "", html )

        KeyedTag output ->
            output


{-| -}
rawRow : Types.RowConfig msg -> Types.CustomConfig msg
rawRow config =
    { content = config.content
    , palette = config.palette
    , rows = config.rows
    , verticalAlign = Types.MiddleAlign
    , extraContent = Nothing
    , modifyRow = Types.Normal
    , hasDashedBorder = False
    }


{-| -}
rawRowWithExtraContent : Types.ExtraContenRowConfig msg -> Types.CustomConfig msg
rawRowWithExtraContent config =
    { content = config.content
    , palette = config.palette
    , rows = config.rows
    , verticalAlign = Types.TopAlign
    , extraContent = Just config.extraContent
    , modifyRow = Types.Normal
    , hasDashedBorder = False
    }
