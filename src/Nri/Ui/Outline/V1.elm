module Nri.Ui.Outline.V1 exposing
    ( Outline
    , view
    , row, customRow
    , RowTheme
    , white, gray, darkGray, blue, darkBlue, purple, turquoise, green, red, aqua, cornflower
    , blueDashBordered
    , purpleBordered, greenBordered
    )

{-| A nestable layout that can be themed.

@docs Outline
@docs view


## Rows

@docs row, customRow


## Predefined color palettes for use with Outline.

@docs RowTheme
@docs white, gray, darkGray, blue, darkBlue, purple, turquoise, green, red, aqua, cornflower
@docs blueDashBordered
@docs purpleBordered, greenBordered

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Attributes
import Nri.Ui.Html.V3 exposing (viewJust)


{-| -}
type Outline msg
    = Outline
        { rows : List (Outline msg)
        , title : Maybe String
        , content : Html msg
        , palette : RowTheme
        }


{-|

    import Html.Styled exposing (..)
    import Nri.Ui.Outline.V1 as Outline

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
        (viewRows Root rows)


viewRows : Hierarchy -> List (Outline msg) -> List (Html msg)
viewRows hierarchy rows =
    let
        orderedNodeColors =
            (List.map (\(Outline { palette }) -> Just palette.border) rows ++ [ Nothing ])
                |> List.drop 1
    in
    List.map2 (viewRow hierarchy) orderedNodeColors rows


viewRow : Hierarchy -> Maybe Css.Color -> Outline msg -> Html msg
viewRow hierarchy nextNodeColor (Outline config) =
    utilViewRow hierarchy
        nextNodeColor
        { rows = config.rows
        , title = config.title
        , content = config.content
        , palette = config.palette
        , extraContent = Nothing
        }
        (Html.Styled.ul
            [ css
                [ Css.marginLeft (Css.px 25)
                , Css.paddingLeft (Css.px 25)
                , Css.paddingTop (Css.px 25)
                , Css.listStyleType Css.none
                ]
            ]
            (viewRows Child config.rows)
        )


{-|

    import Html.Styled exposing (..)
    import Nri.Ui.Outline.V1 as Outline exposing (Outline)

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
        }


{-| -}
customRow :
    { rows : List (Outline msg)
    , title : Maybe String
    , content : Html msg
    , palette : RowTheme
    }
    -> Outline msg
customRow config =
    Outline config



-- THEMES


{-| -}
type alias RowTheme =
    { border : Color
    , borderStyle : Style
    , background : Color
    }


{-| Aqua palette
-}
aqua : RowTheme
aqua =
    { border = Colors.aquaDark
    , borderStyle = Css.batch []
    , background = Colors.aquaLight
    }


{-| Dark Gray palette
-}
darkGray : RowTheme
darkGray =
    { border = Colors.gray45
    , borderStyle = Css.batch []
    , background = Colors.gray96
    }


{-| -}
gray : RowTheme
gray =
    { border = Colors.gray45
    , borderStyle = Css.batch []
    , background = Colors.white
    }


{-| Blue palette
-}
blue : RowTheme
blue =
    { border = Colors.azure
    , borderStyle = Css.batch []
    , background = Colors.frost
    }


{-| Blue palette
-}
blueDashBordered : RowTheme
blueDashBordered =
    { border = Colors.azure
    , borderStyle = Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.dashed ]
    , background = Colors.frost
    }


{-| Dark blue palette
-}
darkBlue : RowTheme
darkBlue =
    { border = Colors.navy
    , borderStyle = Css.batch []
    , background = Colors.frost
    }


{-| Purple palette with a purple border instead of a purple background color
-}
purple : RowTheme
purple =
    { border = Colors.purple
    , borderStyle = Css.batch []
    , background = Colors.purpleLight
    }


{-| Purple palette with a purple border instead of a purple background color
-}
purpleBordered : RowTheme
purpleBordered =
    { border = Colors.purple
    , borderStyle = Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.solid ]
    , background = Colors.white
    }


{-| Turquoise palette
-}
turquoise : RowTheme
turquoise =
    { border = Colors.turquoiseDark
    , borderStyle = Css.batch []
    , background = Colors.turquoiseLight
    }


{-| Green palette
-}
green : RowTheme
green =
    { border = Colors.greenDarkest
    , borderStyle = Css.batch []
    , background = Colors.greenLightest
    }


{-| Green palette with a green border instead of a green background color
-}
greenBordered : RowTheme
greenBordered =
    { border = Colors.greenDarkest
    , borderStyle = Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.solid ]
    , background = Colors.white
    }


{-| Red palette
-}
red : RowTheme
red =
    { border = Colors.red
    , borderStyle = Css.batch []
    , background = Colors.redLight
    }


{-| White palette (borders are blue)
-}
white : RowTheme
white =
    { border = Colors.navy
    , borderStyle = Css.batch []
    , background = Colors.white
    }


{-| Cornflower palette
-}
cornflower : RowTheme
cornflower =
    { border = Colors.cornflowerDark
    , borderStyle = Css.batch []
    , background = Colors.cornflowerLight
    }



-- UTILS


wrapViewWithTitleBubble :
    { title : String
    , content : Html msg
    , palette : RowTheme
    }
    -> Html msg
wrapViewWithTitleBubble config =
    let
        kebabTitle =
            String.replace " " "-" (String.toLower config.title)
    in
    Html.Styled.div []
        [ Html.Styled.div
            [ Html.Styled.Attributes.attribute "data-nri-description" "outline-title"
            , css
                [ Fonts.baseFont
                , borderRadius (px 18)
                , color Colors.white
                , display inlineBlock
                , fontSize (px 15)
                , fontWeight bold
                , height (px 35)
                , lineHeight (px 35)
                , left (px -10)
                , padding2 zero (px 15)
                , position absolute
                , top (px -15)
                , backgroundColor config.palette.border
                ]
            ]
            [ Html.Styled.text config.title ]
        , Html.Styled.div
            [ Attributes.testId (kebabTitle ++ "-text")
            , css
                [ borderRadius (px 8)
                , color Colors.gray20
                , fontSize (px 18)
                , Fonts.quizFont
                , padding3 (px 30) (px 15) (px 15)
                , lineHeight (px 30)
                , backgroundColor config.palette.background
                , config.palette.borderStyle
                , borderColor config.palette.border
                , after [ borderColor config.palette.border ]
                ]
            ]
            [ config.content ]
        ]


{-| -}
utilViewRow :
    Hierarchy
    -> Maybe Color
    ->
        { title : Maybe String
        , content : Html msg
        , extraContent : Maybe { border : Maybe Color, content : Html msg }
        , palette : RowTheme
        , rows : List outline
        }
    -> Html msg
    -> Html msg
utilViewRow hierarchy nextNodeColor config children =
    let
        rowAttrs =
            [ Html.Styled.Attributes.attribute "data-nri-description" "outline-row"
            , css
                [ paddingBottom (px 25)
                , position relative
                , lastChild [ paddingBottom zero ]
                , case hierarchy of
                    Child ->
                        verticalChildConnector config.palette.border nextNodeColor

                    Root ->
                        Css.batch []
                ]
            ]
    in
    Html.Styled.li
        rowAttrs
        [ Html.Styled.div
            [ css
                [ position relative
                , case hierarchy of
                    Child ->
                        horizontalChildConnector config.palette.border

                    Root ->
                        Css.batch []
                ]
            ]
            [ case config.title of
                Just title ->
                    wrapViewWithTitleBubble
                        { title = title
                        , content = config.content
                        , palette = config.palette
                        }

                Nothing ->
                    config.content
            , viewJust viewExtraContent config.extraContent
            ]
        , if List.isEmpty config.rows then
            text ""

          else
            children
        ]


verticalChildConnector : Color -> Maybe Color -> Style
verticalChildConnector paletteBorder nextNodeColor =
    Css.batch
        [ after
            [ property "content" "''"
            , position absolute
            , top (px -25)
            , left (px -18)
            , width (px 18)
            , borderLeft3 (px 1) solid Colors.gray75
            , property "height" "calc(16px)"
            , borderColor paletteBorder
            ]
        , before
            (case nextNodeColor of
                Just border ->
                    [ property "content" "''"
                    , position absolute
                    , left (px -18)
                    , width (px 18)
                    , borderLeft3 (px 1) solid Colors.gray75
                    , property "height" "calc(100%)"
                    , borderColor border
                    ]

                Nothing ->
                    []
            )
        , lastChild
            [ after [ borderLeftWidth zero ]
            , before [ borderLeftWidth zero ]
            ]
        ]


horizontalChildConnector : Color -> Style
horizontalChildConnector paletteBorder =
    after
        [ property "content" "''"
        , height (pct 80)
        , width (px 18)
        , borderBottom3 (px 1) solid Colors.gray75
        , borderLeft3 (px 1) solid Colors.gray75
        , left (px -18)
        , borderRadius4 zero zero zero (px 4)
        , top (px -25)
        , position absolute
        , maxHeight (px 60)
        , borderColor paletteBorder
        ]


viewExtraContent : { border : Maybe Color, content : Html msg } -> Html msg
viewExtraContent { border, content } =
    div
        [ css
            [ marginLeft (px 32)
            , Maybe.map (borderLeft3 (px 1) solid) border
                |> Maybe.withDefault (Css.batch [])
            , paddingLeft (px 15)
            ]
        ]
        [ content ]



-- Types


{-| -}
type Hierarchy
    = Root
    | Child
