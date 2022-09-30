module Nri.Ui.HighlighterToolbar.V1 exposing (view, static, RowTheme)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view, static, RowTheme

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (Color)
import EventExtras exposing (onClickPreventDefaultAndStopPropagation)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| A colour combination to use for highlights.
-}
type alias RowTheme =
    { solid : Color
    , light : Color
    , name : String
    }


toolbar : List (Html msg) -> Html msg
toolbar =
    ul
        [ nriDescription "tools"
        , css
            [ Css.displayFlex
            , Css.listStyle Css.none
            , Css.padding (Css.px 0)
            , Css.margin (Css.px 0)
            , Css.marginTop (Css.px 10)
            , Css.flexWrap Css.wrap
            ]
        ]


toolContainer : String -> Html msg -> Html msg
toolContainer toolName tool =
    li [ nriDescription toolName ] [ tool ]


{-| View renders each marker and an eraser. This is usually used with a Highlighter.
-}
view :
    { onSetEraser : msg
    , onChangeTag : tag -> msg
    , getColor : tag -> RowTheme
    , getName : tag -> String
    }
    -> { extras | currentTool : Maybe tag, tags : List tag }
    -> Html msg
view config model =
    let
        viewTagWithConfig : tag -> Html msg
        viewTagWithConfig tag =
            viewTag config (model.currentTool == Just tag) tag

        eraserSelected : Bool
        eraserSelected =
            model.currentTool == Nothing
    in
    toolbar
        (List.map viewTagWithConfig model.tags
            ++ [ viewEraser config.onSetEraser eraserSelected ]
        )


{-| Render only tags and not eraser without triggering an action
-}
static : (a -> String) -> (a -> RowTheme) -> List a -> Html msg
static getName getColor tags =
    toolbar (List.map (staticTag getName getColor) tags)


staticTag : (a -> String) -> (a -> RowTheme) -> a -> Html msg
staticTag getName getColor tag =
    toolContainer ("static-tag-" ++ getName tag)
        (toolContent (getName tag) (getColor tag) (Just tag))


viewTag :
    { onSetEraser : msg
    , onChangeTag : tag -> msg
    , getColor : tag -> RowTheme
    , getName : tag -> String
    }
    -> Bool
    -> tag
    -> Html msg
viewTag { getColor, onChangeTag, getName } selected tag =
    toolContainer ("tag-" ++ getName tag)
        (viewTool (getName tag) (onChangeTag tag) (getColor tag) selected <| Just tag)


viewEraser : msg -> Bool -> Html msg
viewEraser onSetEraser selected =
    toolContainer "eraser"
        (viewTool "Remove highlight"
            onSetEraser
            { light = Colors.gray75
            , solid = Colors.white
            , name = "eraser"
            }
            selected
            Nothing
        )


viewTool : String -> msg -> RowTheme -> Bool -> Maybe tag -> Html msg
viewTool name onClick theme selected tool =
    button
        [ css
            [ Css.backgroundColor Css.transparent
            , Css.borderRadius (Css.px 0)
            , Css.border (Css.px 0)
            , Css.active [ Css.outlineStyle Css.none ]
            , Css.focus [ Css.outlineStyle Css.none ]
            , Css.cursor Css.pointer
            ]
        , onClickPreventDefaultAndStopPropagation onClick
        , Aria.pressed (Just selected)
        ]
        [ toolContent name theme tool
        , viewIf (\() -> active theme) selected
        ]


active : RowTheme -> Html msg
active palette_ =
    div
        [ nriDescription "active-tool"
        , css
            [ Css.width (Css.px 38)
            , Css.height (Css.px 4)
            , Css.backgroundColor palette_.light
            ]
        ]
        []


toolContent : String -> RowTheme -> Maybe tag -> Html msg
toolContent name palette_ tool =
    span
        [ nriDescription "tool-content"
        , css
            [ Css.position Css.relative
            , Css.height (Css.pct 100)
            , Css.padding (Css.px 0)
            , Css.paddingRight (Css.px 15)
            , Css.display Css.inlineFlex
            , Css.alignItems Css.center
            ]
        ]
        [ case tool of
            Just _ ->
                span
                    [ css
                        [ Css.backgroundColor palette_.solid
                        , Css.width (Css.px 38)
                        , Css.height (Css.px 38)
                        , Css.borderRadius (Css.pct 50)
                        , Css.padding (Css.px 7)
                        ]
                    ]
                    [ UiIcon.highlighter
                        |> Svg.withColor Colors.white
                        |> Svg.toHtml
                    ]

            Nothing ->
                --( Styles.IconEraser, True )
                text "TODO -- eraser icon"
        , span
            [ nriDescription "tool-label"
            , css
                [ Css.color Colors.navy
                , Css.fontSize (Css.px 15)
                , Css.marginLeft (Css.px 5)
                , Css.fontWeight (Css.int 600)
                , Fonts.baseFont
                ]
            ]
            [ text name ]
        ]
