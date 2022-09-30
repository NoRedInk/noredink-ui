module Nri.Ui.HighlighterToolbar.V1 exposing (view, static)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view, static

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
import Nri.Ui.Util exposing (dashify)


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
    , getColor : tag -> { extras | colorSolid : Color, colorLight : Color }
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
static :
    (a -> String)
    -> (a -> { extras | colorSolid : Color, colorLight : Color })
    -> List a
    -> Html msg
static getName getColor tags =
    toolbar (List.map (staticTag getName getColor) tags)


staticTag :
    (a -> String)
    -> (a -> { extras | colorSolid : Color, colorLight : Color })
    -> a
    -> Html msg
staticTag getName getColor tag =
    toolContainer ("static-tag-" ++ dashify (getName tag))
        (toolContent (getName tag) (getColor tag) (Just tag))


viewTag :
    { onSetEraser : msg
    , onChangeTag : tag -> msg
    , getColor : tag -> { extras | colorSolid : Color, colorLight : Color }
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
            { colorLight = Colors.gray75, colorSolid = Colors.white }
            selected
            Nothing
        )


viewTool :
    String
    -> msg
    -> { extras | colorSolid : Color, colorLight : Color }
    -> Bool
    -> Maybe tag
    -> Html msg
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


active :
    { extras | colorLight : Color }
    -> Html msg
active palette_ =
    div
        [ nriDescription "active-tool"
        , css
            [ Css.width (Css.px 38)
            , Css.height (Css.px 4)
            , Css.backgroundColor palette_.colorLight
            ]
        ]
        []


toolContent :
    String
    -> { extras | colorSolid : Color, colorLight : Color }
    -> Maybe tag
    -> Html msg
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
                toolIcon
                    { background = palette_.colorSolid
                    , border = palette_.colorSolid
                    , icon = Svg.withColor Colors.white UiIcon.highlighter
                    }

            Nothing ->
                toolIcon
                    { background = palette_.colorSolid
                    , border = Colors.gray75
                    , icon = Svg.withColor Colors.gray20 UiIcon.eraser
                    }
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


toolIcon : { background : Color, border : Color, icon : Svg.Svg } -> Html msg
toolIcon config =
    span
        [ css
            [ Css.backgroundColor config.background
            , Css.width (Css.px 38)
            , Css.height (Css.px 38)
            , Css.borderRadius (Css.pct 50)
            , Css.padding (Css.px 7)
            , Css.border3 (Css.px 1) Css.solid config.border
            ]
        ]
        [ Svg.toHtml config.icon
        ]
