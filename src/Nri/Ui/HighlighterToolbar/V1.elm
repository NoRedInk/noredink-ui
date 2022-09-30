module Nri.Ui.HighlighterToolbar.V1 exposing (view, static)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view, static

-}

import Accessibility.Styled.Aria as Aria
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import Nri.MultiHighlighter.Styles as Styles exposing (RowTheme, class, classList)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)
import Nri.Ui.Html.V3 exposing (viewIf)


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
            model.currentTool == Eraser
    in
    toolbar
        (List.map viewTagWithConfig model.tags
            ++ [ viewEraser config.onSetEraser eraserSelected ]
        )


{-| Render only tags and not eraser without triggering an action
-}
static : (a -> String) -> (a -> RowTheme) -> List a -> Html msg
static getName getColor =
    toolbar (List.map (staticTag getName getColor))


staticTag : (a -> String) -> (a -> RowTheme) -> a -> Html msg
staticTag getName getColor tag =
    toolContainer ("static-tag-" ++ getName tag)
        (toolContent (getName tag) (Just tag))


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
        (viewTool (getName tag) (onChangeTag tag) selected <| Just tag)


viewEraser : msg -> Bool -> Html msg
viewEraser onSetEraser selected =
    toolContainer "eraser"
        (viewTool "Remove highlight" onSetEraser selected Eraser)


viewTool : String -> msg -> Bool -> Tool a -> Html msg
viewTool name onClick selected tool =
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
        [ toolContent name tool
        , viewIf (\() -> active) selected
        ]


active : Html msg
active =
    div
        [ class [ Styles.ActiveTool ] ]
        []


toolContent : String -> Maybe tag -> Html msg
toolContent name tool =
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
        [ span [ classList [ iconClassFromTool tool ] ] []
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


iconClassFromTool : Maybe tag -> ( Styles.Classes, Bool )
iconClassFromTool tool =
    case tool of
        Just _ ->
            ( Styles.IconMarker, True )

        Nothing ->
            ( Styles.IconEraser, True )
