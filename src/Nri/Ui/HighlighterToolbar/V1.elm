module Nri.Ui.HighlighterToolbar.V1 exposing (view, static)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view, static

-}

import Accessibility.Styled.Aria as Aria
import Html.Styled exposing (..)
import Html.Styled.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import Nri.MultiHighlighter.Styles as Styles exposing (RowTheme, class, classList)
import Nri.Ui.Html.V3 exposing (viewIf)


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
    ul [ class [ Styles.Tools ] ] <|
        List.map viewTagWithConfig model.tags
            ++ [ viewEraser config.onSetEraser eraserSelected ]


staticTag : (a -> String) -> (a -> RowTheme) -> a -> Html msg
staticTag getName getColor tag =
    li [ class [ Styles.Tag <| getColor tag ] ]
        [ staticTool (getName tag) (Just tag)
        ]


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
    li [ class [ Styles.Tag <| getColor tag ] ]
        [ viewTool (getName tag) (onChangeTag tag) selected <| Just tag
        ]


{-| Render only tags and not eraser without triggering an action
-}
static : (a -> String) -> (a -> RowTheme) -> List a -> Html msg
static getName getColor =
    ul [ class [ Styles.Tools ] ]
        << List.map (staticTag getName getColor)


viewEraser : msg -> Bool -> Html msg
viewEraser onSetEraser selected =
    li [ class [ Styles.Eraser ] ]
        [ viewTool "Remove highlight" onSetEraser selected Eraser
        ]


viewTool : String -> msg -> Bool -> Tool a -> Html msg
viewTool name onClick selected tool =
    -- Looks like according to this, https://bugzilla.mozilla.org/show_bug.cgi?id=984869#c2,
    -- buttons don't react to CSS in a specified way.
    -- So, we wrap the content in a div and style it instead of the button.
    button
        [ class [ Styles.ToolButton ]
        , onClickPreventDefaultAndStopPropagation onClick
        , Aria.pressed (Just selected)
        ]
        [ div
            [ class [ Styles.ToolButtonContent ] ]
            [ span
                [ classList
                    [ iconClassFromTool tool
                    ]
                ]
                []
            , span [ class [ Styles.Label ] ] [ text name ]
            ]
        , viewIf (\() -> active) selected
        ]


active : Html msg
active =
    div
        [ class [ Styles.ActiveTool ] ]
        []


staticTool : String -> Maybe tag -> Html msg
staticTool name tool =
    span [ class [ Styles.ToolButtonContent ] ]
        [ span [ classList [ iconClassFromTool tool ] ] []
        , span [ class [ Styles.Label ] ] [ text name ]
        ]


iconClassFromTool : Maybe tag -> ( Styles.Classes, Bool )
iconClassFromTool tool =
    case tool of
        Just _ ->
            ( Styles.IconMarker, True )

        Nothing ->
            ( Styles.IconEraser, True )
