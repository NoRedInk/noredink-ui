module Nri.Ui.HighlighterToolbar.V3 exposing (view)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view


### Changes from V2:

  - Use radio inputs under the hood
  - don't arbitrarily complicate API -- match the usecases on the monolith side


### Patch changes:

  - Ensure selected tool is clear in high contrast mode


### Changes from V1:

  - replaces `onChangeTag` and `onSetEraser` with `onSelect`.
  - adds `highlighterId` to config
  - adds keyboard navigation

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription, safeIdWithPrefix)
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| View renders each marker and an eraser. This is exclusively used with an interactive Highlighter, whose id you should pass in when initializing the HighlighterToolbar.
-}
view :
    { onSelect : Maybe tag -> msg
    , getNameAndColor : tag -> { extras | name : String, colorSolid : Color, colorLight : Color }
    , highlighterId : String
    }
    -> { model | currentTool : Maybe tag, tags : List tag }
    -> Html msg
view config model =
    let
        viewTagWithConfig : tag -> Html msg
        viewTagWithConfig tag =
            viewTool config.onSelect (config.getNameAndColor tag) (Just tag) model
    in
    toolbar config.highlighterId
        (List.map viewTagWithConfig model.tags
            ++ [ viewEraser config.onSelect model ]
        )


toolbar : String -> List (Html msg) -> Html msg
toolbar highlighterId =
    div
        [ nriDescription "tools"
        , Role.toolBar
        , Aria.label "Highlighter options"
        , Aria.controls [ highlighterId ]
        , css
            [ Css.displayFlex
            , Css.listStyle Css.none
            , Css.padding (Css.px 0)
            , Css.margin (Css.px 0)
            , Css.marginTop (Css.px 10)
            , Css.flexWrap Css.wrap
            ]
        ]


viewEraser :
    (Maybe tag -> msg)
    -> { model | currentTool : Maybe tag }
    -> Html msg
viewEraser onSelect model =
    viewTool
        onSelect
        { name = "Remove highlight"
        , colorLight = Colors.gray75
        , colorSolid = Colors.white
        }
        Nothing
        model


viewTool :
    (Maybe tag -> msg)
    -> { extras | name : String, colorSolid : Color, colorLight : Color }
    -> Maybe tag
    -> { model | currentTool : Maybe tag }
    -> Html msg
viewTool onSelect ({ name } as theme) tag model =
    let
        selected =
            model.currentTool == tag
    in
    label
        [ id (safeIdWithPrefix "tag" name)
        , css
            [ Css.cursor Css.pointer
            , Css.position Css.relative
            , Css.pseudoClass "focus-within" FocusRing.styles
            , Css.paddingBottom (Css.px 2)
            , Css.marginRight (Css.px 15)
            ]
        , -- this class is used to remove focus styles for mouse users.
          -- any changes should be applied to FocusRing as well.
          class "highlighter-toolbar-label"
        ]
        [ input
            [ Attributes.value name
            , Attributes.type_ "radio"
            , Attributes.name "highlighter-toolbar-tool"
            , Attributes.checked selected
            , onClick (onSelect tag)
            , css
                [ Css.cursor Css.pointer

                -- position the radio input underneath the tool content
                , Css.position Css.absolute
                , Css.top (Css.px 4)
                , Css.left (Css.px 4)
                ]
            , Attributes.class FocusRing.customClass
            ]
            []
        , toolContent name theme tag
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
            , Css.border3 (Css.px 2) Css.solid palette_.colorLight
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
