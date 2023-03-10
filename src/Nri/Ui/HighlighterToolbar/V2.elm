module Nri.Ui.HighlighterToolbar.V2 exposing (view)

{-| Bar with markers for choosing how text will be highlighted in a highlighter.

@docs view

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css exposing (Color)
import EventExtras exposing (onClickPreventDefaultAndStopPropagation)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


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


toolContainer : String -> Html msg -> Html msg
toolContainer toolName tool =
    div [ nriDescription toolName ] [ tool ]


{-| View renders each marker and an eraser. This is usually used with a Highlighter.
-}
view :
    { focusAndSelect : { select : Maybe tag, focus : Maybe String } -> msg
    , getColor : tag -> { extras | colorSolid : Color, colorLight : Color }
    , getName : tag -> String
    , highlighterId : String
    }
    -> { model | currentTool : Maybe tag, tags : List tag }
    -> Html msg
view config model =
    let
        tools =
            List.map Just model.tags ++ [ Nothing ]

        viewTagWithConfig : tag -> Html msg
        viewTagWithConfig tag =
            viewTag config (model.currentTool == Just tag) tag tools model.currentTool

        eraserSelected : Bool
        eraserSelected =
            model.currentTool == Nothing
    in
    toolbar config.highlighterId
        (List.map viewTagWithConfig model.tags
            ++ [ viewEraser config.focusAndSelect eraserSelected tools model.currentTool config.getName ]
        )


viewTag :
    { focusAndSelect : { select : Maybe tag, focus : Maybe String } -> msg
    , getColor : tag -> { extras | colorSolid : Color, colorLight : Color }
    , getName : tag -> String
    , highlighterId : String
    }
    -> Bool
    -> tag
    -> List (Maybe tag)
    -> Maybe tag
    -> Html msg
viewTag { focusAndSelect, getColor, getName } selected tag tools currentTool =
    toolContainer ("tag-" ++ getName tag)
        (viewTool (getName tag) focusAndSelect (getColor tag) selected (Just tag) tools currentTool getName)


viewEraser :
    ({ select : Maybe tag, focus : Maybe String } -> msg)
    -> Bool
    -> List (Maybe tag)
    -> Maybe tag
    -> (tag -> String)
    -> Html msg
viewEraser focusAndSelect selected tools currentTool getName =
    toolContainer "eraser"
        (viewTool "Remove highlight"
            focusAndSelect
            { colorLight = Colors.gray75, colorSolid = Colors.white }
            selected
            Nothing
            tools
            currentTool
            getName
        )


viewTool :
    String
    -> ({ select : Maybe tag, focus : Maybe String } -> msg)
    -> { extras | colorSolid : Color, colorLight : Color }
    -> Bool
    -> Maybe tag
    -> List (Maybe tag)
    -> Maybe tag
    -> (tag -> String)
    -> Html msg
viewTool name focusAndSelect theme selected tag tools currentTool getName =
    button
        [ id ("tag-" ++ name)
        , css
            [ Css.backgroundColor Css.transparent
            , Css.borderRadius (Css.px 0)
            , Css.border (Css.px 0)
            , Css.active [ Css.outlineStyle Css.none ]
            , Css.focus [ Css.outlineStyle Css.none ]
            , Css.cursor Css.pointer
            ]
        , onClickPreventDefaultAndStopPropagation (focusAndSelect { select = tag, focus = Nothing })
        , Aria.pressed (Just selected)
        , tabindex
            (if selected then
                0

             else
                -1
            )
        , Key.onKeyDownPreventDefault (keyEvents focusAndSelect currentTool tools getName)
        ]
        [ toolContent name theme tag
        , viewIf (\() -> active theme) selected
        ]


keyEvents : ({ select : Maybe tag, focus : Maybe String } -> msg) -> Maybe tag -> List (Maybe tag) -> (tag -> String) -> List (Key.Event msg)
keyEvents focusAndSelect tag tools getName =
    let
        onFocus tag_ =
            focusAndSelect
                { select = tag_
                , focus =
                    case tag_ of
                        Just tag__ ->
                            Just ("tag-" ++ getName tag__)

                        Nothing ->
                            Just "tag-Remove highlight"
                }

        findAdjacentTag tag_ ( isAdjacentTab, acc ) =
            if isAdjacentTab then
                ( False, Just (onFocus tag_) )

            else
                ( tag_ == tag, acc )

        goToNextTag : Maybe msg
        goToNextTag =
            List.foldl findAdjacentTag
                ( False
                , -- if there is no adjacent tag, default to the first tag
                  Maybe.map onFocus (List.head tools)
                )
                tools
                |> Tuple.second

        goToPreviousTag : Maybe msg
        goToPreviousTag =
            List.foldr findAdjacentTag
                ( False
                , -- if there is no adjacent tag, default to the last tag
                  Maybe.map onFocus (List.head (List.reverse tools))
                )
                tools
                |> Tuple.second
    in
    List.filterMap identity
        [ Maybe.map Key.right goToNextTag
        , Maybe.map Key.left goToPreviousTag
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
