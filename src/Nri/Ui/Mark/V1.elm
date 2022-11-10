module Nri.Ui.Mark.V1 exposing
    ( Mark
    , view, viewWithInlineTags, viewWithBalloonTags
    )

{-|

@docs Mark
@docs view, viewWithInlineTags, viewWithBalloonTags

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style exposing (invisibleStyle)
import Css exposing (Style)
import Css.Global
import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (class, css)
import Nri.Ui.Balloon.V1 as Balloon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-| -}
type alias Mark =
    { name : Maybe String
    , startStyles : List Css.Style
    , styles : List Css.Style
    , endStyles : List Css.Style
    }


{-| When elements are marked, wrap them in a single `mark` html node.
-}
view :
    (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
view =
    view_ HiddenTags


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in-line with the emphasized content.

-}
viewWithInlineTags :
    (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
viewWithInlineTags =
    view_ InlineTags


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in a balloon centered above the emphasized content.

-}
viewWithBalloonTags :
    (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
viewWithBalloonTags =
    view_ BalloonTags


type TagStyle
    = HiddenTags
    | InlineTags
    | BalloonTags


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in-line with the emphasized content when `showTagsInline` is True.

-}
view_ :
    TagStyle
    -> (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
view_ tagStyle viewSegment highlightables =
    case highlightables of
        [] ->
            []

        ( _, marked ) :: _ ->
            let
                segments =
                    List.indexedMap
                        (\index ( content, mark ) -> viewSegment content (markStyles index mark))
                        highlightables
            in
            case marked of
                Just markedWith ->
                    [ Html.mark
                        [ markedWith.name
                            |> Maybe.map (\name -> Aria.roleDescription (name ++ " highlight"))
                            |> Maybe.withDefault AttributesExtra.none
                        , css
                            [ Css.backgroundColor Css.transparent
                            , Css.Global.children
                                [ Css.Global.selector ":last-child"
                                    (Css.after
                                        [ Css.property "content" ("\" [end " ++ (Maybe.map (\name -> name) markedWith.name |> Maybe.withDefault "highlight") ++ "] \"")
                                        , invisibleStyle
                                        ]
                                        :: markedWith.endStyles
                                    )
                                ]
                            ]
                        ]
                        (viewStartHighlight tagStyle markedWith :: segments)
                    ]

                Nothing ->
                    segments


viewStartHighlight : TagStyle -> Mark -> Html msg
viewStartHighlight tagStyle marked =
    span
        [ css (marked.styles ++ marked.startStyles)
        , class "highlighter-inline-tag highlighter-inline-tag-highlighted"
        ]
        [ viewJust (viewTag tagStyle) marked.name ]


markStyles : Int -> Maybe Mark -> List Css.Style
markStyles index marked =
    case ( index == 0, marked ) of
        ( True, Just markedWith ) ->
            -- if we're on the first highlighted element, we add
            -- a `before` content saying what kind of highlight we're starting
            tagBeforeContent markedWith :: markedWith.styles

        _ ->
            Maybe.map .styles marked
                |> Maybe.withDefault []


tagBeforeContent : Mark -> Css.Style
tagBeforeContent markedWith =
    case markedWith.name of
        Just name ->
            Css.before
                [ MediaQuery.notHighContrastMode
                    [ Css.property "content" ("\" [start " ++ name ++ " highlight] \"")
                    , invisibleStyle
                    ]
                ]

        Nothing ->
            Css.before
                [ Css.property "content" "\" [start highlight] \""
                , invisibleStyle
                ]


viewTag : TagStyle -> String -> Html msg
viewTag tagStyle =
    case tagStyle of
        InlineTags ->
            viewInlineTag
                [ MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "color" "initial" |> Css.important
                    ]
                ]

        HiddenTags ->
            viewInlineTag
                [ Css.display Css.none
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.display Css.inline |> Css.important
                    , Css.property "color" "initial" |> Css.important
                    ]
                ]

        BalloonTags ->
            viewBalloon


viewInlineTag : List Css.Style -> String -> Html msg
viewInlineTag customizations name =
    span
        [ css
            [ Fonts.baseFont
            , Css.backgroundColor Colors.white
            , Css.color Colors.navy
            , Css.padding2 (Css.px 2) (Css.px 4)
            , Css.borderRadius (Css.px 3)
            , Css.margin2 Css.zero (Css.px 5)
            , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 1) Css.zero Colors.gray75
            , Css.batch customizations
            ]
        , -- we use the :before element to convey details about the start of the
          -- highlighter to screenreaders, so the visual label is redundant
          Aria.hidden True
        ]
        [ Html.text name ]


viewBalloon : String -> Html msg
viewBalloon label =
    Html.text label
