module Nri.Ui.Block.V1 exposing
    ( view, Attribute
    , plaintext, content
    , Content, string, blank
    , emphasize, label, labelOffset
    , yellow, cyan, magenta, green, blue, purple, brown
    , class
    )

{-|

@docs view, Attribute


## Content

@docs plaintext, content
@docs Content, string, blank


## Content customization

@docs emphasize, label, labelOffset


### Visual customization

@docs yellow, cyan, magenta, green, blue, purple, brown


### General attributes

@docs class

-}

import Accessibility.Styled exposing (..)
import Css exposing (Color)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (nriDescription)
import Nri.Ui.Mark.V1 as Mark exposing (Mark)
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-|

    Block.view [ Block.plaintext "Hello, world!" ]

-}
view : List Attribute -> List (Html msg)
view attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) defaultConfig
        |> render



-- Attributes


{-| Provide the main content of the block as a plain-text string. You can also use `content` for more complex cases, including a blank appearing within an emphasis.
-}
plaintext : String -> Attribute
plaintext content_ =
    Attribute <| \config -> { config | content = [ String_ content_ ] }


{-| Use `content` for more complex block views, for instance when a blank appears within an emphasis block. Prefer to use `plaintext` when possible for better readability.

    Block.view
        [ Block.emphasize
        , Block.content [ Block.string "Hello, ", Block.blank, Block.string "!" ]
        ]

-}
content : List Content -> Attribute
content content_ =
    Attribute <| \config -> { config | content = content_ }


{-| Mark content as emphasized.
-}
emphasize : Attribute
emphasize =
    Attribute <| \config -> { config | theme = Just Emphasis }


{-| -}
label : String -> Attribute
label label_ =
    Attribute <| \config -> { config | label = Just label_ }


{-| -}
labelOffset : Maybe Float -> Attribute
labelOffset offset =
    Attribute <| \config -> { config | labelOffset = offset }



-- Content


{-| -}
type Content
    = String_ String
    | Blank


renderContent : Maybe String -> Content -> List Css.Style -> Html msg
renderContent class_ content_ markStyles =
    span
        [ css (Css.whiteSpace Css.preWrap :: markStyles)
        , nriDescription "block-segment-container"
        , AttributesExtra.maybe Attributes.class class_
        ]
        (case content_ of
            String_ str ->
                [ text str ]

            Blank ->
                [ viewBlank class_ ]
        )


{-| You will only need to use this helper if you're also using `content` to construct a more complex Block. Maybe you want `plaintext` instead?
-}
string : String -> Content
string =
    String_


{-| You will only need to use this helper if you're also using `content` to construct a more complex Block. For a less complex blank Block, don't include content or plaintext in the list of attributes.
-}
blank : Content
blank =
    Blank



-- Color themes


{-| -}
type Theme
    = Emphasis
    | Yellow
    | Cyan
    | Magenta
    | Green
    | Blue
    | Purple
    | Brown


themeToPalette : Theme -> Palette
themeToPalette theme =
    case theme of
        Emphasis ->
            defaultPalette

        Yellow ->
            { backgroundColor = Colors.highlightYellow
            , borderColor = Colors.highlightYellowDark
            }

        Cyan ->
            { backgroundColor = Colors.highlightCyan
            , borderColor = Colors.highlightCyanDark
            }

        Magenta ->
            { backgroundColor = Colors.highlightMagenta
            , borderColor = Colors.highlightMagentaDark
            }

        Green ->
            { backgroundColor = Colors.highlightGreen
            , borderColor = Colors.highlightGreenDark
            }

        Blue ->
            { backgroundColor = Colors.highlightBlue
            , borderColor = Colors.highlightBlueDark
            }

        Purple ->
            { backgroundColor = Colors.highlightPurple
            , borderColor = Colors.highlightPurpleDark
            }

        Brown ->
            { backgroundColor = Colors.highlightBrown
            , borderColor = Colors.highlightBrownDark
            }


type alias Palette =
    { backgroundColor : Color, borderColor : Color }


defaultPalette : Palette
defaultPalette =
    { backgroundColor = Colors.highlightYellow
    , borderColor = Colors.highlightYellowDark
    }


toMark : Maybe String -> Maybe Palette -> Maybe Mark
toMark label_ palette =
    case ( label_, palette ) of
        ( _, Just { backgroundColor, borderColor } ) ->
            let
                borderWidth =
                    Css.px 1

                borderStyles =
                    [ Css.borderStyle Css.dashed
                    , Css.borderColor borderColor
                    ]
            in
            Just
                { name = label_
                , startStyles =
                    [ Css.paddingLeft (Css.px 2)
                    , Css.batch borderStyles
                    , Css.borderWidth4 borderWidth Css.zero borderWidth borderWidth
                    ]
                , styles =
                    [ Css.padding2 (Css.px 4) Css.zero
                    , Css.backgroundColor backgroundColor
                    , Css.batch borderStyles
                    , Css.borderWidth2 borderWidth Css.zero
                    , MediaQuery.highContrastMode
                        [ Css.property "background-color" "Mark"
                        , Css.property "color" "MarkText"
                        , Css.property "forced-color-adjust" "none"
                        ]
                    ]
                , endStyles =
                    [ Css.paddingRight (Css.px 2)
                    , Css.batch borderStyles
                    , Css.borderWidth4 borderWidth borderWidth borderWidth Css.zero
                    ]
                }

        ( Just l, Nothing ) ->
            Just
                { name = Just l
                , startStyles = []
                , styles = []
                , endStyles = []
                }

        ( Nothing, Nothing ) ->
            Nothing


{-| -}
yellow : Attribute
yellow =
    Attribute (\config -> { config | theme = Just Yellow })


{-| -}
cyan : Attribute
cyan =
    Attribute (\config -> { config | theme = Just Cyan })


{-| -}
magenta : Attribute
magenta =
    Attribute (\config -> { config | theme = Just Magenta })


{-| -}
green : Attribute
green =
    Attribute (\config -> { config | theme = Just Green })


{-| -}
blue : Attribute
blue =
    Attribute (\config -> { config | theme = Just Blue })


{-| -}
purple : Attribute
purple =
    Attribute (\config -> { config | theme = Just Purple })


{-| -}
brown : Attribute
brown =
    Attribute (\config -> { config | theme = Just Brown })


{-| -}
class : String -> Attribute
class class_ =
    Attribute (\config -> { config | class = Just class_ })



-- Internals


{-| -}
type Attribute
    = Attribute (Config -> Config)


defaultConfig : Config
defaultConfig =
    { content = []
    , label = Nothing
    , labelOffset = Nothing
    , theme = Nothing
    , class = Nothing
    }


type alias Config =
    { content : List Content
    , label : Maybe String
    , labelOffset : Maybe Float
    , theme : Maybe Theme
    , class : Maybe String
    }


render : Config -> List (Html msg)
render config =
    let
        maybePalette =
            Maybe.map themeToPalette config.theme

        maybeMark =
            toMark config.label maybePalette
    in
    case config.content of
        [] ->
            case maybeMark of
                Just mark ->
                    viewMark
                        (Maybe.withDefault defaultPalette maybePalette)
                        config.class
                        ( [ Blank ]
                        , Just mark
                        , config.labelOffset
                        )

                Nothing ->
                    [ viewBlank config.class ]

        _ ->
            viewMark (Maybe.withDefault defaultPalette maybePalette)
                config.class
                ( config.content, maybeMark, config.labelOffset )


viewMark : Palette -> Maybe String -> ( List Content, Maybe Mark, Maybe Float ) -> List (Html msg)
viewMark palette class_ ( content_, mark, offset ) =
    Mark.viewWithOffsetBalloonTags
        { renderSegment = renderContent class_
        , backgroundColor = palette.backgroundColor
        , maybeMarker = mark
        , offset = offset
        }
        content_


viewBlank : Maybe String -> Html msg
viewBlank class_ =
    span
        [ css
            [ Css.border3 (Css.px 2) Css.dashed Colors.navy
            , MediaQuery.highContrastMode
                [ Css.property "border-color" "CanvasText"
                , Css.property "background-color" "Canvas"
                ]
            , Css.backgroundColor Colors.white
            , Css.minWidth (Css.px 80)
            , Css.display Css.inlineBlock
            , Css.borderRadius (Css.px 4)
            ]
        , AttributesExtra.maybe Attributes.class class_
        ]
        [ span
            [ css
                [ Css.overflowX Css.hidden
                , Css.width (Css.px 0)
                , Css.display Css.inlineBlock
                , Css.verticalAlign Css.bottom
                ]
            ]
            [ text "blank" ]
        ]
