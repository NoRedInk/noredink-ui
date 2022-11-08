module Nri.Ui.Block.V1 exposing
    ( view, Attribute
    , plaintext, content
    , Content, string, blank
    , emphasize, label
    , yellow, cyan, magenta, green, blue, purple, brown
    )

{-|

@docs view, Attribute


## Content

@docs plaintext, content
@docs Content, string, blank


## Customization

@docs emphasize, label


### Color themes

@docs yellow, cyan, magenta, green, blue, purple, brown

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Style exposing (invisibleStyle)
import Css exposing (Color)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as HighlighterTool exposing (MarkerModel)


{-|

    Block.view [ Block.plaintext "Hello, world!" ]

-}
view : List Attribute -> Html msg
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



-- Content


{-| -}
type Content
    = String_ String
    | Blank


contentToString : Content -> String
contentToString content_ =
    case content_ of
        String_ str ->
            str

        Blank ->
            -- TODO: reimplement the sub-emphasis blank
            "blank"


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


themeToPalette : Theme -> { backgroundColor : Color, borderColor : Color }
themeToPalette theme =
    case theme of
        Emphasis ->
            { backgroundColor = Colors.highlightYellow, borderColor = Colors.highlightYellowDark }

        Yellow ->
            { backgroundColor = Colors.highlightYellow, borderColor = Colors.highlightYellowDark }

        Cyan ->
            { backgroundColor = Colors.highlightCyan, borderColor = Colors.highlightCyanDark }

        Magenta ->
            { backgroundColor = Colors.highlightMagenta, borderColor = Colors.highlightMagentaDark }

        Green ->
            { backgroundColor = Colors.highlightGreen, borderColor = Colors.highlightGreenDark }

        Blue ->
            { backgroundColor = Colors.highlightBlue, borderColor = Colors.highlightBlueDark }

        Purple ->
            { backgroundColor = Colors.highlightPurple, borderColor = Colors.highlightPurpleDark }

        Brown ->
            { backgroundColor = Colors.highlightBrown, borderColor = Colors.highlightBrownDark }


themeToMarker : Maybe String -> Theme -> MarkerModel ()
themeToMarker name theme =
    let
        palette =
            themeToPalette theme
    in
    HighlighterTool.buildStaticMarker
        { highlightColor = palette.backgroundColor
        , borderWidth = Css.px 1
        , borderStyle = Css.dashed
        , borderColor = palette.borderColor
        , name = name
        }


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



-- Internals


{-| -}
type Attribute
    = Attribute (Config -> Config)


defaultConfig : Config
defaultConfig =
    { content = []
    , label = Nothing
    , theme = Nothing
    }


type alias Config =
    { content : List Content
    , label : Maybe String
    , theme : Maybe Theme
    }


render : Config -> Html msg
render config =
    case config.content of
        [] ->
            viewBlank

        _ ->
            span []
                (List.map
                    (\c ->
                        Highlighter.staticSegment
                            (Highlightable.init Highlightable.Static
                                (Maybe.map (themeToMarker config.label) config.theme)
                                0
                                ( [], contentToString c )
                            )
                    )
                    config.content
                )


viewBlank : Html msg
viewBlank =
    span
        [ css
            [ Css.border3 (Css.px 2) Css.dashed Colors.navy
            , Css.backgroundColor Colors.white
            , Css.width (Css.px 80)
            , Css.display Css.inlineBlock
            , Css.padding (Css.px 10)
            , Css.borderRadius (Css.px 4)
            , Css.verticalAlign Css.middle
            ]
        ]
        [ span [ css [ invisibleStyle ] ] [ text "blank" ] ]
