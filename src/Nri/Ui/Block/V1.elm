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
import Nri.Ui.HighlighterTool.V1 as HighlighterTool
import Nri.Ui.Mark.V1 as Mark exposing (Mark)
import Nri.Ui.MediaQuery.V1 as MediaQuery


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


renderContent : Content -> Html msg
renderContent content_ =
    case content_ of
        String_ str ->
            text str

        Blank ->
            viewBlank


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


type alias Palette =
    { backgroundColor : Color, borderColor : Color }


paletteToMark : Maybe String -> Palette -> Mark
paletteToMark label_ { backgroundColor, borderColor } =
    let
        borderWidth =
            Css.px 1

        borderStyles =
            [ Css.borderStyle Css.dashed
            , Css.borderColor borderColor
            ]
    in
    { name = label_
    , startStyles =
        [ Css.paddingLeft (Css.px 2)
        , Css.batch borderStyles
        , Css.borderWidth4 borderWidth Css.zero borderWidth borderWidth
        ]
    , styles =
        [ Css.padding2 (Css.px 4) Css.zero
        , Css.backgroundColor backgroundColor
        , MediaQuery.highContrastMode [ Css.property "background-color" "Mark" ]
        ]
    , endStyles =
        [ Css.paddingRight (Css.px 2)
        , Css.batch borderStyles
        , Css.borderWidth4 borderWidth borderWidth borderWidth Css.zero
        ]
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
            let
                maybeMark =
                    Maybe.map (themeToPalette >> paletteToMark config.label) config.theme
            in
            Mark.view
                { showTagsInline = False }
                (\content_ markStyles ->
                    span [ css markStyles ]
                        (List.map renderContent content_)
                )
                [ ( config.content, maybeMark ) ]
                |> span []


viewBlank : Html msg
viewBlank =
    span
        [ css
            [ Css.border3 (Css.px 2) Css.dashed Colors.navy
            , Css.backgroundColor Colors.white
            , Css.width (Css.px 80)
            , Css.display Css.inlineBlock
            , Css.height (Css.px 18)
            , Css.borderRadius (Css.px 4)
            , Css.verticalAlign Css.textBottom
            ]
        ]
        [ span [ css [ invisibleStyle ] ] [ text "blank" ] ]
