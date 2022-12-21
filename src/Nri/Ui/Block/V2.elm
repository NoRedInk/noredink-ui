module Nri.Ui.Block.V2 exposing
    ( view, Attribute
    , plaintext, content
    , Content, phrase, blank
    , emphasize
    , label
    , labelId, labelContentId
    , LabelPosition, getLabelPositions, labelPosition
    , bottomSpacingPx
    , yellow, cyan, magenta, green, blue, purple, brown
    , withQuestionBox
    , class
    )

{-|

@docs view, Attribute


## Content

@docs plaintext, content
@docs Content, phrase, blank


## Content customization

@docs emphasize


## Labels & positioning

@docs label

You will need these helpers if you want to prevent label overlaps. (Which is to say -- anytime you have labels!)

@docs labelId, labelContentId
@docs LabelPosition, getLabelPositions, labelPosition

@docs bottomSpacingPx


### Visual customization

@docs yellow, cyan, magenta, green, blue, purple, brown


### Add a question box

@docs withQuestionBox


### Possibly deprecated???

@docs class

-}

import Accessibility.Styled exposing (..)
import Browser.Dom as Dom
import Css exposing (Color)
import Dict exposing (Dict)
import Html.Styled.Attributes as Attributes exposing (css)
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (nriDescription)
import Nri.Ui.Mark.V2 as Mark exposing (Mark)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.QuestionBox.V2 as QuestionBox
import Position exposing (xOffsetPx)


{-|

    Block.view [ Block.plaintext "Hello, world!" ]

-}
view : List (Attribute msg) -> List (Html msg)
view attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) defaultConfig
        |> render



-- Attributes


{-| Provide the main content of the block as a plain-text string. You can also use `content` for more complex cases, including a blank appearing within an emphasis.
-}
plaintext : String -> Attribute msg
plaintext content_ =
    Attribute <| \config -> { config | content = parseString content_ }


{-| Use `content` for more complex block views, for instance when a blank appears within an emphasis block. Prefer to use `plaintext` when possible for better readability.

    Block.view
        [ Block.emphasize
        , Block.content (Block.phrase "Hello, " ++  Block.blank :: Block.phrase "!" ) ]
        ]

-}
content : List Content -> Attribute msg
content content_ =
    Attribute <| \config -> { config | content = content_ }


{-| Mark content as emphasized.
-}
emphasize : Attribute msg
emphasize =
    Attribute <| \config -> { config | emphasize = True }


{-| -}
label : String -> Attribute msg
label label_ =
    Attribute <| \config -> { config | label = Just label_, emphasize = True }


{-| Use `getLabelPositions` to construct this value.
-}
type alias LabelPosition =
    { totalHeight : Float
    , arrowHeight : Float
    , zIndex : Int
    , xOffset : Float
    }


{-| Use `getLabelPositions` to calculate what these values should be.
-}
labelPosition : Maybe LabelPosition -> Attribute msg
labelPosition offset =
    Attribute <| \config -> { config | labelPosition = offset }


{-| When using a `QuestionBox` that is `pointingTo` a block, you may want to add bottom spacing to the block in order to avoid having the QuestionBox cover meaningful content.
-}
bottomSpacingPx : Maybe Float -> Attribute msg
bottomSpacingPx offset =
    Attribute <| \config -> { config | bottomSpacingPx = offset }


{-| -}
labelContentId : String -> String
labelContentId labelId_ =
    labelId_ ++ "-label-content"


{-| Determine where to position labels in order to dynamically avoid overlapping content.

    - First, we add ids to block with labels with `Block.labelId`.
    - Say we added `Block.labelId "example-id"`, then we will use `Browser.Dom.getElement "example-id"` and `Browser.Dom.getElement (Block.labelContentId "example-id")` to construct a record in the shape { label : Dom.Element, labelContent : Dom.Element }. We store this record in a dictionary keyed by ids (e.g., "example-id") with measurements for all other labels.

`getLabelPositions` will return a dictionary of values (keyed by label ids) whose values can be passed directly to `labelPosition` for positioning.

-}
getLabelPositions :
    Dict String { label : Dom.Element, labelContent : Dom.Element }
    -> Dict String LabelPosition
getLabelPositions labelMeasurementsById =
    let
        startingArrowHeight =
            8
    in
    Dict.toList labelMeasurementsById
        |> splitByOverlaps
        |> List.concatMap
            (\row ->
                let
                    maxRowIndex =
                        List.length row - 1
                in
                row
                    -- Put the widest elements higher visually to avoid overlaps
                    |> List.sortBy (Tuple.second >> .labelContent >> .element >> .width)
                    |> List.foldl
                        (\( idString, e ) ( index, height, acc ) ->
                            ( index + 1
                            , height + e.labelContent.element.height + 4
                            , ( idString
                              , { totalHeight = height + e.labelContent.element.height
                                , arrowHeight = height
                                , zIndex = maxRowIndex - index
                                , xOffset = xOffsetPx e.label
                                }
                              )
                                :: acc
                            )
                        )
                        ( 0, startingArrowHeight, [] )
                    |> (\( _, _, v ) -> v)
            )
        |> Dict.fromList


{-| Group the elements whose bottom edges are at the same height. This ensures that we only offset labels against other labels in the same visual line of content.

Then, for elements in the same row, group elements with horizontal overlaps .

-}
splitByOverlaps : List ( id, { a | label : Dom.Element } ) -> List (List ( id, { a | label : Dom.Element } ))
splitByOverlaps =
    -- consider the elements from top to bottom
    groupWithSort (\( _, a ) -> a.label.element.y + a.label.element.height)
        (\( _, a ) ( _, b ) ->
            (a.label.element.y + a.label.element.height) == (b.label.element.y + b.label.element.height)
        )
        >> List.concatMap
            -- consider the elements from left to right
            (groupWithSort (\( _, a ) -> a.label.element.x)
                (\( _, a ) ( _, b ) ->
                    (a.label.element.x + xOffsetPx a.label + a.label.element.width) >= (b.label.element.x + xOffsetPx b.label)
                )
            )


groupWithSort : (a -> comparable) -> (a -> a -> Bool) -> List a -> List (List a)
groupWithSort sortBy groupBy =
    List.sortBy sortBy
        >> List.Extra.groupWhile groupBy
        >> List.map (\( first, rem ) -> first :: rem)



-- Content


{-| -}
type Content
    = String_ String
    | Blank
    | FullHeightBlank


parseString : String -> List Content
parseString =
    String.split " "
        >> List.intersperse " "
        >> List.filter (\str -> str /= "")
        >> List.map String_


renderContent :
    { config | class : Maybe String, bottomSpacingPx : Maybe Float }
    -> Content
    -> List Css.Style
    -> Html msg
renderContent config content_ markStyles =
    let
        marginBottom =
            case config.bottomSpacingPx of
                Just by ->
                    Css.important (Css.marginBottom (Css.px by))

                Nothing ->
                    Css.batch []
    in
    span
        [ css (Css.whiteSpace Css.preWrap :: marginBottom :: markStyles)
        , nriDescription "block-segment-container"
        , AttributesExtra.maybe Attributes.class config.class
        ]
        (case content_ of
            String_ str ->
                [ text str ]

            Blank ->
                [ viewBlank [ Css.lineHeight (Css.int 1) ] { class = Nothing } ]

            FullHeightBlank ->
                [ viewBlank
                    [ Css.paddingTop topBottomSpace
                    , Css.paddingBottom topBottomSpace
                    , Css.lineHeight Css.initial
                    ]
                    { class = Nothing }
                ]
        )


{-| -}
phrase : String -> List Content
phrase =
    parseString


{-| You will only need to use this helper if you're also using `content` to construct a more complex Block. For a less complex blank Block, don't include content or plaintext in the list of attributes.
-}
blank : Content
blank =
    Blank



-- Color themes


{-| -}
type Theme
    = Yellow
    | Cyan
    | Magenta
    | Green
    | Blue
    | Purple
    | Brown


themeToPalette : Theme -> Palette
themeToPalette theme =
    case theme of
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


toMark : { config | emphasize : Bool, label : Maybe String } -> Palette -> Maybe Mark
toMark config { backgroundColor, borderColor } =
    case ( config.label, config.emphasize ) of
        ( _, True ) ->
            let
                borderWidth =
                    Css.px 1

                borderStyle =
                    Css.dashed
            in
            Just
                { name = config.label
                , startStyles =
                    [ Css.borderLeft3 borderWidth borderStyle borderColor
                    , Css.paddingLeft (Css.px 2)
                    ]
                , styles =
                    [ Css.paddingTop topBottomSpace
                    , Css.paddingBottom topBottomSpace
                    , Css.backgroundColor backgroundColor
                    , Css.borderTop3 borderWidth borderStyle borderColor
                    , Css.borderBottom3 borderWidth borderStyle borderColor
                    , MediaQuery.highContrastMode
                        [ Css.property "background-color" "Mark"
                        , Css.property "color" "MarkText"
                        , Css.property "forced-color-adjust" "none"
                        ]
                    ]
                , endStyles =
                    [ Css.borderRight3 borderWidth borderStyle borderColor
                    , Css.paddingRight (Css.px 2)
                    ]
                }

        ( Just l, False ) ->
            Just
                { name = Just l
                , startStyles = []
                , styles = []
                , endStyles = []
                }

        ( Nothing, False ) ->
            Nothing


topBottomSpace : Css.Px
topBottomSpace =
    Css.px 4


{-| -}
yellow : Attribute msg
yellow =
    Attribute (\config -> { config | theme = Yellow })


{-| -}
cyan : Attribute msg
cyan =
    Attribute (\config -> { config | theme = Cyan })


{-| -}
magenta : Attribute msg
magenta =
    Attribute (\config -> { config | theme = Magenta })


{-| -}
green : Attribute msg
green =
    Attribute (\config -> { config | theme = Green })


{-| -}
blue : Attribute msg
blue =
    Attribute (\config -> { config | theme = Blue })


{-| -}
purple : Attribute msg
purple =
    Attribute (\config -> { config | theme = Purple })


{-| -}
brown : Attribute msg
brown =
    Attribute (\config -> { config | theme = Brown })


{-| -}
class : String -> Attribute msg
class class_ =
    Attribute (\config -> { config | class = Just class_ })


{-| -}
labelId : String -> Attribute msg
labelId id_ =
    Attribute (\config -> { config | labelId = Just id_ })


{-| -}
withQuestionBox : List (QuestionBox.Attribute msg) -> Attribute msg
withQuestionBox attributes =
    Attribute (\config -> { config | questionBox = attributes })



-- Internals


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


defaultConfig : Config msg
defaultConfig =
    { content = []
    , label = Nothing
    , labelId = Nothing
    , labelPosition = Nothing
    , theme = Yellow
    , emphasize = False
    , class = Nothing
    , bottomSpacingPx = Nothing
    , questionBox = []
    }


type alias Config msg =
    { content : List Content
    , label : Maybe String
    , labelId : Maybe String
    , labelPosition : Maybe LabelPosition
    , theme : Theme
    , emphasize : Bool
    , class : Maybe String
    , bottomSpacingPx : Maybe Float
    , questionBox : List (QuestionBox.Attribute msg)
    }


render : Config msg -> List (Html msg)
render config =
    let
        palette =
            themeToPalette config.theme

        maybeMark =
            toMark config palette
    in
    case config.content of
        [] ->
            case maybeMark of
                Just mark ->
                    Mark.viewWithBalloonTags
                        { renderSegment = renderContent config
                        , backgroundColor = palette.backgroundColor
                        , maybeMarker =
                            Just
                                { name = config.label
                                , startStyles = []
                                , styles = []
                                , endStyles = []
                                }
                        , labelPosition = config.labelPosition
                        , labelId = config.labelId
                        , labelContentId = Maybe.map labelContentId config.labelId
                        }
                        [ FullHeightBlank ]

                Nothing ->
                    [ viewBlank
                        [ Css.paddingTop topBottomSpace
                        , Css.paddingBottom topBottomSpace
                        , case config.bottomSpacingPx of
                            Just by ->
                                Css.marginBottom (Css.px by)

                            Nothing ->
                                Css.batch []
                        ]
                        config
                    ]

        _ ->
            Mark.viewWithBalloonTags
                { renderSegment = renderContent config
                , backgroundColor = palette.backgroundColor
                , maybeMarker = maybeMark
                , labelPosition = config.labelPosition
                , labelId = config.labelId
                , labelContentId = Maybe.map labelContentId config.labelId
                }
                config.content


viewBlank : List Css.Style -> { config | class : Maybe String } -> Html msg
viewBlank styles config =
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
            , Css.batch styles
            ]
        , AttributesExtra.maybe Attributes.class config.class
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
