module Nri.Ui.RadioButtonDotless.V1 exposing
    ( view
    , Attribute
    , onSelect
    , id, containerCss, labelCss
    , unboundedWidth, fillContainerWidth
    , textAlignCenter, textAlignLeft
    )

{-| Looks like a standard button, behaves like a radio button


# Create a radio button

@docs view
@docs Attribute


## Behavior

@docs onSelect


## Customization

@docs id, containerCss, labelCss
@docs unboundedWidth, fillContainerWidth
@docs textAlignCenter, textAlignLeft

-}

import Accessibility.Styled exposing (..)
import Css exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributeExtra


{-| Customizations for the RadioButton.
-}
type Attribute value msg
    = Attribute (Config value msg -> Config value msg)


type ButtonWidth
    = UnboundedWidth
    | FillContainerWidth


type TextAlign
    = TextAlignLeft
    | TextAlignCenter


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config value msg =
    { id : Maybe String
    , onSelect : Maybe (value -> msg)
    , width : ButtonWidth
    , textAlign : TextAlign
    , containerCss : List Style
    , labelCss : List Style
    }


{-| Set a unique identifier for the button. This id will be used as the root name for sub elements.
-}
id : String -> Attribute value msg
id id_ =
    Attribute <| \config -> { config | id = Just id_ }


{-| Fire a message upon selection of this radio button option
-}
onSelect : (value -> msg) -> Attribute value msg
onSelect onSelect_ =
    Attribute <| \config -> { config | onSelect = Just onSelect_ }


{-| Leave the width unbounded (this is the default)
-}
unboundedWidth : Attribute value msg
unboundedWidth =
    Attribute <| \config -> { config | width = UnboundedWidth }


{-| The button will attempt to fill the width of its container
-}
fillContainerWidth : Attribute value msg
fillContainerWidth =
    Attribute <| \config -> { config | width = FillContainerWidth }


{-| Align the text to the center in the button
-}
textAlignCenter : Attribute value msg
textAlignCenter =
    Attribute <| \config -> { config | textAlign = TextAlignCenter }


{-| Align the text to the left in the button
-}
textAlignLeft : Attribute value msg
textAlignLeft =
    Attribute <| \config -> { config | textAlign = TextAlignLeft }


{-| Adds CSS to the element containing the input.
-}
containerCss : List Css.Style -> Attribute value msg
containerCss styles =
    Attribute <| \config -> { config | containerCss = config.containerCss ++ styles }


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element.

-}
labelCss : List Css.Style -> Attribute value msg
labelCss styles =
    Attribute <| \config -> { config | labelCss = config.labelCss ++ styles }


applyConfig : List (Attribute value msg) -> Config value msg -> Config value msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


emptyConfig : Config value msg
emptyConfig =
    { id = Nothing
    , onSelect = Nothing
    , width = UnboundedWidth
    , textAlign = TextAlignCenter
    , containerCss = []
    , labelCss = []
    }


{-| View the single dotless radio button
-}
view :
    { label : String
    , name : String
    , value : value
    , valueToString : value -> String
    , selectedValue : Maybe value
    }
    -> List (Attribute value msg)
    -> Html msg
view { label, name, value, valueToString, selectedValue } attributes =
    let
        config =
            applyConfig attributes emptyConfig

        idValue =
            case config.id of
                Just specificId ->
                    specificId

                Nothing ->
                    AttributeExtra.safeId (name ++ "-" ++ valueToString value)

        isChecked =
            selectedValue == Just value
    in
    span
        [ css
            ([ position relative
             , display inlineBlock
             , textAlign center
             , case config.width of
                UnboundedWidth ->
                    width unset

                FillContainerWidth ->
                    width (pct 100)
             ]
                ++ config.containerCss
            )
        ]
        [ radio name
            (valueToString value)
            isChecked
            [ Attributes.id idValue
            , case config.onSelect of
                Just onSelect_ ->
                    Events.onClick (onSelect_ value)

                Nothing ->
                    AttributeExtra.none
            , css
                [ position absolute
                , opacity zero
                ]
            ]
        , Html.label
            [ Attributes.for idValue
            , css
                [ display inlineBlock
                , borderRadius (px 8)
                , borderWidth (px 2)
                , borderStyle solid
                , Fonts.baseFont
                , fontSize (px 18)
                , cursor pointer
                , case config.textAlign of
                    TextAlignLeft ->
                        textAlign left

                    TextAlignCenter ->
                        textAlign center
                , width (pct 100)
                , if isChecked then
                    Css.batch
                        [ color Colors.navy
                        , backgroundColor Colors.glacier
                        , borderColor Colors.azure
                        ]

                  else
                    Css.batch
                        [ color Colors.azure
                        , backgroundColor Colors.white
                        , borderColor Colors.glacier
                        , hover
                            [ color Colors.navy
                            , backgroundColor Colors.glacier
                            , borderColor Colors.glacier
                            ]
                        ]
                ]
            ]
            [ span
                [ css
                    ([ property "word-break" "break-word"
                     , padding2 (px 10) (px 20)
                     , display inlineBlock
                     ]
                        ++ config.labelCss
                    )
                ]
                [ text label ]
            ]
        ]
