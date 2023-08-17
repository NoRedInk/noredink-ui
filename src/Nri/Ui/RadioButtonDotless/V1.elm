module Nri.Ui.RadioButtonDotless.V1 exposing (Attribute, id, onSelect, view)

{-| Looks like a standard button, behaves like a radio button
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


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config value msg =
    { id : Maybe String
    , onSelect : Maybe (value -> msg)
    }


id : String -> Attribute value msg
id id_ =
    Attribute <| \config -> { config | id = Just id_ }


onSelect : (value -> msg) -> Attribute value msg
onSelect onSelect_ =
    Attribute <| \config -> { config | onSelect = Just onSelect_ }


applyConfig : List (Attribute value msg) -> Config value msg -> Config value msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


emptyConfig : Config value msg
emptyConfig =
    { id = Nothing
    , onSelect = Nothing
    }


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
            [ position relative
            , display inlineBlock
            , textAlign center
            ]
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
                    [ property "word-break" "break-word"
                    , padding2 (px 10) (px 20)
                    , display inlineBlock
                    ]
                ]
                [ text label ]
            ]
        ]
