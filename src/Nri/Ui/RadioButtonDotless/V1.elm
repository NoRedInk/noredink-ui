module Nri.Ui.RadioButtonDotless.V1 exposing
    ( view
    , Attribute
    , onSelect
    , unboundedWidth, fillContainerWidth
    , textAlignCenter, textAlignLeft
    , small, medium, large
    , enabled, disabled
    , containerCss, labelCss
    , id, custom, nriDescription, testId
    )

{-| Looks like a standard button, behaves like a radio button


### Patch changes:

    - Introduce `enabled`/`disabled` attributes and styles
    - Support markdown for radio button contents (like Button does)


# Create a radio button

@docs view
@docs Attribute


## Behavior

@docs onSelect


## Customization

@docs unboundedWidth, fillContainerWidth
@docs textAlignCenter, textAlignLeft
@docs small, medium, large
@docs enabled, disabled
@docs containerCss, labelCss


## Attributes

@docs id, custom, nriDescription, testId

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Content
import Css exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
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


type ButtonSize
    = Small
    | Medium
    | Large


type State msg
    = Enabled
    | Disabled msg


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config value msg =
    { id : Maybe String
    , onSelect : Maybe (value -> msg)
    , width : ButtonWidth
    , textAlign : TextAlign
    , size : ButtonSize
    , state : State msg
    , containerCss : List Style
    , labelCss : List Style
    , customAttributes : List (Html.Attribute Never)
    }


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


{-| A small sized button (~36px tall)
-}
small : Attribute value msg
small =
    Attribute <| \config -> { config | size = Small }


{-| A medium sized button (~45px tall - this is the default)
-}
medium : Attribute value msg
medium =
    Attribute <| \config -> { config | size = Medium }


{-| A large sized button (~56px tall)
-}
large : Attribute value msg
large =
    Attribute <| \config -> { config | size = Large }


{-| Enable the input (this is the default)
-}
enabled : Attribute value msg
enabled =
    Attribute <| \config -> { config | state = Enabled }


{-| Disable the input
-}
disabled : msg -> Attribute value msg
disabled noOp =
    Attribute <| \config -> { config | state = Disabled noOp }


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


{-| Set a unique identifier for the button. This id will also be used as the root name for sub elements.
-}
id : String -> Attribute value msg
id id_ =
    Attribute <| \config -> { config | id = Just id_ }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `containerCss` or `labelCss` helper.

-}
custom : List (Html.Attribute Never) -> Attribute value msg
custom attributes =
    Attribute <| \config -> { config | customAttributes = config.customAttributes ++ attributes }


{-| Set the "data-nri-description" attribute
-}
nriDescription : String -> Attribute value msg
nriDescription description =
    custom [ AttributeExtra.nriDescription description ]


{-| See Cypress best practices: <https://docs.cypress.io/guides/references/best-practices.html#Selecting-Elements>
-}
testId : String -> Attribute value msg
testId id_ =
    custom [ AttributeExtra.testId id_ ]


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
    , size = Medium
    , state = Enabled
    , containerCss = []
    , labelCss = []
    , customAttributes = []
    }


{-| View the single dotless radio button

`label` supports bold and italic markdown syntax

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

        isDisabled =
            case config.state of
                Disabled _ ->
                    True

                Enabled ->
                    False
    in
    span
        [ Attributes.class "Nri-RadioButton-Dotless"
        , css
            ([ position relative
             , display inlineBlock
             , textAlign center
             , pseudoClass "focus-within" FocusRing.styles
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
            ([ Attributes.id idValue
             , Attributes.class "Nri-RadioButton-HiddenRadioInput"
             , Aria.disabled isDisabled
             , case ( config.state, config.onSelect ) of
                ( Disabled noOp, _ ) ->
                    Events.custom "click" (Json.Decode.succeed { message = noOp, stopPropagation = False, preventDefault = True })

                ( Enabled, Just onSelect_ ) ->
                    Events.onClick (onSelect_ value)

                ( Enabled, Nothing ) ->
                    AttributeExtra.none
             , css
                [ position absolute
                , opacity zero
                ]
             ]
                ++ List.map (Attributes.map never) config.customAttributes
            )
        , Html.label
            [ Attributes.for idValue
            , Attributes.classList
                [ ( "Nri-RadioButton-RadioButton", True )
                , ( "Nri-RadioButton-RadioButtonChecked", isChecked )
                ]
            , css
                [ displayFlex
                , alignItems center
                , borderRadius (px 8)
                , borderWidth (px 2)
                , borderStyle solid
                , Fonts.baseFont
                , fontWeight (int 600)
                , if isDisabled then
                    cursor notAllowed

                  else
                    cursor pointer
                , case config.textAlign of
                    TextAlignLeft ->
                        Css.batch
                            [ justifyContent flexStart
                            , textAlign left
                            ]

                    TextAlignCenter ->
                        Css.batch
                            [ justifyContent center
                            , textAlign center
                            ]
                , case config.size of
                    Small ->
                        Css.batch
                            [ minHeight (px 35)
                            , fontSize (px 15)
                            , lineHeight (px 23)
                            ]

                    Medium ->
                        Css.batch
                            [ minHeight (px 45)
                            , fontSize (px 15)
                            , lineHeight (px 23)
                            ]

                    Large ->
                        Css.batch
                            [ minHeight (px 55)
                            , fontSize (px 18)
                            , lineHeight (px 28)
                            ]
                , width (pct 100)
                , case ( isChecked, isDisabled ) of
                    ( True, False ) ->
                        Css.batch
                            [ color Colors.navy
                            , backgroundColor Colors.glacier
                            , borderColor Colors.azure
                            ]

                    ( False, False ) ->
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

                    ( True, True ) ->
                        Css.batch
                            [ color Colors.gray45
                            , backgroundColor Colors.gray92
                            , borderColor Colors.gray45
                            ]

                    ( False, True ) ->
                        Css.batch
                            [ color Colors.gray45
                            , backgroundColor Colors.gray92
                            , borderWidth zero
                            ]
                ]
            ]
            [ span
                [ css
                    ([ padding2 (px 10) (px 20)
                     , display inlineBlock
                     ]
                        ++ config.labelCss
                    )
                ]
                (Content.markdownInline label)
            ]
        ]
