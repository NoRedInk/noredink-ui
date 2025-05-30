module Nri.Ui.Select.V9 exposing
    ( view, generateId
    , Choice, choices
    , ChoicesGroup, groupedChoices
    , value
    , Attribute, defaultDisplayText
    , hiddenLabel, visibleLabel
    , disabled, loading, errorIf, errorMessage, guidance, guidanceHtml
    , custom, nriDescription, id, testId
    , icon
    , containerCss, noMargin
    , batch
    , disableWhen
    )

{-| Build a select input with a label, optional guidance, and error messaging.

Patch changes:

  - Adjust disabled styles
  - Add `disableWhen` attribute

Changes from V8

    - The option `value` attribute is no longer prefixed with `nri-select-`;
      This is not a breaking change to the API, but affects automated tests
      that are looking for this prefix.
    - adds `icon` support
    - when disabled, the select element is now replaced by a div element with
      `aria-disabled="true"` and `tabindex="0"`. This change prevents user
      interactions while maintaining the element in the tab order.
    - sets cursor to `not-allowed` when disabled`

@docs view, generateId


### Input types

@docs Choice, choices
@docs ChoicesGroup, groupedChoices


### Input content

@docs value


### Attributes

@docs Attribute, defaultDisplayText
@docs hiddenLabel, visibleLabel
@docs disabled, loading, errorIf, errorMessage, guidance, guidanceHtml
@docs custom, nriDescription, id, testId
@docs icon
@docs containerCss, noMargin
@docs batch
@docs disableWhen

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css
import Dict
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import InputLabelInternal
import Json.Decode
import Nri.Ui
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.InputStyles.V4 as InputStyles
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import SolidColor


{-| -}
defaultDisplayText : String -> Attribute value
defaultDisplayText text =
    Attribute (\config -> { config | defaultDisplayText = Just text })


{-| -}
value : Maybe value -> Attribute value
value value_ =
    Attribute (\config -> { config | value = value_ })


{-| Sets whether or not the field will be highlighted as having a validation error.

If you have an error message to display, use `errorMessage` instead.

-}
errorIf : Bool -> Attribute value
errorIf =
    Attribute << InputErrorAndGuidanceInternal.setErrorIf


{-| Disables the input
-}
disabled : Attribute value
disabled =
    Attribute (\config -> { config | disabled = True })


{-| Use this while the form the input is a part of is being submitted.
-}
loading : Attribute value
loading =
    Attribute (\config -> { config | loading = True })


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute value
errorMessage =
    Attribute << InputErrorAndGuidanceInternal.setErrorMessage


{-| Combine several attributes into one. A nice way to do nothing via batch []
-}
batch : List (Attribute value) -> Attribute value
batch attrs =
    List.foldl (\(Attribute update) composition -> composition >> update) identity attrs |> Attribute


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute value
guidance =
    Attribute << InputErrorAndGuidanceInternal.setGuidance


{-| A guidance message (HTML) shows below the input, unless an error message is showing instead.
-}
guidanceHtml : List (Html Never) -> Attribute value
guidanceHtml =
    Attribute << InputErrorAndGuidanceInternal.setGuidanceHtml


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute value
hiddenLabel =
    Attribute (\config -> { config | hideLabel = True })


{-| Default behavior.
-}
visibleLabel : Attribute value
visibleLabel =
    Attribute (\config -> { config | hideLabel = False })


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute value
id id_ =
    Attribute (\config -> { config | id = Just id_ })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute value
custom attributes =
    Attribute (\config -> { config | custom = config.custom ++ attributes })


{-| -}
nriDescription : String -> Attribute value
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute value
testId id_ =
    custom [ Extra.testId id_ ]


{-| Add an SVG icon that will render on top of the select by way of absolute positioning.
-}
icon : Svg -> Attribute msg
icon icon_ =
    Attribute <| \attributes -> { attributes | icon = Just icon_ }


{-| Adds CSS to the element containing the input.
-}
containerCss : List Css.Style -> Attribute value
containerCss styles =
    Attribute <|
        \config -> { config | containerCss = config.containerCss ++ styles }


{-| Remove default spacing from the Input.
-}
noMargin : Bool -> Attribute value
noMargin removeMargin =
    Attribute <| \config -> { config | noMarginTop = removeMargin }


{-| Add a `disableWhen` predicate so callers can disable options by value
-}
disableWhen : (value -> Bool) -> Attribute value
disableWhen pred =
    Attribute <|
        \config ->
            { config
                | disableWhen = \v -> config.disableWhen v || pred v
            }


{-| Groupings of choices (will be added _after_ isolated choices.)
-}
type alias ChoicesGroup value =
    { label : String
    , choices : List (Choice value)
    }


{-| -}
groupedChoices : (value -> String) -> List (ChoicesGroup value) -> Attribute value
groupedChoices valueToString optgroups =
    Attribute
        (\config ->
            { config
                | valueToString = Just valueToString
                , optgroups = optgroups
            }
        )


{-| A single possible choice.
-}
type alias Choice value =
    { label : String, value : value }


{-| -}
choices : (value -> String) -> List (Choice value) -> Attribute value
choices valueToString choices_ =
    Attribute
        (\config ->
            { config
                | valueToString = Just valueToString
                , choices = choices_
            }
        )


{-| Customizations for the Select.
-}
type Attribute value
    = Attribute (Config value -> Config value)


applyConfig : List (Attribute value) -> Config value
applyConfig attributes =
    List.foldl (\(Attribute update) config -> update config)
        defaultConfig
        attributes


type alias Config value =
    { id : Maybe String
    , value : Maybe value
    , choices : List (Choice value)
    , optgroups : List (ChoicesGroup value)
    , valueToString : Maybe (value -> String)
    , defaultDisplayText : Maybe String
    , error : ErrorState
    , disabled : Bool
    , loading : Bool
    , guidance : Guidance Never
    , hideLabel : Bool
    , icon : Maybe Svg
    , noMarginTop : Bool
    , containerCss : List Css.Style
    , custom : List (Html.Attribute Never)
    , disableWhen : value -> Bool
    }


defaultConfig : Config value
defaultConfig =
    { id = Nothing
    , value = Nothing
    , choices = []
    , optgroups = []
    , valueToString = Nothing
    , defaultDisplayText = Nothing
    , error = InputErrorAndGuidanceInternal.noError
    , disabled = False
    , loading = False
    , guidance = InputErrorAndGuidanceInternal.noGuidance
    , hideLabel = False
    , icon = Nothing
    , noMarginTop = False
    , containerCss = []
    , custom = []
    , disableWhen = \_ -> False
    }


{-| -}
view : String -> List (Attribute a) -> Html a
view label attributes =
    let
        config =
            applyConfig attributes

        id_ =
            Maybe.withDefault (generateId label) config.id

        ( opacity, disabled_ ) =
            case ( config.disabled, config.loading ) of
                ( False, False ) ->
                    ( Css.num 1, False )

                ( False, True ) ->
                    ( Css.num 0.5, True )

                ( True, _ ) ->
                    ( Css.num 1, True )
    in
    Html.div
        [ css
            ([ Css.position Css.relative
             , Css.opacity opacity
             , if config.noMarginTop then
                Css.batch []

               else
                Css.paddingTop (Css.px InputStyles.defaultMarginTop)
             ]
                ++ config.containerCss
            )
        ]
        ([ viewSelect
            { id = id_
            , disabled = disabled_
            }
            config
         , viewJust (viewIcon config) config.icon
         , InputLabelInternal.view
            { for = id_
            , label = label
            , theme = InputStyles.Standard
            }
            config
         ]
            ++ (InputErrorAndGuidanceInternal.view id_ InputErrorAndGuidanceInternal.smallMargin config
                    |> List.map (Html.map never)
               )
        )


viewIcon : { config | noMarginTop : Bool, disabled : Bool } -> Svg.Svg -> Html a
viewIcon config =
    let
        topPx =
            15
                + (if config.noMarginTop then
                    0

                   else
                    InputStyles.defaultMarginTop
                  )
                + (if config.disabled then
                    1

                   else
                    0
                  )
    in
    Svg.withWidth (Css.px 17)
        >> Svg.withHeight (Css.px 17)
        >> Svg.withCss
            [ Css.position Css.absolute
            , Css.top (Css.px topPx)
            , Css.left (Css.px 15)
            , Css.pointerEvents Css.none
            , if config.disabled then
                Css.color Colors.gray45

              else
                Css.color Colors.navy
            ]
        >> Svg.toHtml


viewSelect : { id : String, disabled : Bool } -> Config a -> Html a
viewSelect config_ config =
    let
        toChoice valueToString choice =
            let
                strValue =
                    valueToString choice.value
            in
            { label = choice.label
            , id = generateId strValue
            , value = choice.value
            , strValue = strValue
            , disabled = config.disableWhen choice.value
            }

        ( optionStringChoices, groupStringChoices ) =
            case config.valueToString of
                Just valueToString ->
                    ( List.map (toChoice valueToString) config.choices
                    , List.concatMap (.choices >> List.map (toChoice valueToString)) config.optgroups
                    )

                Nothing ->
                    ( [], [] )

        valueLookup =
            optionStringChoices
                ++ groupStringChoices
                |> List.map (\x -> ( x.strValue, x.value ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault
                    -- At present, elm/virtual-dom throws this failure away.
                    (Json.Decode.fail
                        ("Nri.Select: could not decode the value: "
                            ++ string
                            ++ "\nexpected one of: "
                            ++ String.join ", " (Dict.keys valueLookup)
                        )
                    )

        onSelectHandler =
            Events.on "change" (Events.targetValue |> Json.Decode.andThen decodeValue)

        defaultOption =
            config.defaultDisplayText
                |> Maybe.map (viewDefaultChoice config.value >> List.singleton)
                |> Maybe.withDefault []

        currentVal =
            if config.value == Nothing && config.defaultDisplayText == Nothing then
                config.choices
                    |> List.head
                    |> Maybe.map .value

            else
                config.value

        viewGroupedChoices group =
            Html.optgroup [ Attributes.attribute "label" group.label ]
                (case config.valueToString of
                    Just valueToString ->
                        List.map
                            (toChoice valueToString >> viewChoice currentVal)
                            group.choices

                    Nothing ->
                        []
                )

        isInError =
            InputErrorAndGuidanceInternal.getIsInError config.error

        selectStyles =
            [ -- border
              Css.border3 (Css.px 1)
                Css.solid
                (if isInError then
                    Colors.purple

                 else if config.disabled then
                    Colors.gray85

                 else
                    Colors.gray75
                )
            , if config.disabled then
                Css.borderBottomWidth (Css.px 1)

              else
                Css.borderBottomWidth (Css.px 3)
            , Css.borderRadius (Css.px 8)
            , Css.focus
                [ Css.borderColor Colors.azure
                , Css.borderRadius (Css.px 8) |> Css.important
                ]

            -- Font and text
            , Fonts.baseFont
            , Css.fontSize (Css.px 15)
            , Css.fontWeight (Css.int 600)
            , Css.textOverflow Css.ellipsis
            , Css.overflow Css.hidden
            , Css.whiteSpace Css.noWrap
            , if config.disabled then
                Css.color Colors.gray45

              else
                Css.color Colors.navy
            , Css.marginTop (Css.px 2)

            -- Interaction
            , Css.cursor
                (if config.disabled then
                    Css.notAllowed

                 else
                    Css.pointer
                )

            -- Size and spacing
            , Css.height (Css.px 45)
            , Css.width (Css.pct 100)
            , case config.icon of
                Just _ ->
                    Css.paddingLeft (Css.px 37)

                Nothing ->
                    Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 35)

            -- Icons
            , selectArrowsCss config
            ]

        selectAttributes =
            [ Aria.invalid isInError
            , InputErrorAndGuidanceInternal.describedBy config_.id config |> Attributes.map never
            , Attributes.id config_.id
            ]
                ++ List.map (Attributes.map never) config.custom

        enabledSelectAttributes =
            [ onSelectHandler
            ]

        disabledSelectAttributes =
            [ Aria.disabled True
            , Key.tabbable True
            , Role.listBox
            ]
    in
    if config_.disabled then
        Nri.Ui.styled Html.div
            "nri-select-menu"
            selectStyles
            (selectAttributes ++ disabledSelectAttributes)
            (case config.valueToString of
                Just valueToString ->
                    [ Html.div [ Attributes.css [ Css.paddingTop (Css.px 12) ] ]
                        [ Html.text
                            (case currentVal of
                                Just val ->
                                    valueToString val

                                Nothing ->
                                    ""
                            )
                        ]
                    ]

                Nothing ->
                    []
            )

    else
        (defaultOption
            ++ List.map (viewChoice currentVal) optionStringChoices
            ++ List.map viewGroupedChoices config.optgroups
        )
            |> Nri.Ui.styled Html.select
                "nri-select-menu"
                selectStyles
                (selectAttributes ++ enabledSelectAttributes)


viewDefaultChoice : Maybe a -> String -> Html a
viewDefaultChoice current displayText =
    Html.option
        [ Attributes.selected (current == Nothing)
        , Attributes.disabled True
        ]
        [ Html.text displayText ]


viewChoice : Maybe a -> { value : a, strValue : String, id : String, label : String, disabled : Bool } -> Html a
viewChoice current choice =
    let
        isSelected =
            current
                |> Maybe.map ((==) choice.value)
                |> Maybe.withDefault False
    in
    Html.option
        [ Attributes.id choice.id
        , Attributes.value choice.strValue
        , Attributes.selected isSelected
        , Attributes.disabled choice.disabled
        ]
        [ Html.text choice.label ]


{-| Pass in the label to generate the default DOM element id used by a `Select.view` with the given label.
-}
generateId : String -> String
generateId x =
    Extra.safeIdWithPrefix "nri-select-" x


selectArrowsCss : { config | disabled : Bool } -> Css.Style
selectArrowsCss config =
    let
        color =
            (if config.disabled then
                Colors.gray45

             else
                Colors.navy
            )
                |> ColorsExtra.fromCssColor
                |> SolidColor.toRGBString
    in
    Css.batch
        [ """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="22px" height="22px" viewBox="0 0 22 22"><g fill=" """
            ++ color
            ++ """ "><path d="M16.2109 9.46973C16.7019 9.87021 17.4261 9.84145 17.8838 9.38379C18.3414 8.92613 18.3702 8.20191 17.9697 7.71094L17.8838 7.61621L11.8838 1.61621C11.3956 1.12807 10.6043 1.12806 10.1162 1.61621L4.1162 7.61621L4.03026 7.71094C3.6298 8.2019 3.65855 8.92614 4.1162 9.38379C4.57385 9.84142 5.29809 9.87018 5.78905 9.46973L5.88377 9.38379L11 4.26758L16.1162 9.38379L16.2109 9.46973Z"/><path d="M16.2109 12.5303C16.7019 12.1298 17.4261 12.1585 17.8838 12.6162C18.3414 13.0739 18.3702 13.7981 17.9697 14.2891L17.8838 14.3838L11.8838 20.3838C11.3956 20.8719 10.6043 20.8719 10.1162 20.3838L4.1162 14.3838L4.03026 14.2891C3.6298 13.7981 3.65855 13.0739 4.1162 12.6162C4.57385 12.1586 5.29809 12.1298 5.78905 12.5303L5.88377 12.6162L11 17.7324L16.1162 12.6162L16.2109 12.5303Z"/></g></svg> """
            |> urlUtf8
            |> Css.property "background"
        , Css.backgroundColor
            (if config.disabled then
                Colors.gray85

             else
                Colors.white
            )

        -- "appearance: none" removes the default dropdown arrows
        , VendorPrefixed.property "appearance" "none"
        , Css.backgroundRepeat Css.noRepeat
        , Css.property "background-position" "center right -23px"
        , Css.property "background-size" "17px"
        , Css.backgroundOrigin Css.contentBox
        ]


urlUtf8 : String -> String
urlUtf8 content =
    """url('data:image/svg+xml;utf8,""" ++ content ++ """')"""
