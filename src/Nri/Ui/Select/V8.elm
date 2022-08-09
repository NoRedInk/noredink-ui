module Nri.Ui.Select.V8 exposing
    ( view, generateId
    , Choice, choices
    , ChoicesGroup, groupedChoices
    , value
    , Attribute, defaultDisplayText
    , hiddenLabel, visibleLabel
    , disabled, loading, errorIf, errorMessage, guidance
    , custom, nriDescription, id, testId
    , containerCss, noMargin
    )

{-| Build a select input with a label, optional guidance, and error messaging.


# Changes from V7

    - view adds a label
    - adds standard custom, nriDescription, etc. attributes
    - switches to a list-based attribute API from a record-based API

@docs view, generateId


### Input types

@docs Choice, choices
@docs ChoicesGroup, groupedChoices


### Input content

@docs value


### Attributes

@docs Attribute, defaultDisplayText
@docs hiddenLabel, visibleLabel
@docs disabled, loading, errorIf, errorMessage, guidance
@docs custom, nriDescription, id, testId
@docs containerCss, noMargin

-}

import Accessibility.Styled as Html exposing (Html)
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
import Nri.Ui.InputStyles.V3 as InputStyles
import Nri.Ui.Util
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


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute value
guidance =
    Attribute << InputErrorAndGuidanceInternal.setGuidance


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
    , guidance : Guidance
    , hideLabel : Bool
    , noMarginTop : Bool
    , containerCss : List Css.Style
    , custom : List (Html.Attribute Never)
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
    , noMarginTop = False
    , containerCss = []
    , custom = []
    }


{-| -}
view : String -> List (Attribute a) -> Html a
view label attributes =
    let
        config =
            applyConfig attributes

        isInError_ =
            InputErrorAndGuidanceInternal.getIsInError config.error

        id_ =
            Maybe.withDefault (generateId label) config.id

        ( opacity, disabled_ ) =
            case ( config.disabled, config.loading ) of
                ( False, False ) ->
                    ( Css.num 1, False )

                ( False, True ) ->
                    ( Css.num 0.5, True )

                ( True, _ ) ->
                    ( Css.num 0.4, True )
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
        [ viewSelect
            { choices = config.choices
            , optgroups = config.optgroups
            , current = config.value
            , id = id_
            , custom = config.custom
            , valueToString = config.valueToString
            , defaultDisplayText = config.defaultDisplayText
            , isInError = isInError_
            , disabled = disabled_
            }
        , InputLabelInternal.view
            { for = id_
            , label = label
            , theme = InputStyles.Standard
            }
            config
        , InputErrorAndGuidanceInternal.view id_ config
        ]


viewSelect :
    { choices : List (Choice a)
    , optgroups : List (ChoicesGroup a)
    , current : Maybe a
    , id : String
    , valueToString : Maybe (a -> String)
    , defaultDisplayText : Maybe String
    , isInError : Bool
    , disabled : Bool
    , custom : List (Html.Attribute Never)
    }
    -> Html a
viewSelect config =
    let
        toChoice valueToString choice =
            { label = choice.label
            , idAndValue = generateId (valueToString choice.value)
            , value = choice.value
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
                |> List.map (\x -> ( x.idAndValue, x.value ))
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
                |> Maybe.map (viewDefaultChoice config.current >> List.singleton)
                |> Maybe.withDefault []

        currentVal =
            if config.current == Nothing && config.defaultDisplayText == Nothing then
                config.choices
                    |> List.head
                    |> Maybe.map .value

            else
                config.current

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
    in
    (defaultOption
        ++ List.map (viewChoice currentVal) optionStringChoices
        ++ List.map viewGroupedChoices config.optgroups
    )
        |> Nri.Ui.styled Html.select
            "nri-select-menu"
            [ -- border
              Css.border3 (Css.px 1)
                Css.solid
                (if config.isInError then
                    Colors.purple

                 else
                    Colors.gray75
                )
            , Css.borderBottomWidth (Css.px 3)
            , Css.borderRadius (Css.px 8)
            , Css.focus [ Css.borderColor Colors.azure ]

            -- Font and color
            , Css.color Colors.gray20
            , Fonts.baseFont
            , Css.fontSize (Css.px 15)
            , Css.fontWeight (Css.int 600)
            , Css.textOverflow Css.ellipsis
            , Css.overflow Css.hidden
            , Css.whiteSpace Css.noWrap

            -- Interaction
            , Css.cursor
                (if config.disabled then
                    Css.default

                 else
                    Css.pointer
                )

            -- Size and spacing
            , Css.height (Css.px 45)
            , Css.width (Css.pct 100)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 30)

            -- Icons
            , selectArrowsCss
            ]
            (onSelectHandler
                :: Attributes.id config.id
                :: Attributes.disabled config.disabled
                :: List.map (Attributes.map never) config.custom
            )


viewDefaultChoice : Maybe a -> String -> Html a
viewDefaultChoice current displayText =
    Html.option
        [ Attributes.selected (current == Nothing)
        , Attributes.disabled True
        ]
        [ Html.text displayText ]


viewChoice : Maybe a -> { value : a, idAndValue : String, label : String } -> Html a
viewChoice current choice =
    let
        isSelected =
            current
                |> Maybe.map ((==) choice.value)
                |> Maybe.withDefault False
    in
    Html.option
        [ Attributes.id choice.idAndValue
        , Attributes.value choice.idAndValue
        , Attributes.selected isSelected
        ]
        [ Html.text choice.label ]


{-| Pass in the label to generate the default DOM element id used by a `Select.view` with the given label.
-}
generateId : String -> String
generateId x =
    "nri-select-" ++ Nri.Ui.Util.dashify (Nri.Ui.Util.removePunctuation x)


selectArrowsCss : Css.Style
selectArrowsCss =
    let
        color =
            SolidColor.toRGBString (ColorsExtra.fromCssColor Colors.azure)
    in
    Css.batch
        [ """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="12px" height="16px" viewBox="0 0 12 16"><g fill=" """
            ++ color
            ++ """ "><path d="M2.10847,9.341803 C1.65347,8.886103 0.91427,8.886103 0.45857,9.341803 C0.23107,9.570003 0.11697,9.868203 0.11697,10.167103 C0.11697,10.465303 0.23107,10.763503 0.45857,10.991703 L5.12547,15.657903 C5.57977,16.114303 6.31897,16.114303 6.77537,15.657903 L11.44157,10.991703 C11.89727,10.536003 11.89727,9.797503 11.44157,9.341803 C10.98657,8.886103 10.24667,8.886103 9.79167,9.341803 L5.95007,13.182703 L2.10847,9.341803 Z"/><path d="M1.991556,6.658179 C1.536659,7.11394 0.797279,7.11394 0.3416911,6.658179 C0.1140698,6.43004 0,6.13173 0,5.83325 C0,5.53476 0.1140698,5.23645 0.3416911,5.00831 L5.008185,0.34182 C5.463081,-0.11394 6.202461,-0.11394 6.65805,0.34182 L11.32454,5.00831 C11.78031,5.4639 11.78031,6.202592 11.32454,6.658179 C10.86965,7.11394 10.13027,7.11394 9.674679,6.658179 L5.833118,2.81679 L1.991556,6.658179 Z"/></g></svg> """
            |> urlUtf8
            |> Css.property "background"
        , Css.backgroundColor Colors.white

        -- "appearance: none" removes the default dropdown arrows
        , VendorPrefixed.property "appearance" "none"
        , Css.backgroundRepeat Css.noRepeat
        , Css.property "background-position" "center right -20px"
        , Css.backgroundOrigin Css.contentBox
        ]


urlUtf8 : String -> String
urlUtf8 content =
    """url('data:image/svg+xml;utf8,""" ++ content ++ """')"""
