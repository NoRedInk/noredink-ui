module Nri.Ui.Select.V8 exposing
    ( view, generateId
    , id
    , errorIf, errorMessage
    )

{-| Build a select input with a label, optional guidance, and error messaging.


# Changes from V5

    -  view adds a label
    - `Choice` is no longer exposed
    - adds `generateId`

@docs view, generateId


### Input types


### Input content


### Input handlers


### Attributes

@docs Attribute, placeholder, hiddenLabel, autofocus
@docs css, custom, nriDescription, id, testId, noMargin
@docs disabled, loading, errorIf, errorMessage, guidance

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Dict
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import InputErrorInternal exposing (ErrorState)
import Json.Decode exposing (Decoder)
import Nri.Ui
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.InputStyles.V3 as InputStyles exposing (defaultMarginTop)
import Nri.Ui.Util
import SolidColor


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute
id id_ =
    Attribute (\config -> { config | id = Just id_ })


{-| Sets whether or not the field will be highlighted as having a validation error.

If you have an error message to display, use `errorMessage` instead.

-}
errorIf : Bool -> Attribute
errorIf =
    Attribute << InputErrorInternal.setErrorIf


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute
errorMessage =
    Attribute << InputErrorInternal.setErrorMessage


{-| Customizations for the Select.
-}
type Attribute
    = Attribute (Config -> Config)


applyConfig : List Attribute -> Config
applyConfig attributes =
    List.foldl (\(Attribute update) config -> update config)
        defaultConfig
        attributes


type alias Config =
    { id : Maybe String
    , error : ErrorState
    }


defaultConfig : Config
defaultConfig =
    { id = Nothing
    , error = InputErrorInternal.init
    }


{-| A single possible choice.
-}
type alias Choice a =
    { label : String, value : a }


{-| A select dropdown. Remember to add a label!
-}
view :
    String
    ->
        { choices : List (Choice a)
        , current : Maybe a
        , valueToString : a -> String
        , defaultDisplayText : Maybe String
        }
    -> List Attribute
    -> Html a
view label config attributes =
    let
        config_ =
            applyConfig attributes

        isInError_ =
            InputErrorInternal.getIsInError config_.error

        id_ =
            Maybe.withDefault (generateId label) config_.id
    in
    Html.div
        [ css
            [ Css.position Css.relative
            , Css.marginTop (Css.px defaultMarginTop)
            ]
        ]
        [ Html.label
            [ Attributes.for id_
            , css
                [ InputStyles.label InputStyles.Standard isInError_
                , Css.top (Css.px -defaultMarginTop)
                ]
            ]
            [ Html.text label ]
        , viewSelect
            { choices = config.choices
            , current = config.current
            , id = id_
            , valueToString = config.valueToString
            , defaultDisplayText = config.defaultDisplayText
            , isInError = isInError_
            }
        ]


viewSelect :
    { choices : List (Choice a)
    , current : Maybe a
    , id : String
    , valueToString : a -> String
    , defaultDisplayText : Maybe String
    , isInError : Bool
    }
    -> Html a
viewSelect config =
    let
        valueLookup =
            config.choices
                |> List.map (\x -> ( generateId (config.valueToString x.value), x.value ))
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
    in
    config.choices
        |> List.map (viewChoice currentVal config.valueToString)
        |> (++) defaultOption
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
            , Css.cursor Css.pointer

            -- Size and spacing
            , Css.height (Css.px 45)
            , Css.width (Css.pct 100)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 30)

            -- Icons
            , selectArrowsCss
            ]
            [ onSelectHandler
            , Attributes.id config.id
            ]


viewDefaultChoice : Maybe a -> String -> Html a
viewDefaultChoice current displayText =
    Html.option
        [ Attributes.selected (current == Nothing)
        , Attributes.disabled True
        ]
        [ Html.text displayText ]


viewChoice : Maybe a -> (a -> String) -> Choice a -> Html a
viewChoice current toString choice =
    let
        isSelected =
            current
                |> Maybe.map ((==) choice.value)
                |> Maybe.withDefault False
    in
    Html.option
        [ Attributes.id (generateId (toString choice.value))
        , Attributes.value (generateId (toString choice.value))
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
