module Nri.Ui.TextInput.V6 exposing
    ( view, generateId
    , InputType, number, float, text, password, email
    , Attribute, placeholder, hiddenLabel, onBlur, autofocus, custom
    , disabled, loading, errorIf, errorMessage
    , writing
    )

{-|


# Changes from V5

  - new Attributes-style API

@docs view, generateId


### Input types

@docs InputType, number, float, text, password, email


## Attributes

@docs Attribute, placeholder, hiddenLabel, onBlur, autofocus, custom
@docs disabled, loading, errorIf, errorMessage
@docs writing

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (center, num, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onInput)
import Nri.Ui.Alert.V4 as Alert
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.InputStyles.V2 as InputStyles
import Nri.Ui.Util exposing (dashify)


{-| -}
type InputType value msg
    = InputType
        { toString : value -> String
        , fromString : String -> msg
        , fieldType : String
        , inputMode : Maybe String
        , autocomplete : Maybe String
        }


{-| An input that allows text entry
-}
text : (String -> msg) -> InputType String msg
text toMsg =
    InputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "text"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows integer entry
-}
number : (Maybe Int -> msg) -> InputType (Maybe Int) msg
number toMsg =
    InputType
        { toString = Maybe.map String.fromInt >> Maybe.withDefault ""
        , fromString = String.toInt >> toMsg
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows float entry
-}
float : (Maybe Float -> msg) -> InputType (Maybe Float) msg
float toMsg =
    InputType
        { toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
        , fromString = String.toFloat >> toMsg
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows password entry
-}
password : (String -> msg) -> InputType String msg
password toMsg =
    InputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "password"
        , inputMode = Nothing
        , autocomplete = Just "current-password"
        }


{-| An input that is optimized for email entry

NOTE: this uses `inputmode="email"` so that mobile devices will use the email keyboard,
but not `type="email"` because that would enable browser-provided validation which is inconsistent and at odds
with our validation UI.

-}
email : (String -> msg) -> InputType String msg
email toMsg =
    InputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "text"
        , inputMode = Just "email"
        , autocomplete = Just "email"
        }


{-| An optional customization of a TextInput.
-}
type Attribute msg
    = InputStyleAttribute InputStyles.Theme
    | ErrorAttribute ErrorState
    | HideLabelAttribute Bool
    | PlaceholderAttribute String
    | OnBlurAttribute msg
    | AutofocusAttribute Bool
    | Attribute (Config msg -> Config msg)
    | CustomAttribute (Html.Attribute msg)


{-| If not explicit placeholder is given, the input label will be used as the placeholder.
-}
placeholder : String -> Attribute msg
placeholder text_ =
    PlaceholderAttribute text_


{-| This disables the input
-}
disabled : Attribute msg
disabled =
    Attribute <|
        \config -> { config | disabled = True }


{-| Use this while the form the input is a part of is being submitted.
-}
loading : Attribute msg
loading =
    Attribute <|
        \config -> { config | loading = True }


{-| Sets whether or not the field will be highlighted as having a validation error.
If you are always passing `True`, then you don't need to use this attribute.
-}
errorIf : Bool -> Attribute msg
errorIf isInError =
    ErrorAttribute <|
        if isInError then
            Error { message = Nothing }

        else
            NoError


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute msg
errorMessage maybeMessage =
    case maybeMessage of
        Nothing ->
            ErrorAttribute NoError

        Just message ->
            ErrorAttribute <| Error { message = Just message }


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute msg
hiddenLabel =
    HideLabelAttribute True


{-| Causes the TextInput to produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute msg
onBlur msg =
    OnBlurAttribute msg


{-| Sets the `autofocus` attribute of the resulting HTML input.
-}
autofocus : Attribute msg
autofocus =
    AutofocusAttribute True


{-| Add any attribute to the input.

NOTE: This is meant for short-term workarounds, and if you use this,
consider adding to the TextInput API to support what you need.

-}
custom : Html.Attribute msg -> Attribute msg
custom attr =
    CustomAttribute attr


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { inputStyle : InputStyles.Theme
    , error : ErrorState
    , disabled : Bool
    , loading : Bool
    , hideLabel : Bool
    , placeholder : Maybe String
    , onBlur : Maybe msg
    , autofocus : Bool
    , custom : List (Html.Attribute msg)
    }


type ErrorState
    = NoError
    | Error { message : Maybe String }


emptyConfig : Config msg
emptyConfig =
    { inputStyle = InputStyles.Standard
    , error = NoError
    , disabled = False
    , loading = False
    , hideLabel = False
    , placeholder = Nothing
    , onBlur = Nothing
    , autofocus = False
    , custom = []
    }


updateConfig : Attribute msg -> Config msg -> Config msg
updateConfig attribute config =
    case attribute of
        InputStyleAttribute theme ->
            { config | inputStyle = theme }

        ErrorAttribute errorState ->
            { config | error = errorState }

        HideLabelAttribute hideLabel ->
            { config | hideLabel = hideLabel }

        PlaceholderAttribute text_ ->
            { config | placeholder = Just text_ }

        OnBlurAttribute msg ->
            { config | onBlur = Just msg }

        AutofocusAttribute autofocus_ ->
            { config | autofocus = autofocus_ }

        Attribute updateConfig_ ->
            updateConfig_ config

        CustomAttribute attr ->
            { config | custom = attr :: config.custom }


{-| Render the TextInput as HTML.
The input's label, InputType, and current value are all required. Other attributes are all optional.
-}
view : String -> InputType value msg -> List (Attribute msg) -> value -> Html msg
view label inputType attributes currentValue =
    let
        config =
            List.foldl updateConfig emptyConfig attributes
    in
    view_ label inputType config currentValue


{-| Uses the "Writing" input style. See [`Nri.Ui.InputStyles.V2.Theme`](Nri-Ui-InputStyles-V2#Theme).
-}
writing : Attribute msg
writing =
    InputStyleAttribute InputStyles.Writing


view_ : String -> InputType value msg -> Config msg -> value -> Html msg
view_ label (InputType inputType) config currentValue =
    let
        idValue =
            generateId label

        placeholder_ =
            config.placeholder
                |> Maybe.withDefault label

        ( isInError, errorMessage_ ) =
            case config.error of
                NoError ->
                    ( False, Nothing )

                Error { message } ->
                    ( True, message )

        ( opacity, disabled_ ) =
            case ( config.disabled, config.loading ) of
                ( False, False ) ->
                    ( num 1, False )

                ( False, True ) ->
                    ( num 0.5, True )

                ( True, _ ) ->
                    ( num 0.4, True )

        maybeStep =
            if inputType.fieldType == "number" then
                [ step "any" ]

            else
                []

        maybeAttr attr maybeValue =
            maybeValue
                |> Maybe.map attr
                |> Maybe.withDefault Extra.none
    in
    div
        [ Attributes.css
            [ position relative
            , Css.opacity opacity
            ]
        ]
        [ input
            (maybeStep
                ++ List.reverse config.custom
                ++ [ Attributes.id idValue
                   , css
                        [ InputStyles.input config.inputStyle isInError
                        , if config.inputStyle == InputStyles.Writing then
                            Css.Global.withClass "override-sass-styles"
                                [ textAlign center
                                , Css.height Css.auto
                                ]

                          else
                            Css.Global.withClass "override-sass-styles"
                                [ Css.height (px 45)
                                ]
                        ]
                   , Attributes.placeholder placeholder_
                   , value (inputType.toString currentValue)
                   , Attributes.disabled disabled_
                   , onInput inputType.fromString
                   , maybeAttr Events.onBlur config.onBlur
                   , Attributes.autofocus config.autofocus
                   , type_ inputType.fieldType
                   , maybeAttr (attribute "inputmode") inputType.inputMode
                   , maybeAttr (attribute "autocomplete") inputType.autocomplete
                   , class "override-sass-styles"
                   , Attributes.attribute "aria-invalid" <|
                        if isInError then
                            "true"

                        else
                            "false"
                   ]
            )
            []
        , let
            extraStyles =
                if config.hideLabel then
                    Accessibility.invisible

                else
                    []
          in
          Html.label
            ([ for idValue
             , css [ InputStyles.label config.inputStyle isInError ]
             ]
                ++ extraStyles
            )
            [ Html.text label ]
        , case errorMessage_ of
            Just m ->
                Alert.error m

            Nothing ->
                Html.text ""
        ]


{-| Gives you the DOM element id that will be used by a `TextInput.view` with the given label.
This is for use when you need the DOM element id for use in javascript (such as trigger an event to focus a particular text input)
-}
generateId : String -> String
generateId labelText =
    "Nri-Ui-TextInput-" ++ dashify labelText
