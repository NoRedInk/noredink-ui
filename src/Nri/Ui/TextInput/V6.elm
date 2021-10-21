module Nri.Ui.TextInput.V6 exposing
    ( view, generateId
    , InputType, number, float, text, password, email, search
    , Attribute, placeholder, hiddenLabel, autofocus
    , onBlur, onReset
    , css, custom, nriDescription, id, testId, noMargin
    , disabled, loading, errorIf, errorMessage
    , writing
    )

{-|


# Patch changes:

  - adds `nriDescription` and `testId` helpers


# Changes from V5

  - new Attributes-style API

@docs view, generateId


### Input types

@docs InputType, number, float, text, password, email, search


## Attributes

@docs Attribute, placeholder, hiddenLabel, autofocus
@docs onBlur, onReset
@docs css, custom, nriDescription, id, testId, noMargin
@docs disabled, loading, errorIf, errorMessage
@docs writing
@docs custom

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (center, num, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onInput)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.InputStyles.V3 as InputStyles
import Nri.Ui.Message.V3 as Message
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
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


{-| An input with ["search" type](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search) specified.
-}
search : (String -> msg) -> InputType String msg
search toMsg =
    InputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "search"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An optional customization of a TextInput.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| If not explicit placeholder is given, the input label will be used as the placeholder.
-}
placeholder : String -> Attribute msg
placeholder text_ =
    Attribute <|
        \config -> { config | placeholder = Just text_ }


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
    Attribute <|
        \config ->
            { config
                | error =
                    if isInError then
                        Error { message = Nothing }

                    else
                        NoError
            }


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute msg
errorMessage maybeMessage =
    Attribute <|
        \config ->
            { config
                | error =
                    case maybeMessage of
                        Nothing ->
                            NoError

                        Just message ->
                            Error { message = Just message }
            }


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute msg
hiddenLabel =
    Attribute <|
        \config -> { config | hideLabel = True }


{-| Causes the TextInput to produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute msg
onBlur msg =
    Attribute <|
        \config -> { config | onBlur = Just msg }


{-| -}
onReset : msg -> Attribute msg
onReset msg =
    Attribute <|
        \config -> { config | onReset = Just msg }


{-| Sets the `autofocus` attribute of the resulting HTML input.
-}
autofocus : Attribute msg
autofocus =
    Attribute <|
        \config -> { config | autofocus = True }


{-| Adds CSS to the input container.

If you want to customize colors, borders, font sizes, etc, you should instead add to the TextInput API
to support what you need.

-}
css : List Css.Style -> Attribute msg
css styles =
    Attribute <|
        \config -> { config | css = styles :: config.css }


{-| Remove default spacing from the Input.
-}
noMargin : Bool -> Attribute msg
noMargin removeMargin =
    Attribute <| \config -> { config | noMarginTop = removeMargin }


{-| Add any attribute to the input. Don't use this helper for adding css!

TODO: in V7, change this helper's type to `List (Html.Attribute msg) -> Attribute msg`,
to be more consistent with other helpers.

Also, we should probably change the implemenation from `attr :: config.custom` to
`config.custom ++ attributes` for a more intuitive and consistent API.

-}
custom : Html.Attribute msg -> Attribute msg
custom attr =
    Attribute <|
        \config -> { config | custom = attr :: config.custom }


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute msg
id id_ =
    Attribute <|
        \config -> { config | id = Just id_ }


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom (Extra.nriDescription description)


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom (Extra.testId id_)


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
    , onReset : Maybe msg
    , autofocus : Bool
    , noMarginTop : Bool
    , css : List (List Css.Style)
    , id : Maybe String
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
    , onReset = Nothing
    , autofocus = False
    , id = Nothing
    , noMarginTop = False
    , css = []
    , custom = []
    }


{-| Render the TextInput as HTML.
The input's label, InputType, and current value are all required. Other attributes are all optional.
-}
view : String -> InputType value msg -> List (Attribute msg) -> value -> Html msg
view label inputType attributes currentValue =
    let
        config =
            List.foldl (\(Attribute update) -> update) emptyConfig attributes
    in
    view_ label inputType config currentValue


{-| Uses the "Writing" input style. See [`Nri.Ui.InputStyles.V2.Theme`](Nri-Ui-InputStyles-V2#Theme).
-}
writing : Attribute msg
writing =
    Attribute <|
        \config -> { config | inputStyle = InputStyles.Writing }


view_ : String -> InputType value msg -> Config msg -> value -> Html msg
view_ label (InputType inputType) config currentValue =
    let
        idValue =
            case config.id of
                Just id_ ->
                    id_

                Nothing ->
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

        stringValue =
            inputType.toString currentValue
    in
    div
        ([ Attributes.css
            [ position relative
            , Css.opacity opacity
            ]
         ]
            ++ List.map Attributes.css (List.reverse config.css)
        )
        [ input
            (maybeStep
                ++ List.reverse config.custom
                ++ [ Attributes.id idValue
                   , Attributes.css
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
                        , Css.pseudoElement "-webkit-search-cancel-button"
                            [ Css.display Css.none ]
                        , if config.noMarginTop then
                            Css.important (Css.marginTop Css.zero)

                          else
                            Css.batch []
                        ]
                   , Attributes.placeholder placeholder_
                   , value stringValue
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
             , Attributes.css
                [ InputStyles.label config.inputStyle isInError
                , if config.noMarginTop then
                    Css.top (Css.px -9)

                  else
                    Css.batch []
                ]
             ]
                ++ extraStyles
            )
            [ Html.text label ]
        , case ( config.onReset, stringValue, inputType.fieldType ) of
            ( Just _, "", "search" ) ->
                UiIcon.search
                    |> Svg.withWidth (Css.px 20)
                    |> Svg.withHeight (Css.px 20)
                    |> Svg.withColor Colors.gray75
                    |> Svg.withCss
                        [ Css.position Css.absolute
                        , Css.right (Css.px 10)
                        , Css.top (Css.px 22)
                        ]
                    |> Svg.toHtml

            ( Just resetAction, _, _ ) ->
                ClickableSvg.button ("Reset " ++ label)
                    UiIcon.x
                    [ ClickableSvg.onClick resetAction
                    , ClickableSvg.exactWidth 14
                    , ClickableSvg.exactHeight 14
                    , ClickableSvg.css
                        [ Css.position Css.absolute
                        , Css.right (Css.px 10)
                        , Css.top (Css.px 25)
                        ]
                    ]

            ( Nothing, _, _ ) ->
                Html.text ""
        , case errorMessage_ of
            Just m ->
                Message.view
                    [ Message.tiny
                    , Message.error
                    , Message.plaintext m
                    , Message.alertRole
                    ]

            Nothing ->
                Html.text ""
        ]


{-| Gives you the DOM element id that will be used by a `TextInput.view` with the given label.
This is for use when you need the DOM element id for use in javascript (such as trigger an event to focus a particular text input)
-}
generateId : String -> String
generateId labelText =
    "Nri-Ui-TextInput-" ++ dashify labelText
