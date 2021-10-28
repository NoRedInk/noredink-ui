module Nri.Ui.TextInput.V7 exposing
    ( view, generateId
    , number, float, text, password, email, search
    , value
    , Attribute, placeholder, hiddenLabel, autofocus
    , onBlur, onReset, onEnter
    , css, custom, nriDescription, id, testId, noMargin
    , disabled, loading, errorIf, errorMessage
    , writing
    )

{-|


# Changes from V6

  - custom takes a list of attributes and appends them to the end of the previous attributes, instead of prepending a single attr.
  - change `view` API so it only takes a list of attributes (meaning the value and input type are now passed in as attributes)

@docs view, generateId


### Input types

@docs number, float, text, password, email, search


### Input content

@docs value


## Attributes

@docs Attribute, placeholder, hiddenLabel, autofocus
@docs onBlur, onReset, onEnter
@docs css, custom, nriDescription, id, testId, noMargin
@docs disabled, loading, errorIf, errorMessage
@docs writing

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (center, num, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onInput)
import Keyboard.Event exposing (KeyboardEvent)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.InputStyles.V3 as InputStyles
import Nri.Ui.Message.V3 as Message
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import Nri.Ui.Util exposing (dashify)


{-| (Internal only)
-}
type alias InputType value msg =
    { toString : value -> String
    , fromString : String -> msg
    , fieldType : String
    , inputMode : Maybe String
    , autocomplete : Maybe String
    }


{-| An input that allows text entry
-}
text : (String -> msg) -> Attribute String msg
text toMsg =
    setInputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "text"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows integer entry
-}
number : (Maybe Int -> msg) -> Attribute (Maybe Int) msg
number toMsg =
    setInputType
        { toString = Maybe.map String.fromInt >> Maybe.withDefault ""
        , fromString = String.toInt >> toMsg
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows float entry
-}
float : (Maybe Float -> msg) -> Attribute (Maybe Float) msg
float toMsg =
    setInputType
        { toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
        , fromString = String.toFloat >> toMsg
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows password entry
-}
password : (String -> msg) -> Attribute String msg
password toMsg =
    setInputType
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
email : (String -> msg) -> Attribute String msg
email toMsg =
    setInputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "text"
        , inputMode = Just "email"
        , autocomplete = Just "email"
        }


{-| An input with ["search" type](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search) specified.
-}
search : (String -> msg) -> Attribute String msg
search toMsg =
    setInputType
        { toString = identity
        , fromString = toMsg
        , fieldType = "search"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


setInputType : InputType value msg -> Attribute value msg
setInputType inputType =
    Attribute <| \config -> { config | inputType = Just inputType }


{-| -}
value : value -> Attribute value msg
value value_ =
    Attribute <| \config -> { config | currentValue = Just value_ }


{-| If not explicit placeholder is given, the input label will be used as the placeholder.
-}
placeholder : String -> Attribute value msg
placeholder text_ =
    Attribute <| \config -> { config | placeholder = Just text_ }


{-| This disables the input
-}
disabled : Attribute value msg
disabled =
    Attribute <|
        \config -> { config | disabled = True }


{-| Use this while the form the input is a part of is being submitted.
-}
loading : Attribute value msg
loading =
    Attribute <|
        \config -> { config | loading = True }


{-| Sets whether or not the field will be highlighted as having a validation error.
If you are always passing `True`, then you don't need to use this attribute.
-}
errorIf : Bool -> Attribute value msg
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
errorMessage : Maybe String -> Attribute value msg
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
hiddenLabel : Attribute value msg
hiddenLabel =
    Attribute <|
        \config -> { config | hideLabel = True }


{-| Causes the TextInput to produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute value msg
onBlur msg =
    Attribute <|
        \config -> { config | onBlur = Just msg }


{-| -}
onReset : msg -> Attribute value msg
onReset msg =
    Attribute <|
        \config -> { config | onReset = Just msg }


{-| -}
onEnter : msg -> Attribute value msg
onEnter msg =
    Attribute <|
        \config -> { config | onEnter = Just msg }


{-| Sets the `autofocus` attribute of the resulting HTML input.
-}
autofocus : Attribute value msg
autofocus =
    Attribute <|
        \config -> { config | autofocus = True }


{-| Adds CSS to the input container.

If you want to customize colors, borders, font sizes, etc, you should instead add to the TextInput API
to support what you need.

-}
css : List Css.Style -> Attribute value msg
css styles =
    Attribute <|
        \config -> { config | css = styles :: config.css }


{-| Remove default spacing from the Input.
-}
noMargin : Bool -> Attribute value msg
noMargin removeMargin =
    Attribute <| \config -> { config | noMarginTop = removeMargin }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute value msg
custom attributes =
    Attribute <|
        \config -> { config | custom = config.custom ++ attributes }


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute value msg
id id_ =
    Attribute <|
        \config -> { config | id = Just id_ }


{-| -}
nriDescription : String -> Attribute value msg
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute value msg
testId id_ =
    custom [ Extra.testId id_ ]


{-| Uses the "Writing" input style.
-}
writing : Attribute value msg
writing =
    Attribute <|
        \config -> { config | inputStyle = InputStyles.Writing }


{-| An optional customization of a TextInput.
-}
type Attribute value msg
    = Attribute (Config value msg -> Config value msg)


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config value msg =
    { inputStyle : InputStyles.Theme
    , error : ErrorState
    , disabled : Bool
    , loading : Bool
    , hideLabel : Bool
    , placeholder : Maybe String
    , onBlur : Maybe msg
    , onReset : Maybe msg
    , onEnter : Maybe msg
    , autofocus : Bool
    , noMarginTop : Bool
    , css : List (List Css.Style)
    , id : Maybe String
    , custom : List (Html.Attribute msg)
    , currentValue : Maybe value
    , inputType : Maybe (InputType value msg)
    }


type ErrorState
    = NoError
    | Error { message : Maybe String }


emptyConfig : Config value msg
emptyConfig =
    { inputStyle = InputStyles.Standard
    , error = NoError
    , disabled = False
    , loading = False
    , hideLabel = False
    , placeholder = Nothing
    , onBlur = Nothing
    , onReset = Nothing
    , onEnter = Nothing
    , autofocus = False
    , id = Nothing
    , noMarginTop = False
    , css = []
    , custom = []
    , currentValue = Nothing
    , inputType = Nothing
    }


{-| Render the TextInput as HTML.
-}
view : String -> List (Attribute value msg) -> Html msg
view label attributes =
    let
        config =
            List.foldl (\(Attribute update) -> update) emptyConfig attributes

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
            case Maybe.map .fieldType config.inputType of
                Just "number" ->
                    [ step "any" ]

                _ ->
                    []

        maybeAttr attr maybeValue =
            maybeValue
                |> Maybe.map attr
                |> Maybe.withDefault Extra.none

        stringValue =
            Maybe.map2 (\{ toString } -> toString) config.inputType config.currentValue
                |> Maybe.withDefault ""

        onEnter_ : msg -> Html.Attribute msg
        onEnter_ msg =
            (\event ->
                case event.key of
                    Just "Enter" ->
                        Just msg

                    _ ->
                        Nothing
            )
                |> Keyboard.Event.considerKeyboardEvent
                |> Events.on "keydown"
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
                   , Attributes.value stringValue
                   , Attributes.disabled disabled_
                   , maybeAttr (.fromString >> onInput) config.inputType
                   , maybeAttr Events.onBlur config.onBlur
                   , Attributes.autofocus config.autofocus
                   , maybeAttr (.fieldType >> type_) config.inputType
                   , maybeAttr (attribute "inputmode") (Maybe.andThen .inputMode config.inputType)
                   , maybeAttr (attribute "autocomplete") (Maybe.andThen .autocomplete config.inputType)
                   , maybeAttr onEnter_ config.onEnter
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
        , case ( config.onReset, stringValue, Maybe.map .fieldType config.inputType ) of
            ( Just _, "", Just "search" ) ->
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
