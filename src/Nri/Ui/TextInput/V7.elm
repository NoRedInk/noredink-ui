module Nri.Ui.TextInput.V7 exposing
    ( view, generateId
    , number, float, text, password, email, search
    , value, map
    , onInput, onBlur, onReset, onEnter
    , Attribute, placeholder, hiddenLabel, autofocus
    , css, custom, nriDescription, id, testId, noMargin
    , disabled, loading, errorIf, errorMessage
    , writing
    )

{-|


# Changes from V6

  - custom takes a list of attributes and appends them to the end of the previous attributes, instead of prepending a single attr.
  - change `view` API so it only takes a list of attributes (meaning the value and input type are now passed in as attributes)
  - split the event hander (e.g., `onInput`) from the input type (e.g., `password`)

@docs view, generateId


### Input types

@docs number, float, text, password, email, search


### Input content

@docs value, map


### Event handlers

@docs onInput, onBlur, onReset, onEnter


## Attributes

@docs Attribute, placeholder, hiddenLabel, autofocus
@docs css, custom, nriDescription, id, testId, noMargin
@docs disabled, loading, errorIf, errorMessage
@docs writing

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (center, num, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events
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


{-| An input that allows text entry
-}
text : Attribute String msg
text =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| An input that allows integer entry
-}
number : Attribute (Maybe Int) msg
number =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map String.fromInt >> Maybe.withDefault "")
            , fromString = Just String.toInt
        }
        (\config ->
            { config
                | fieldType = Just "number"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| An input that allows float entry
-}
float : Attribute (Maybe Float) msg
float =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map String.fromFloat >> Maybe.withDefault "")
            , fromString = Just String.toFloat
        }
        (\config ->
            { config
                | fieldType = Just "number"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| An input that allows password entry
-}
password : Attribute String msg
password =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
        }
        (\config ->
            { config
                | fieldType = Just "password"
                , inputMode = Nothing
                , autocomplete = Just "current-password"
            }
        )


{-| An input that is optimized for email entry

NOTE: this uses `inputmode="email"` so that mobile devices will use the email keyboard,
but not `type="email"` because that would enable browser-provided validation which is inconsistent and at odds
with our validation UI.

-}
email : Attribute String msg
email =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Just "email"
                , autocomplete = Just "email"
            }
        )


{-| An input with ["search" type](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search) specified.
-}
search : Attribute String msg
search =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
        }
        (\config ->
            { config
                | fieldType = Just "search"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| -}
value : value -> Attribute value msg
value value_ =
    Attribute { emptyEventsAndValues | currentValue = Just value_ } identity


{-| If not explicit placeholder is given, the input label will be used as the placeholder.
-}
placeholder : String -> Attribute value msg
placeholder text_ =
    Attribute emptyEventsAndValues <|
        \config -> { config | placeholder = Just text_ }


{-| This disables the input
-}
disabled : Attribute value msg
disabled =
    Attribute emptyEventsAndValues <|
        \config -> { config | disabled = True }


{-| Use this while the form the input is a part of is being submitted.
-}
loading : Attribute value msg
loading =
    Attribute emptyEventsAndValues <|
        \config -> { config | loading = True }


{-| Sets whether or not the field will be highlighted as having a validation error.
If you are always passing `True`, then you don't need to use this attribute.
-}
errorIf : Bool -> Attribute value msg
errorIf isInError =
    Attribute emptyEventsAndValues <|
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
    Attribute emptyEventsAndValues <|
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
    Attribute emptyEventsAndValues <|
        \config -> { config | hideLabel = True }


{-| -}
onInput : (value -> msg) -> Attribute value msg
onInput msg =
    Attribute { emptyEventsAndValues | onInput = Just msg } identity


{-| Causes the TextInput to produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute value msg
onBlur msg =
    Attribute { emptyEventsAndValues | onBlur = Just msg } identity


{-| -}
onReset : msg -> Attribute value msg
onReset msg =
    Attribute { emptyEventsAndValues | onReset = Just msg } identity


{-| -}
onEnter : msg -> Attribute value msg
onEnter msg =
    Attribute { emptyEventsAndValues | onEnter = Just msg } identity


{-| Sets the `autofocus` attribute of the resulting HTML input.
-}
autofocus : Attribute value msg
autofocus =
    Attribute emptyEventsAndValues <|
        \config -> { config | autofocus = True }


{-| Adds CSS to the input container.

If you want to customize colors, borders, font sizes, etc, you should instead add to the TextInput API
to support what you need.

-}
css : List Css.Style -> Attribute value msg
css styles =
    Attribute emptyEventsAndValues <|
        \config -> { config | css = styles :: config.css }


{-| Remove default spacing from the Input.
-}
noMargin : Bool -> Attribute value msg
noMargin removeMargin =
    Attribute emptyEventsAndValues <| \config -> { config | noMarginTop = removeMargin }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute value msg
custom attributes =
    Attribute emptyEventsAndValues <|
        \config -> { config | custom = config.custom ++ attributes }


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute value msg
id id_ =
    Attribute emptyEventsAndValues <|
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
    Attribute emptyEventsAndValues <|
        \config -> { config | inputStyle = InputStyles.Writing }


{-| Customizations for the TextInput.
-}
type Attribute value msg
    = Attribute (EventsAndValues value msg) (Config msg -> Config msg)


type alias EventsAndValues value msg =
    { currentValue : Maybe value
    , toString : Maybe (value -> String)
    , fromString : Maybe (String -> value)
    , onInput : Maybe (value -> msg)
    , onBlur : Maybe msg
    , onReset : Maybe msg
    , onEnter : Maybe msg
    }


emptyEventsAndValues : EventsAndValues value msg
emptyEventsAndValues =
    { currentValue = Nothing
    , toString = Nothing
    , fromString = Nothing
    , onBlur = Nothing
    , onReset = Nothing
    , onEnter = Nothing
    , onInput = Nothing
    }


{-| -}
map : (a -> b) -> (b -> String) -> (b -> msg) -> Attribute a msg -> Attribute b msg
map f toString onInput_ (Attribute eventsAndValues configF) =
    Attribute
        { currentValue = Maybe.map f eventsAndValues.currentValue
        , toString = Just toString
        , fromString = Maybe.map (\from -> from >> f) eventsAndValues.fromString
        , onBlur = eventsAndValues.onBlur
        , onReset = eventsAndValues.onReset
        , onEnter = eventsAndValues.onEnter
        , onInput = Just onInput_
        }
        configF


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { inputStyle : InputStyles.Theme
    , error : ErrorState
    , disabled : Bool
    , loading : Bool
    , hideLabel : Bool
    , placeholder : Maybe String
    , autofocus : Bool
    , noMarginTop : Bool
    , css : List (List Css.Style)
    , id : Maybe String
    , custom : List (Html.Attribute msg)
    , fieldType : Maybe String
    , inputMode : Maybe String
    , autocomplete : Maybe String
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
    , autofocus = False
    , id = Nothing
    , noMarginTop = False
    , css = []
    , custom = []
    , fieldType = Nothing
    , inputMode = Nothing
    , autocomplete = Nothing
    }


applyConfig : List (Attribute value msg) -> Config msg
applyConfig attributes =
    List.foldl (\(Attribute _ update) config -> update config)
        emptyConfig
        attributes


orExisting : (acc -> Maybe a) -> acc -> acc -> Maybe a
orExisting f new previous =
    case f previous of
        Just just ->
            Just just

        Nothing ->
            f new


applyEvents : List (Attribute value msg) -> EventsAndValues value msg
applyEvents =
    List.foldl
        (\(Attribute eventsAndValues _) existing ->
            { currentValue = orExisting .currentValue eventsAndValues existing
            , toString = orExisting .toString eventsAndValues existing
            , fromString = orExisting .fromString eventsAndValues existing
            , onBlur = orExisting .onBlur eventsAndValues existing
            , onReset = orExisting .onReset eventsAndValues existing
            , onEnter = orExisting .onEnter eventsAndValues existing
            , onInput = orExisting .onInput eventsAndValues existing
            }
        )
        emptyEventsAndValues


{-| Render the TextInput as HTML.
-}
view : String -> List (Attribute value msg) -> Html msg
view label attributes =
    let
        config : Config msg
        config =
            applyConfig attributes

        eventsAndValues : EventsAndValues value msg
        eventsAndValues =
            applyEvents attributes

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
            case config.fieldType of
                Just "number" ->
                    [ step "any" ]

                _ ->
                    []

        maybeAttr attr maybeValue =
            maybeValue
                |> Maybe.map attr
                |> Maybe.withDefault Extra.none

        stringValue =
            eventsAndValues.currentValue
                |> Maybe.map2 identity eventsAndValues.toString
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
                   , maybeAttr Events.onInput
                        (Maybe.map2 (>>)
                            eventsAndValues.fromString
                            eventsAndValues.onInput
                        )
                   , maybeAttr Events.onBlur eventsAndValues.onBlur
                   , Attributes.autofocus config.autofocus
                   , maybeAttr type_ config.fieldType
                   , maybeAttr (attribute "inputmode") config.inputMode
                   , maybeAttr (attribute "autocomplete") config.autocomplete
                   , maybeAttr onEnter_ eventsAndValues.onEnter
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
        , case ( eventsAndValues.onReset, stringValue, config.fieldType ) of
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
