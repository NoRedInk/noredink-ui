module Nri.Ui.TextInput.V8 exposing
    ( view, generateId
    , number, float, text, newPassword, currentPassword, email, search, addressLevel2, addressLine1, countryName, familyName, givenName, username, organization, organizationTitle, postalCode, sex, tel, date, datetime
    , readOnlyText
    , value, map
    , onFocus, onBlur, onEnter
    , Attribute, placeholder, autofocus
    , hiddenLabel, visibleLabel
    , css, custom, nriDescription, id, testId, noMargin
    , disabled, loading, errorIf, errorMessage, guidance, guidanceHtml
    , writing
    )

{-|


# Changes from V7

  - add `visibilityToggleId` to password inputs to enable blurring and refocusing on the toggle button, ensuring that screen readers announce the label change.

@docs view, generateId


### Input types

@docs number, float, text, newPassword, currentPassword, email, search, addressLevel2, addressLine1, countryName, familyName, givenName, username, organization, organizationTitle, postalCode, sex, tel, date, datetime
@docs readOnlyText


### Input content

@docs value, map


### Event handlers

@docs onFocus, onBlur, onEnter


### Attributes

@docs Attribute, placeholder, autofocus
@docs hiddenLabel, visibleLabel
@docs css, custom, nriDescription, id, testId, noMargin
@docs disabled, loading, errorIf, errorMessage, guidance, guidanceHtml
@docs writing

-}

import Css exposing (center, num, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import InputLabelInternal
import Iso8601
import Keyboard.Event
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.InputStyles.V4 as InputStyles exposing (defaultMarginTop)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V2 as UiIcon
import Time


{-| An input that allows text entry
-}
text : (String -> msg) -> Attribute String msg
text onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| A read-only input for text values
-}
readOnlyText : Attribute String msg
readOnlyText =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Nothing
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Nothing
                , readOnly = True
                , inputCss = [ Css.backgroundColor Colors.frost ]
            }
        )


{-| An input that allows integer entry
-}
number : (Maybe Int -> msg) -> Attribute (Maybe Int) msg
number onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map String.fromInt >> Maybe.withDefault "")
            , fromString = Just String.toInt
            , onInput = Just (String.toInt >> onInput_)
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
float : (Maybe Float -> msg) -> Attribute (Maybe Float) msg
float onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map String.fromFloat >> Maybe.withDefault "")
            , fromString = Just String.toFloat
            , onInput = Just (String.toFloat >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "number"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| An input that allows password entry with autocomplete value "new-password"

If the user types at least one character into the input box, a
floating control "Show password" will appear. When clicked, the
input type will change from "password" to "text", in order
to enable the user to check what they've typed.

-}
newPassword :
    { onInput : String -> msg
    , showPassword : Bool
    , setShowPassword : Bool -> msg
    , visibilityToggleId : String
    }
    -> Attribute String msg
newPassword =
    password "new-password"


{-| An input that allows password entry with autocomplete value "current-password"

If the user types at least one character into the input box, a
floating control "Show password" will appear. When clicked, the
input type will change from "password" to "text", in order
to enable the user to check what they've typed.

-}
currentPassword :
    { onInput : String -> msg
    , showPassword : Bool
    , setShowPassword : Bool -> msg
    , visibilityToggleId : String
    }
    -> Attribute String msg
currentPassword =
    password "current-password"


password :
    String
    ->
        { onInput : String -> msg
        , showPassword : Bool
        , setShowPassword : Bool -> msg
        , visibilityToggleId : String
        }
    -> Attribute String msg
password autocomplete settings =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just settings.onInput
            , passwordFloatingContent =
                Just <|
                    viewPasswordFloatingContent
                        (if settings.showPassword then
                            "Hide password"

                         else
                            "Show password"
                        )
                        (settings.setShowPassword (not settings.showPassword))
        }
        (\config ->
            { config
                | fieldType =
                    Just <|
                        if settings.showPassword then
                            "text"

                        else
                            "password"
                , inputMode = Nothing
                , autocomplete = Just autocomplete
                , inputCss = Css.paddingRight (Css.px 135) :: config.inputCss
                , isPassword = True
                , passwordVisibilityToggleId = Just settings.visibilityToggleId
            }
        )


{-| An input that is optimized for email entry

NOTE: this uses `inputmode="email"` so that mobile devices will use the email keyboard,
but not `type="email"` because that would enable browser-provided validation which is inconsistent and at odds
with our validation UI.

-}
email : (String -> msg) -> Attribute String msg
email onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just onInput_
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
search : (String -> msg) -> Attribute String msg
search onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , floatingContent = Just viewSearchFloatingContent
            , onInput = Just onInput_
        }
        (\config ->
            { config
                | fieldType = Just "search"
                , inputMode = Nothing
                , autocomplete = Nothing
                , inputCss = Css.paddingRight (Css.px 30) :: config.inputCss
            }
        )


{-| An input that allows given-name entry
-}
givenName : (String -> msg) -> Attribute String msg
givenName onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "given-name"
            }
        )


{-| An input that allows family-name entry
-}
familyName : (String -> msg) -> Attribute String msg
familyName onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "family-name"
            }
        )


{-| An input that allows username entry
-}
username : (String -> msg) -> Attribute String msg
username onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "username"
            }
        )


{-| An input that allows organization entry
-}
organization : (String -> msg) -> Attribute String msg
organization onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "organization"
            }
        )


{-| An input that allows organization-title entry
-}
organizationTitle : (String -> msg) -> Attribute String msg
organizationTitle onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "organization-title"
            }
        )


{-| An input that allows address-line1 entry
-}
addressLine1 : (String -> msg) -> Attribute String msg
addressLine1 onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "address-line1"
            }
        )


{-| An input that allows address-level2 entry
-}
addressLevel2 : (String -> msg) -> Attribute String msg
addressLevel2 onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "address-level2"
            }
        )


{-| An input that allows country-name entry
-}
countryName : (String -> msg) -> Attribute String msg
countryName onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "country-name"
            }
        )


{-| An input that allows postal-code entry
-}
postalCode : (String -> msg) -> Attribute String msg
postalCode onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "postal-code"
            }
        )


{-| An input that allows tel entry
-}
tel : (String -> msg) -> Attribute String msg
tel onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "tel"
                , inputMode = Just "tel"
                , autocomplete = Just "tel"
            }
        )


{-| An input that allows sex entry
-}
sex : (String -> msg) -> Attribute String msg
sex onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just identity
            , fromString = Just identity
            , onInput = Just (identity >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "text"
                , inputMode = Nothing
                , autocomplete = Just "sex"
            }
        )


{-| An input that allows date entry. The date is represented as a `Maybe Time.Posix`
value, where `Nothing` represents an empty date field. The date is formatted as
an ISO8601 date string, with the time portion removed.

Format for a date input field is `YYYY-MM-DD`. Iso8601.fromTime generates a string in
the format `YYYY-MM-DDThh:mmZ` so we slice to only use the first 10 characters.

-}
date : (Maybe Time.Posix -> msg) -> Attribute (Maybe Time.Posix) msg
date onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map (Iso8601.fromTime >> String.slice 0 10) >> Maybe.withDefault "")
            , fromString = Just (Iso8601.toTime >> Result.toMaybe)
            , onInput = Just (Iso8601.toTime >> Result.toMaybe >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "date"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| An input that allows datetime entry. The datetime is represented as a `Maybe Time.Posix`
value, where `Nothing` represents an empty datetime field. The datetime is formatted as
an ISO8601 datetime string, with the time zone removed.

Format for a datetime input field value should be `YYYY-MM-DDThh:mm`.
From [MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/datetime-local#value):

> There are several methods provided by JavaScript's Date that can be used to convert numeric
> date information into a properly-formatted string. For example, the Date.toISOString() method
> returns the date/time in UTC with the suffix "Z" denoting that timezone; removing the "Z" would
> provide a value in the format expected by a datetime-local input.

Iso8601.fromTime generates the same string as Date.toISOString, so we need to drop the "Z" suffix.

-}
datetime : (Maybe Time.Posix -> msg) -> Attribute (Maybe Time.Posix) msg
datetime onInput_ =
    Attribute
        { emptyEventsAndValues
            | toString = Just (Maybe.map (Iso8601.fromTime >> String.dropRight 1) >> Maybe.withDefault "")
            , fromString = Just (Iso8601.toTime >> Result.toMaybe)
            , onInput = Just (Iso8601.toTime >> Result.toMaybe >> onInput_)
        }
        (\config ->
            { config
                | fieldType = Just "datetime-local"
                , inputMode = Nothing
                , autocomplete = Nothing
            }
        )


{-| -}
value : value -> Attribute value msg
value value_ =
    Attribute { emptyEventsAndValues | currentValue = Just value_ } identity


{-| -}
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
-}
errorIf : Bool -> Attribute value msg
errorIf =
    Attribute emptyEventsAndValues << InputErrorAndGuidanceInternal.setErrorIf


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute value msg
errorMessage =
    Attribute emptyEventsAndValues << InputErrorAndGuidanceInternal.setErrorMessage


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute value msg
guidance =
    Attribute emptyEventsAndValues << InputErrorAndGuidanceInternal.setGuidance


{-| A guidance message (HTML) shows below the input, unless an error message is showing instead.
-}
guidanceHtml : List (Html msg) -> Attribute value msg
guidanceHtml =
    Attribute emptyEventsAndValues << InputErrorAndGuidanceInternal.setGuidanceHtml


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute value msg
hiddenLabel =
    Attribute emptyEventsAndValues <|
        \config -> { config | hideLabel = True }


{-| Default behavior.
-}
visibleLabel : Attribute value msg
visibleLabel =
    Attribute emptyEventsAndValues <|
        \config -> { config | hideLabel = False }


{-| Causes the TextInput to produce the given `msg` when the field is focused.
-}
onFocus : msg -> Attribute value msg
onFocus msg =
    Attribute { emptyEventsAndValues | onFocus = Just msg } identity


{-| Causes the TextInput to produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute value msg
onBlur msg =
    Attribute { emptyEventsAndValues | onBlur = Just msg } identity


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


{-| Adds CSS to the element containing the input.

If you want to customize colors, borders, font sizes, etc, you should instead add to the TextInput API
to support what you need.

-}
css : List Css.Style -> Attribute value msg
css styles =
    Attribute emptyEventsAndValues <|
        \config -> { config | containerCss = config.containerCss ++ styles }


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
custom : List (Html.Attribute Never) -> Attribute value msg
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
    , onInput : Maybe (String -> msg)
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    , onEnter : Maybe msg
    , floatingContent : Maybe (FloatingContentConfig msg -> Html msg)
    , passwordFloatingContent : Maybe (PasswordFloatingContentConfig msg -> Html msg)
    }


emptyEventsAndValues : EventsAndValues value msg
emptyEventsAndValues =
    { currentValue = Nothing
    , toString = Nothing
    , fromString = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onEnter = Nothing
    , onInput = Nothing
    , floatingContent = Nothing
    , passwordFloatingContent = Nothing
    }


{-| -}
map : (a -> b) -> (b -> String) -> Attribute a msg -> Attribute b msg
map f toString (Attribute eventsAndValues configF) =
    Attribute
        { currentValue = Maybe.map f eventsAndValues.currentValue
        , toString = Just toString
        , fromString = Maybe.map (\from -> from >> f) eventsAndValues.fromString
        , onFocus = eventsAndValues.onFocus
        , onBlur = eventsAndValues.onBlur
        , onEnter = eventsAndValues.onEnter
        , onInput = eventsAndValues.onInput
        , floatingContent = eventsAndValues.floatingContent
        , passwordFloatingContent = eventsAndValues.passwordFloatingContent
        }
        configF


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { inputStyle : InputStyles.Theme
    , inputCss : List Css.Style
    , guidance : Guidance msg
    , error : ErrorState
    , readOnly : Bool
    , disabled : Bool
    , loading : Bool
    , hideLabel : Bool
    , placeholder : Maybe String
    , autofocus : Bool
    , noMarginTop : Bool
    , containerCss : List Css.Style
    , id : Maybe String
    , custom : List (Html.Attribute Never)
    , fieldType : Maybe String
    , inputMode : Maybe String
    , autocomplete : Maybe String
    , isPassword : Bool
    , passwordVisibilityToggleId : Maybe String
    }


emptyConfig : Config msg
emptyConfig =
    { inputStyle = InputStyles.Standard
    , inputCss = []
    , guidance = InputErrorAndGuidanceInternal.noGuidance
    , error = InputErrorAndGuidanceInternal.noError
    , readOnly = False
    , disabled = False
    , loading = False
    , hideLabel = False
    , placeholder = Nothing
    , autofocus = False
    , id = Nothing
    , noMarginTop = False
    , containerCss = []
    , custom = []
    , fieldType = Nothing
    , inputMode = Nothing
    , autocomplete = Nothing
    , isPassword = False
    , passwordVisibilityToggleId = Nothing
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
            , onFocus = orExisting .onFocus eventsAndValues existing
            , onBlur = orExisting .onBlur eventsAndValues existing
            , floatingContent = orExisting .floatingContent eventsAndValues existing
            , passwordFloatingContent = orExisting .passwordFloatingContent eventsAndValues existing
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

        isInError =
            InputErrorAndGuidanceInternal.getIsInError config.error

        ( opacity, disabled_ ) =
            case ( config.disabled, config.loading ) of
                ( False, False ) ->
                    ( num 1, False )

                ( False, True ) ->
                    ( num 0.5, True )

                ( True, _ ) ->
                    ( num 1, True )

        maybeStep =
            case config.fieldType of
                Just "number" ->
                    [ step "any" ]

                _ ->
                    []

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
        [ Attributes.css
            (position relative
                :: Css.opacity opacity
                :: config.containerCss
            )
        ]
        ([ input
            (maybeStep
                ++ List.map (Attributes.map never) (List.reverse config.custom)
                ++ [ Attributes.id idValue
                   , InputErrorAndGuidanceInternal.describedBy idValue config
                   , Attributes.css
                        [ InputStyles.input config.inputStyle
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
                        , Css.batch config.inputCss |> Css.important
                        , Css.batch
                            (if config.disabled then
                                [ Css.backgroundColor Colors.gray85
                                , Css.borderColor Colors.gray85 |> Css.important
                                , Css.property "box-shadow" "none" |> Css.important
                                ]

                             else
                                []
                            )
                        ]
                   , Extra.maybe Attributes.placeholder config.placeholder
                   , Attributes.value stringValue
                   , Attributes.disabled disabled_
                   , Attributes.readonly config.readOnly
                   , Extra.maybe Events.onInput eventsAndValues.onInput
                   , Extra.maybe Events.onFocus eventsAndValues.onFocus
                   , Extra.maybe Events.onBlur eventsAndValues.onBlur
                   , Attributes.autofocus config.autofocus
                   , Extra.maybe type_ config.fieldType
                   , Extra.maybe (attribute "inputmode") config.inputMode
                   , Extra.maybe (attribute "autocomplete") config.autocomplete
                   , Extra.maybe onEnter_ eventsAndValues.onEnter
                   , class "nri-ui-textinput override-sass-styles custom-focus-ring"
                   , classList
                        [ ( InputStyles.inputClass, True )
                        , ( InputStyles.errorClass, isInError )
                        ]
                   , Attributes.attribute "aria-invalid" <|
                        if isInError then
                            "true"

                        else
                            "false"
                   ]
            )
            []
         , InputLabelInternal.view
            { for = idValue
            , label = label
            , theme = config.inputStyle
            }
            config
         , if config.isPassword then
            Maybe.map2
                (\view_ onStringInput_ ->
                    view_
                        { label = label
                        , stringValue = stringValue
                        , onInput = onStringInput_
                        , noMarginTop = config.noMarginTop
                        , visibilityToggleId = Maybe.withDefault "" config.passwordVisibilityToggleId
                        }
                )
                eventsAndValues.passwordFloatingContent
                eventsAndValues.onInput
                |> Maybe.withDefault (Html.text "")

           else
            Maybe.map2
                (\view_ onStringInput_ ->
                    view_
                        { label = label
                        , stringValue = stringValue
                        , onInput = onStringInput_
                        , noMarginTop = config.noMarginTop
                        }
                )
                eventsAndValues.floatingContent
                eventsAndValues.onInput
                |> Maybe.withDefault (Html.text "")
         ]
            ++ InputErrorAndGuidanceInternal.view idValue InputErrorAndGuidanceInternal.smallMargin config
        )


{-| Gives you the default DOM element id that will be used by a `TextInput.view` with the given label.
This is for use when you need the DOM element id for use in javascript (such as trigger an event to focus a particular text input)
-}
generateId : String -> String
generateId =
    Extra.safeIdWithPrefix "Nri-Ui-TextInput"


type alias FloatingContentConfig msg =
    { label : String
    , stringValue : String
    , onInput : String -> msg
    , noMarginTop : Bool
    }


type alias PasswordFloatingContentConfig msg =
    { label : String
    , stringValue : String
    , onInput : String -> msg
    , noMarginTop : Bool
    , visibilityToggleId : String
    }


viewSearchFloatingContent : FloatingContentConfig msg -> Html msg
viewSearchFloatingContent config =
    if config.stringValue == "" then
        searchIcon config

    else
        resetButton config


searchIcon : { settings | noMarginTop : Bool } -> Html msg
searchIcon config =
    UiIcon.search
        |> Svg.withWidth (Css.px 17)
        |> Svg.withHeight (Css.px 17)
        |> Svg.withColor Colors.navy
        |> Svg.withCss
            [ Css.position Css.absolute
            , Css.right (Css.px 15)
            , if config.noMarginTop then
                Css.top (Css.px (23 - defaultMarginTop))

              else
                Css.top (Css.px 23)
            ]
        |> Svg.toHtml


resetButton : FloatingContentConfig msg -> Html msg
resetButton config =
    ClickableSvg.button ("Reset " ++ config.label)
        UiIcon.x
        [ ClickableSvg.onClick (config.onInput "")
        , ClickableSvg.exactWidth 19
        , ClickableSvg.exactHeight 19
        , ClickableSvg.css
            [ Css.position Css.absolute
            , Css.right (Css.px 15)
            , if config.noMarginTop then
                Css.top (Css.px (23 - defaultMarginTop))

              else
                Css.top (Css.px 23)
            ]
        , ClickableSvg.custom [ Attributes.type_ "button" ]
        ]


viewPasswordFloatingContent : String -> msg -> PasswordFloatingContentConfig msg -> Html msg
viewPasswordFloatingContent label toggle config =
    if config.stringValue == "" then
        Html.text ""

    else
        -- TODO: consider using a "toggle" clickable text button,
        -- a checkbox styled to look like a clickable text, or
        -- adding additional aria attributes connecting this clickable
        -- text to the password field.
        ClickableText.button label
            [ ClickableText.onClick toggle
            , ClickableText.small
            , ClickableText.css
                [ Css.position Css.absolute
                , Css.right (Css.px 15)
                , if config.noMarginTop then
                    Css.top (Css.px (22 - defaultMarginTop))

                  else
                    Css.top (Css.px 22)
                , Css.fontSize (Css.px 13)
                ]
            , ClickableText.custom [ Attributes.type_ "button" ]
            , ClickableText.id config.visibilityToggleId
            ]
