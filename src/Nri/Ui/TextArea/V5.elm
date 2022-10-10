module Nri.Ui.TextArea.V5 exposing
    ( view, Model, generateId
    , Attribute
    , value
    , onInput, onBlur
    , hiddenLabel, visibleLabel
    , standard, writing
    , Height(..), HeightBehavior(..)
    , autofocus
    )

{-|


## Changelog


### Changes from V4

  - Removed contentCreation view styles
  - Changed to a list-based API


## The next version of TextArea should:

  - switch to a list-based API
  - add support for `guidance`
  - add support for `errorMessage`
  - update the disabled styles


## Upgrading to V4

  - Adds field for onBlur


## The Nri styleguide-specified textarea with overlapping label


## Creating New Versions

When upgrading this module, we need to make sure to also include a new
custom element, or else autosizing will break! This means doing the following:

1.  Creating a new module in `lib/TextArea`
2.  Requiring that module in `lib/index.js`


## API

@docs view, Model, generateId
@docs Attribute
@docs value


### Event handlers

@docs onInput, onBlur


### Visual behavior

@docs hiddenLabel, visibleLabel
@docs standard, writing
@docs Height, HeightBehavior


### Other

@docs autofocus

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style as Style
import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.InputStyles.V4 as InputStyles exposing (Theme(..))
import Nri.Ui.Util exposing (dashify, removePunctuation)


{-| -}
type alias Model =
    { isInError : Bool
    , height : HeightBehavior
    , placeholder : String
    , label : String
    }


{-| Control whether to auto-expand the height.
-}
type HeightBehavior
    = Fixed
    | AutoResize Height


{-| For specifying the actual height.
-}
type Height
    = DefaultHeight
    | SingleLine


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { theme : Theme
    , hideLabel : Bool
    , value : String
    , autofocus : Bool
    , onInput : Maybe (String -> msg)
    , onBlur : Maybe msg
    }


defaultConfig : Config msg
defaultConfig =
    { theme = Standard
    , hideLabel = False
    , value = ""
    , autofocus = False
    , onInput = Nothing
    , onBlur = Nothing
    }


applyConfig : List (Attribute msg) -> Config msg
applyConfig =
    List.foldl (\(Attribute update) config -> update config) defaultConfig


{-| Customizations for the TextArea.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| -}
value : String -> Attribute msg
value value_ =
    Attribute (\soFar -> { soFar | value = value_ })


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute msg
hiddenLabel =
    Attribute (\soFar -> { soFar | hideLabel = True })


{-| Default behavior.
-}
visibleLabel : Attribute msg
visibleLabel =
    Attribute (\soFar -> { soFar | hideLabel = False })


{-| Produce the given `msg` when the field is focused.
-}
onInput : (String -> msg) -> Attribute msg
onInput msg =
    Attribute (\soFar -> { soFar | onInput = Just msg })


{-| Produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute msg
onBlur msg =
    Attribute (\soFar -> { soFar | onBlur = Just msg })


{-| Sets the `autofocus` attribute of the textarea to true.
-}
autofocus : Attribute msg
autofocus =
    Attribute (\soFar -> { soFar | autofocus = True })


{-| Use the Standard theme for the TextArea. This is the default.
-}
standard : Attribute msg
standard =
    Attribute (\soFar -> { soFar | theme = InputStyles.Standard })


{-| Use the Writing theme for the TextArea.
-}
writing : Attribute msg
writing =
    Attribute (\soFar -> { soFar | theme = InputStyles.Writing })


{-| -}
view : Model -> List (Attribute msg) -> Html msg
view model attributes =
    view_ (applyConfig attributes) model


{-| -}
view_ : Config msg -> Model -> Html msg
view_ config model =
    let
        autoresizeAttrs =
            case model.height of
                AutoResize _ ->
                    [ Attributes.attribute "data-autoresize" "" ]

                Fixed ->
                    []

        heightForStyle =
            case config.theme of
                Standard ->
                    InputStyles.textAreaHeight

                UserGenerated ->
                    InputStyles.textAreaHeight

                Writing ->
                    InputStyles.writingMinHeight
    in
    Html.styled (Html.node "nri-textarea-v5")
        [ Css.display Css.block, Css.position Css.relative ]
        autoresizeAttrs
        [ Html.styled Html.textarea
            [ InputStyles.input config.theme
            , Css.boxSizing Css.borderBox
            , case model.height of
                AutoResize minimumHeight ->
                    Css.minHeight (calculateMinHeight config.theme minimumHeight)

                Fixed ->
                    Css.minHeight heightForStyle
            ]
            [ Maybe.map Events.onInput config.onInput
                |> Maybe.withDefault Extra.none
            , Maybe.map Events.onBlur config.onBlur
                |> Maybe.withDefault Extra.none
            , Attributes.value config.value
            , Attributes.id (generateId model.label)
            , Attributes.autofocus config.autofocus
            , Attributes.placeholder model.placeholder
            , Attributes.attribute "data-gramm" "false" -- disables grammarly to prevent https://github.com/NoRedInk/NoRedInk/issues/14859
            , Attributes.class "override-sass-styles custom-focus-ring"
            , Attributes.classList
                [ ( InputStyles.inputClass, True )
                , ( InputStyles.errorClass, model.isInError )
                ]
            , Aria.invalid model.isInError
            ]
            []
        , Html.label
            [ Attributes.for (generateId model.label)
            , Attributes.css
                [ if config.hideLabel then
                    Style.invisibleStyle

                  else
                    InputStyles.label config.theme model.isInError
                ]
            ]
            [ Html.text model.label ]
        ]


calculateMinHeight : Theme -> Height -> Css.Px
calculateMinHeight textAreaStyle specifiedHeight =
    {- On including padding in this calculation:

       When the textarea is autoresized, TextArea.js updates the textarea's
       height by taking its scrollHeight. Because scrollHeight's calculation
       includes the element's padding no matter what [1], we need to set the
       textarea's box-sizing to border-box in order to use the same measurement
       for its height as scrollHeight.

       So, min-height also needs to be specified in terms of padding + content
       height.

       [1] https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollHeight
    -}
    case specifiedHeight of
        SingleLine ->
            case textAreaStyle of
                Standard ->
                    singleLineHeight

                UserGenerated ->
                    singleLineHeight

                Writing ->
                    writingSingleLineHeight

        DefaultHeight ->
            case textAreaStyle of
                Standard ->
                    InputStyles.textAreaHeight

                UserGenerated ->
                    InputStyles.textAreaHeight

                Writing ->
                    InputStyles.writingMinHeight


singleLineHeight : Css.Px
singleLineHeight =
    px (.numericValue InputStyles.inputPaddingVertical + .numericValue InputStyles.inputLineHeight + .numericValue InputStyles.inputPaddingVertical)


writingSingleLineHeight : Css.Px
writingSingleLineHeight =
    px (.numericValue InputStyles.writingPaddingTop + .numericValue InputStyles.writingLineHeight + .numericValue InputStyles.writingPadding)


{-| -}
generateId : String -> String
generateId labelText =
    "nri-ui-text-area-" ++ (dashify <| removePunctuation labelText)
