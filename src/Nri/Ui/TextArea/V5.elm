module Nri.Ui.TextArea.V5 exposing
    ( view, generateId
    , Attribute
    , value
    , onInput, onBlur, onFocus
    , hiddenLabel, visibleLabel
    , css, noMargin
    , standard, writing
    , autoResize, autoResizeSingleLine
    , custom, nriDescription, id, testId
    , placeholder, autofocus
    , disabled, errorIf, errorMessage, guidance, guidanceHtml
    )

{-|


## Changelog


### Patch changes

  - No longer defaults the placeholder value to the label text
  - Adjust disabled styles
  - Fix initial autoresize not working in Firefox


### Changes from V4

  - Removes contentCreation view styles
  - Changes to a list-based API
  - Adds guidance and errorMessage support
  - Adds id, custom, nriDescription, testId, css, and noMargin
  - Adds disabled support
  - Adds onFocus


## The next version of TextArea should:

  - update the disabled styles


## Upgrading to V4

  - Adds field for onBlur


## The Nri Component-Catalog-specified textarea with overlapping label


## Creating New Versions

When upgrading this module, we need to make sure to also include a new
custom element, or else autosizing will break! This means doing the following:

1.  Creating a new module in `lib/TextArea`
2.  Requiring that module in `lib/index.js`


## API

@docs view, generateId
@docs Attribute
@docs value


### Event handlers

@docs onInput, onBlur, onFocus


### Visual behavior

@docs hiddenLabel, visibleLabel
@docs css, noMargin
@docs standard, writing
@docs autoResize, autoResizeSingleLine


### Other

@docs custom, nriDescription, id, testId
@docs placeholder, autofocus
@docs disabled, errorIf, errorMessage, guidance, guidanceHtml

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import InputLabelInternal
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.InputStyles.V4 as InputStyles exposing (Theme(..))


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { theme : Theme
    , guidance : Guidance msg
    , error : ErrorState
    , hideLabel : Bool
    , value : String
    , autofocus : Bool
    , onInput : Maybe (String -> msg)
    , onBlur : Maybe msg
    , onFocus : Maybe msg
    , placeholder : Maybe String
    , noMarginTop : Bool
    , containerCss : List Css.Style
    , custom : List (Html.Attribute Never)
    , id : Maybe String
    , height : HeightBehavior
    , disabled : Bool
    }


defaultConfig : Config msg
defaultConfig =
    { theme = Standard
    , guidance = InputErrorAndGuidanceInternal.noGuidance
    , error = InputErrorAndGuidanceInternal.noError
    , hideLabel = False
    , value = ""
    , autofocus = False
    , onInput = Nothing
    , onBlur = Nothing
    , onFocus = Nothing
    , placeholder = Nothing
    , noMarginTop = False
    , containerCss = []
    , custom = []
    , id = Nothing
    , height = Fixed
    , disabled = False
    }


applyConfig : List (Attribute msg) -> Config msg
applyConfig =
    List.foldl (\(Attribute update) config -> update config) defaultConfig


{-| Customizations for the TextArea.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


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


{-| -}
autoResize : Attribute msg
autoResize =
    Attribute (\soFar -> { soFar | height = AutoResize DefaultHeight })


{-| -}
autoResizeSingleLine : Attribute msg
autoResizeSingleLine =
    Attribute (\soFar -> { soFar | height = AutoResize SingleLine })


{-| -}
value : String -> Attribute msg
value value_ =
    Attribute (\soFar -> { soFar | value = value_ })


{-| -}
placeholder : String -> Attribute msg
placeholder text_ =
    Attribute (\soFar -> { soFar | placeholder = Just text_ })


{-| This disables the textarea.
-}
disabled : Attribute msg
disabled =
    Attribute (\config -> { config | disabled = True })


{-| Sets whether or not the field will be highlighted as having a validation error.
-}
errorIf : Bool -> Attribute msg
errorIf =
    Attribute << InputErrorAndGuidanceInternal.setErrorIf


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute msg
errorMessage =
    Attribute << InputErrorAndGuidanceInternal.setErrorMessage


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute msg
guidance =
    Attribute << InputErrorAndGuidanceInternal.setGuidance


{-| A guidance message (HTML) shows below the input, unless an error message is showing instead.
-}
guidanceHtml : List (Html msg) -> Attribute msg
guidanceHtml =
    Attribute << InputErrorAndGuidanceInternal.setGuidanceHtml


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


{-| Produce the given `msg` when the input is changed.
-}
onInput : (String -> msg) -> Attribute msg
onInput msg =
    Attribute (\soFar -> { soFar | onInput = Just msg })


{-| Produce the given `msg` when the field is blurred.
-}
onBlur : msg -> Attribute msg
onBlur msg =
    Attribute (\soFar -> { soFar | onBlur = Just msg })


{-| Produce the given `msg` when the field is focused.
-}
onFocus : msg -> Attribute msg
onFocus msg =
    Attribute (\soFar -> { soFar | onFocus = Just msg })


{-| Sets the `autofocus` attribute of the textarea to true.
-}
autofocus : Attribute msg
autofocus =
    Attribute (\soFar -> { soFar | autofocus = True })


{-| Adds CSS to the element containing the textarea.
-}
css : List Css.Style -> Attribute msg
css styles =
    Attribute (\soFar -> { soFar | containerCss = soFar.containerCss ++ styles })


{-| Remove default spacing from the Input.
-}
noMargin : Bool -> Attribute msg
noMargin removeMargin =
    Attribute (\soFar -> { soFar | noMarginTop = removeMargin })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute msg
custom attributes =
    Attribute (\soFar -> { soFar | custom = soFar.custom ++ attributes })


{-| Set a custom ID for this text area. If you don't set the id explicitly,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one textarea with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs.
-}
id : String -> Attribute msg
id id_ =
    Attribute (\soFar -> { soFar | id = Just id_ })


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ Extra.testId id_ ]


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
view : String -> List (Attribute msg) -> Html msg
view label attributes =
    view_ label (applyConfig attributes)


{-| -}
view_ : String -> Config msg -> Html msg
view_ label config =
    let
        autoresizeAttrs =
            case config.height of
                AutoResize specifiedHeight ->
                    List.filterMap
                        identity
                        [ Just <| Attributes.attribute "data-autoresize" ""
                        , case specifiedHeight of
                            DefaultHeight ->
                                Nothing

                            SingleLine ->
                                Just <| Attributes.attribute "data-singleline" ""
                        ]

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

        idValue : String
        idValue =
            Maybe.withDefault (generateId label) config.id

        isInError =
            InputErrorAndGuidanceInternal.getIsInError config.error
    in
    Html.styled (Html.node "nri-textarea-v5")
        [ Css.display Css.block, Css.position Css.relative, Css.batch config.containerCss ]
        autoresizeAttrs
        ([ Html.styled Html.textarea
            [ InputStyles.input config.theme
            , Css.boxSizing Css.borderBox
            , case config.height of
                AutoResize minimumHeight ->
                    Css.minHeight (calculateMinHeight config.theme minimumHeight)

                Fixed ->
                    Css.minHeight heightForStyle
            , if config.noMarginTop then
                Css.important (Css.marginTop Css.zero)

              else
                Css.batch []
            , Css.batch
                (if config.disabled then
                    [ Css.boxShadow Css.none |> Css.important
                    , Css.borderColor Colors.gray85 |> Css.important
                    , Css.backgroundColor Colors.gray85
                    ]

                 else
                    []
                )
            ]
            ([ Maybe.map Events.onInput config.onInput
                |> Maybe.withDefault Extra.none
             , Maybe.map Events.onBlur config.onBlur
                |> Maybe.withDefault Extra.none
             , Maybe.map Events.onFocus config.onFocus
                |> Maybe.withDefault Extra.none
             , Attributes.value config.value
             , Attributes.disabled config.disabled
             , Attributes.id idValue
             , Attributes.autofocus config.autofocus
             , Maybe.map Attributes.placeholder config.placeholder
                |> Maybe.withDefault Extra.none
             , Attributes.attribute "data-gramm" "false" -- disables grammarly to prevent https://github.com/NoRedInk/NoRedInk/issues/14859
             , Attributes.class "override-sass-styles custom-focus-ring"
             , Attributes.classList
                [ ( InputStyles.inputClass, True )
                , ( InputStyles.errorClass, isInError )
                ]
             , Aria.invalid isInError
             , InputErrorAndGuidanceInternal.describedBy idValue config
             ]
                ++ List.map (Attributes.map never) config.custom
            )
            []
         , InputLabelInternal.view
            { for = idValue
            , label = label
            , theme = config.theme
            }
            config
         ]
            ++ InputErrorAndGuidanceInternal.view idValue InputErrorAndGuidanceInternal.smallMargin config
        )


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
generateId =
    Extra.safeIdWithPrefix "nri-ui-text-area"
