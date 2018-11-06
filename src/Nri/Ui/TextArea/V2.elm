module Nri.Ui.TextArea.V2 exposing (view, writing, contentCreation, Height(..), HeightBehavior(..), Model, generateId, styles)

{-|


## Upgrading from V2

  - The Model's autoResize field is now `height : HeightBehavior`, which can be
    either `Fixed` or `AutoResize Height`. `Height` is either
    `DefaultHeight` or `SingleLine` and controls the minimum height of the textarea.

  - The view now returns `Html.Styled` rather than plain `Html`.


## The Nri styleguide-specified textarea with overlapping label

@docs view, writing, contentCreation, Height, HeightBehavior, Model, generateId, styles

-}

import Accessibility.Styled.Style
import Css exposing ((|+|))
import DEPRECATED.Nri.Ui.Styles.V1 exposing (StylesWithAssets)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.InputStyles as InputStyles exposing (CssClasses(..))
import Nri.Ui.Util exposing (dashify, removePunctuation)


{-| -}
type alias Model msg =
    { value : String
    , autofocus : Bool
    , onInput : String -> msg
    , isInError : Bool
    , height : HeightBehavior
    , placeholder : String
    , label : String
    , showLabel : Bool
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


{-| -}
view : Model msg -> Html msg
view model =
    view_ DefaultStyle model


{-| Used for Writing Cycles
-}
writing : Model msg -> Html msg
writing model =
    view_ WritingStyle model


{-| Used for Content Creation
-}
contentCreation : Model msg -> Html msg
contentCreation model =
    view_ ContentCreationStyle model


type TextAreaStyle
    = DefaultStyle
    | WritingStyle
    | ContentCreationStyle


{-| -}
view_ : TextAreaStyle -> Model msg -> Html msg
view_ textAreaStyle model =
    let
        showWritingClass =
            textAreaStyle == WritingStyle

        showContentCreationClass =
            textAreaStyle == ContentCreationStyle

        minHeight =
            case model.height of
                Fixed ->
                    []

                AutoResize minimumHeight ->
                    -- FIXME: Css.important is needed here because InputStyles's
                    -- min-height rule has more specificity. It can go away once
                    -- we've fully migrated to Html.Styled.
                    [ calculateMinHeight textAreaStyle minimumHeight
                        |> Css.minHeight
                        |> Css.important
                    ]

        sharedAttributes =
            [ Events.onInput model.onInput
            , Attributes.id (generateId model.label)
            , styles.class [ Input ]
                |> Attributes.fromUnstyled
            , Attributes.autofocus model.autofocus
            , Attributes.placeholder model.placeholder
            , Attributes.attribute "data-gramm" "false" -- disables grammarly to prevent https://github.com/NoRedInk/NoRedInk/issues/14859
            , Attributes.css
                (minHeight
                    ++ [ Css.boxSizing Css.borderBox ]
                )
            ]
    in
    Html.div
        [ styles.classList
            [ ( Container, True )
            , ( IsInError, model.isInError )
            , ( Writing, showWritingClass )
            , ( ContentCreation, showContentCreationClass )
            ]
            |> Attributes.fromUnstyled
        ]
        [ case model.height of
            AutoResize _ ->
                {- NOTES:
                   The autoresize-textarea element is implemented to pass information applied to itself to an internal
                   textarea element that it inserts into the DOM automatically. Maintaing this behavior may require some
                   changes on your part, as listed below.

                   - When adding an Html.Attribute that is a _property_, you must edit Nri/TextArea.js to ensure that a getter and setter
                   are set up to properly reflect the property to the actual textarea element that autoresize-textarea creates
                   - When adding a new listener from Html.Events, you must edit Nri/TextArea.js to ensure that a listener is set up on
                   the textarea that will trigger this event on the autoresize-textarea element itself. See AutoresizeTextArea.prototype._onInput
                   and AutoresizeTextArea.prototype.connectedCallback for an example pertaining to the `input` event
                   - When adding a new Html.Attribute that is an _attribute_, you don't have to do anything. All attributes are
                   automatically reflected onto the textarea element via AutoresizeTextArea.prototype.attributeChangedCallback
                -}
                Html.node "autoresize-textarea"
                    (sharedAttributes
                        ++ [ -- setting the default value via a text node doesn't play well with the custom element,
                             -- but we'll be able to switch to the regular value property in 0.19 anyway
                             Attributes.defaultValue model.value
                           ]
                    )
                    []

            Fixed ->
                Html.textarea sharedAttributes
                    [ Html.text model.value ]
        , if not model.showLabel then
            Html.label
                [ Attributes.for (generateId model.label)
                , styles.class [ Label ]
                    |> Attributes.fromUnstyled
                , Accessibility.Styled.Style.invisible
                ]
                [ Html.text model.label ]

          else
            Html.label
                [ Attributes.for (generateId model.label)
                , styles.class [ Label ]
                    |> Attributes.fromUnstyled
                ]
                [ Html.text model.label ]
        ]


calculateMinHeight : TextAreaStyle -> Height -> Css.Px
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
                DefaultStyle ->
                    singleLineHeight

                WritingStyle ->
                    writingSingleLineHeight

                ContentCreationStyle ->
                    singleLineHeight

        DefaultHeight ->
            case textAreaStyle of
                DefaultStyle ->
                    InputStyles.textAreaHeight

                WritingStyle ->
                    InputStyles.writingMinHeight

                ContentCreationStyle ->
                    InputStyles.textAreaHeight


singleLineHeight : Css.Px
singleLineHeight =
    InputStyles.inputPaddingVertical |+| InputStyles.inputLineHeight |+| InputStyles.inputPaddingVertical


writingSingleLineHeight : Css.Px
writingSingleLineHeight =
    InputStyles.writingPaddingTop |+| InputStyles.writingLineHeight |+| InputStyles.writingPadding


{-| -}
generateId : String -> String
generateId labelText =
    "nri-ui-text-area-" ++ (dashify <| removePunctuation labelText)


{-| -}
styles : StylesWithAssets Never CssClasses msg (InputStyles.Assets r)
styles =
    InputStyles.styles
