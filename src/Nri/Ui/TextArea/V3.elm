module Nri.Ui.TextArea.V3
    exposing
        ( Height(..)
        , HeightBehavior(..)
        , Model
        , contentCreation
        , generateId
        , view
        , writing
        )

{-|


## Upgrading to V3

  - Do nothing! (This just uses new elm-css styles)


## The Nri styleguide-specified textarea with overlapping label

@docs view, writing, contentCreation, Height, HeightBehavior, Model, generateId, styles

-}

import Accessibility.Styled.Style
import Css exposing ((|+|))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.InputStyles.V2 as InputStyles
    exposing
        ( Theme(..)
        , input
        , label
        )
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
    view_ Standard model


{-| Used for Writing Cycles
-}
writing : Model msg -> Html msg
writing model =
    view_ Writing2 model


{-| Used for Content Creation
-}
contentCreation : Model msg -> Html msg
contentCreation model =
    view_ ContentCreation2 model


{-| -}
view_ : Theme -> Model msg -> Html msg
view_ theme model =
    let
        minHeight =
            case model.height of
                Fixed ->
                    []

                AutoResize minimumHeight ->
                    [ calculateMinHeight theme minimumHeight
                        |> Css.minHeight
                    ]

        sharedAttributes =
            [ Events.onInput model.onInput
            , Attributes.id (generateId model.label)
            , Attributes.css
                [ InputStyles.input theme model.isInError
                ]
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
        [ Attributes.css [ Css.position Css.relative ]
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
                , Attributes.css [ InputStyles.label theme model.isInError ]
                , Accessibility.Styled.Style.invisible
                ]
                [ Html.text model.label ]
          else
            Html.label
                [ Attributes.for (generateId model.label)
                , Attributes.css [ InputStyles.label theme model.isInError ]
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

                Writing2 ->
                    writingSingleLineHeight

                ContentCreation2 ->
                    singleLineHeight

        DefaultHeight ->
            case textAreaStyle of
                Standard ->
                    InputStyles.textAreaHeight

                Writing2 ->
                    InputStyles.writingMinHeight

                ContentCreation2 ->
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
