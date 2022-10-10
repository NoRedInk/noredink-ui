module Nri.Ui.TextArea.V5 exposing (view, writing, Height(..), HeightBehavior(..), Model, generateId)

{-|


## The next version of TextArea should:

  - switch to a list-based API
  - add support for `guidance`
  - add support for `errorMessage`


## Upgrading to V4

  - Adds field for onBlur


## The Nri styleguide-specified textarea with overlapping label


## Creating New Versions

When upgrading this module, we need to make sure to also include a new
custom element, or else autosizing will break! This means doing the following:

1.  Creating a new module in `lib/TextArea`
2.  Requiring that module in `lib/index.js`

@docs view, writing, Height, HeightBehavior, Model, generateId

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
type alias Model msg =
    { value : String
    , autofocus : Bool
    , onInput : String -> msg
    , onBlur : Maybe msg
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
    view_ Writing model


{-| -}
view_ : Theme -> Model msg -> Html msg
view_ theme model =
    let
        autoresizeAttrs =
            case model.height of
                AutoResize _ ->
                    [ Attributes.attribute "data-autoresize" "" ]

                Fixed ->
                    []

        heightForStyle =
            case theme of
                Standard ->
                    InputStyles.textAreaHeight

                UserGenerated ->
                    InputStyles.textAreaHeight

                Writing ->
                    InputStyles.writingMinHeight
    in
    Html.styled Html.div
        [ Css.position Css.relative ]
        []
        [ Html.styled (Html.node "nri-textarea-v5")
            [ Css.display Css.block ]
            autoresizeAttrs
            [ Html.styled Html.textarea
                [ InputStyles.input theme
                , Css.boxSizing Css.borderBox
                , case model.height of
                    AutoResize minimumHeight ->
                        Css.minHeight (calculateMinHeight theme minimumHeight)

                    Fixed ->
                        Css.minHeight heightForStyle
                ]
                [ Events.onInput model.onInput
                , Maybe.withDefault Extra.none (Maybe.map Events.onBlur model.onBlur)
                , Attributes.value model.value
                , Attributes.id (generateId model.label)
                , Attributes.autofocus model.autofocus
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
                    [ if not model.showLabel then
                        Style.invisibleStyle

                      else
                        InputStyles.label theme model.isInError
                    ]
                ]
                [ Html.text model.label ]
            ]
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
