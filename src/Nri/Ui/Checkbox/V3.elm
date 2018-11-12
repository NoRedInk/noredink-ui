module Nri.Ui.Checkbox.V3 exposing
    ( Model, Theme(..), IsSelected(..)
    , view, viewWithLabel, Assets
    , selectedFromBool
    )

{-|

@docs Model, Theme, IsSelected

@docs view, viewWithLabel, Assets

@docs selectedFromBool

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Global exposing (Snippet, children, descendants, everything, selector)
import Html.Events
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V3 as HtmlExtra exposing (defaultOptions)


{-| -}
type alias Model msg =
    { identifier : String
    , label : String
    , setterMsg : Bool -> msg
    , selected : IsSelected
    , disabled : Bool
    , theme : Theme
    , noOpMsg : msg
    }


{-|

    = Selected --  Checked (rendered with a checkmark)
    | NotSelected -- Not Checked (rendered blank)
    | PartiallySelected -- Indeterminate (rendered dash)

-}
type IsSelected
    = Selected
    | NotSelected
    | PartiallySelected


{-| -}
type Theme
    = Square
    | Locked


{-| If your selectedness is always selected or not selected,
you will likely store that state as a `Bool` in your model.
`selectedFromBool` lets you easily convert that into an `IsSelected` value
for use with `Nri.Ui.Checkbox`.
-}
selectedFromBool : Bool -> IsSelected
selectedFromBool isSelected =
    case isSelected of
        True ->
            Selected

        False ->
            NotSelected


selectedToMaybe : IsSelected -> Maybe Bool
selectedToMaybe selected =
    case selected of
        Selected ->
            Just True

        NotSelected ->
            Just False

        PartiallySelected ->
            Nothing


{-| Shows a checkbox (the label is only used for accessibility hints)
-}
view : Assets a -> Model msg -> Html.Html msg
view assets model =
    buildCheckbox assets model <|
        Html.span Accessibility.Styled.Style.invisible
            [ Html.text model.label ]


{-| Shows a checkbox and its label text
-}
viewWithLabel : Assets a -> Model msg -> Html.Html msg
viewWithLabel assets model =
    buildCheckbox assets model <|
        Html.span [] [ Html.text model.label ]


buildCheckbox : Assets a -> Model msg -> Html.Html msg -> Html.Html msg
buildCheckbox assets model labelContent =
    viewCheckbox model <|
        case model.theme of
            Square ->
                { containerClasses = toClassList [ "SquareClass" ]
                , labelStyles =
                    squareLabelStyles model <|
                        case model.selected of
                            Selected ->
                                assets.checkboxChecked_svg

                            NotSelected ->
                                assets.checkboxUnchecked_svg

                            PartiallySelected ->
                                assets.checkboxCheckedPartially_svg
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Locked ->
                { containerClasses = toClassList [ "Locked" ]
                , labelStyles = lockLabelStyles model assets.checkboxLockOnInside_svg
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }


squareLabelStyles : { b | disabled : Bool } -> Asset -> Html.Styled.Attribute msg
squareLabelStyles model image =
    let
        baseStyles =
            [ positioning
            , textStyle
            , outline none
            , addIcon image
            ]
    in
    css
        (baseStyles
            ++ (if model.disabled then
                    [ cursor auto, checkboxImageSelector [ opacity (num 0.4) ] ]

                else
                    [ cursor pointer ]
               )
        )


lockLabelStyles : { b | disabled : Bool } -> Asset -> Html.Styled.Attribute msg
lockLabelStyles model image =
    let
        baseStyles =
            [ positioning
            , textStyle
            , outline none
            , addIcon image
            ]
    in
    css
        (baseStyles
            ++ (if model.disabled then
                    [ cursor auto
                    , checkboxImageSelector [ opacity (num 0.4) ]
                    ]

                else
                    [ cursor pointer ]
               )
        )


positioning : Style
positioning =
    batch
        [ display inlineBlock
        , padding4 (px 13) zero (px 13) (px 35)
        ]


textStyle : Style
textStyle =
    batch
        [ Fonts.baseFont
        , fontSize (px 16)
        ]


addIcon : Asset -> Style
addIcon icon =
    batch
        [ position relative
        , checkboxImageSelector
            [ backgroundImage icon
            , backgroundRepeat noRepeat
            , backgroundSize (px 24)
            , property "content" "''"
            , position absolute
            , left zero
            , top (px 10)
            , width (px 24)
            , height (px 24)
            ]
        ]


checkboxImageSelector : List Style -> Style
checkboxImageSelector =
    before


labelClass : IsSelected -> Html.Styled.Attribute msg
labelClass isSelected =
    case isSelected of
        Selected ->
            toClassList [ "Label", "Checked" ]

        NotSelected ->
            toClassList [ "Label", "Unchecked" ]

        PartiallySelected ->
            toClassList [ "Label", "Indeterminate" ]


toClassList : List String -> Html.Styled.Attribute msg
toClassList =
    List.map (\a -> ( "checkbox-V3__" ++ a, True )) >> Attributes.classList


viewCheckbox :
    Model msg
    ->
        { containerClasses : Html.Attribute msg
        , labelStyles : Html.Attribute msg
        , labelClasses : Html.Attribute msg
        , labelContent : Html.Html msg
        }
    -> Html.Html msg
viewCheckbox model config =
    Html.Styled.span
        [ css
            [ display block
            , height inherit
            , descendants [ Css.Global.input [ display none ] ]
            ]
        , config.containerClasses
        , Attributes.id <| model.identifier ++ "-container"
        , -- This is necessary to prevent event propagation.
          -- See https://github.com/elm-lang/html/issues/96
          Attributes.map (always model.noOpMsg) <|
            Events.stopPropagationOn "click"
                (Json.Decode.succeed ( "stop click propagation", True ))
        ]
        [ Html.checkbox model.identifier
            (selectedToMaybe model.selected)
            [ Widget.label model.label
            , Events.onCheck model.setterMsg
            , Attributes.id model.identifier
            , Attributes.disabled model.disabled
            ]
        , viewLabel model config.labelContent config.labelClasses config.labelStyles
        ]


viewLabel : Model msg -> Html.Html msg -> Html.Attribute msg -> Html.Attribute msg -> Html.Html msg
viewLabel model content class theme =
    Html.Styled.label
        [ Attributes.for model.identifier
        , Aria.controls model.identifier
        , Widget.disabled model.disabled
        , Widget.checked (selectedToMaybe model.selected)
        , if not model.disabled then
            Attributes.tabindex 0

          else
            ExtraAttributes.none
        , if not model.disabled then
            --TODO: the accessibility keyboard module might make this a tad more readable.
            HtmlExtra.onKeyUp
                { defaultOptions | preventDefault = True }
                (\keyCode ->
                    -- 32 is the space bar, 13 is enter
                    if (keyCode == 32 || keyCode == 13) && not model.disabled then
                        Just <| model.setterMsg (Maybe.map not (selectedToMaybe model.selected) |> Maybe.withDefault True)

                    else
                        Nothing
                )

          else
            ExtraAttributes.none
        , class
        , theme
        ]
        [ content ]


{-| The assets used in this module.
-}
type alias Assets r =
    { r
        | checkboxUnchecked_svg : Asset
        , checkboxChecked_svg : Asset
        , checkboxCheckedPartially_svg : Asset
        , checkboxLockOnInside_svg : Asset
    }


backgroundImage : Asset -> Style
backgroundImage =
    Nri.Ui.AssetPath.Css.url
        >> property "background-image"
