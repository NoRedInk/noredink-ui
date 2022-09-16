module Nri.Ui.Checkbox.V6 exposing
    ( Model, Theme(..), IsSelected(..)
    , view, viewWithLabel
    , selectedFromBool
    , viewIcon, checkboxLockOnInside
    )

{-|


# Patch changes

  - Use Nri.Ui.Svg.V1 rather than a custom Icon type specific to this module
  - Make the filter ids within the svg unique (now the id depends on the checkbox identifier)
  - Explicitly box-sizing content-box on the label (<https://github.com/NoRedInk/NoRedInk/pull/30886#issuecomment-737854831>)


# Changes from V5:

  - Adds `containerCss`
  - Adds `enabledLabelCss`
  - Adds `disabledLabelCss`

@docs Model, Theme, IsSelected

@docs view, viewWithLabel

@docs selectedFromBool

@docs viewIcon, checkboxLockOnInside

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import CheckboxIcons
import Css exposing (..)
import Css.Global
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 exposing (Svg)


{-| -}
type alias Model msg =
    { identifier : String
    , label : String
    , setterMsg : Bool -> msg
    , selected : IsSelected
    , disabled : Bool
    , theme : Theme
    , containerCss : List Css.Style
    , enabledLabelCss : List Css.Style
    , disabledLabelCss : List Css.Style
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
view : Model msg -> Html.Html msg
view model =
    buildCheckbox model
        (\label ->
            Html.span Accessibility.Styled.Style.invisible
                [ Html.text label ]
        )


{-| Shows a checkbox and its label text
-}
viewWithLabel : Model msg -> Html.Html msg
viewWithLabel model =
    buildCheckbox model <|
        \label -> Html.span [] [ Html.text label ]


buildCheckbox : Model msg -> (String -> Html.Html msg) -> Html.Html msg
buildCheckbox model labelView =
    checkboxContainer model
        [ viewCheckbox model
        , case model.theme of
            Square ->
                let
                    icon =
                        case model.selected of
                            Selected ->
                                CheckboxIcons.checked model.identifier

                            NotSelected ->
                                CheckboxIcons.unchecked model.identifier

                            PartiallySelected ->
                                CheckboxIcons.checkedPartially model.identifier
                in
                if model.disabled then
                    viewDisabledLabel model labelView icon

                else
                    viewEnabledLabel model labelView icon

            Locked ->
                if model.disabled then
                    viewDisabledLabel model labelView (checkboxLockOnInside model.identifier)

                else
                    viewEnabledLabel model labelView (checkboxLockOnInside model.identifier)
        ]


checkboxContainer : { a | identifier : String, containerCss : List Style } -> List (Html.Html msg) -> Html.Html msg
checkboxContainer model =
    Html.Styled.span
        [ css
            [ display block
            , height inherit
            , position relative
            , marginLeft (px -4)
            , pseudoClass "focus-within"
                [ Css.Global.descendants
                    [ Css.Global.class "checkbox-icon-container" FocusRing.tightStyles
                    ]
                ]
            , Css.Global.descendants
                [ Css.Global.input
                    [ position absolute
                    , top (calc (pct 50) minus (px 10))
                    , left (px 10)
                    , boxShadow none |> Css.important
                    ]
                ]
            , Css.batch model.containerCss
            ]
        , Attributes.id (model.identifier ++ "-container")
        , Events.stopPropagationOn "click" (Json.Decode.fail "stop click propagation")
        ]


viewCheckbox :
    { a
        | identifier : String
        , setterMsg : Bool -> msg
        , selected : IsSelected
        , disabled : Bool
    }
    -> Html.Html msg
viewCheckbox model =
    Html.checkbox model.identifier
        (selectedToMaybe model.selected)
        [ Attributes.id model.identifier
        , if model.disabled then
            Aria.disabled True

          else
            Events.onCheck (\_ -> onCheck model)
        ]


viewEnabledLabel :
    { a
        | identifier : String
        , setterMsg : Bool -> msg
        , selected : IsSelected
        , label : String
        , enabledLabelCss : List Style
    }
    -> (String -> Html.Html msg)
    -> Svg
    -> Html.Html msg
viewEnabledLabel model labelView icon =
    Html.Styled.label
        [ Attributes.for model.identifier
        , labelClass model.selected
        , css
            [ positioning
            , textStyle
            , cursor pointer
            , Css.batch model.enabledLabelCss
            ]
        ]
        [ viewIcon [] icon
        , labelView model.label
        ]


onCheck : { a | setterMsg : Bool -> msg, selected : IsSelected } -> msg
onCheck model =
    selectedToMaybe model.selected
        |> Maybe.withDefault False
        |> not
        |> model.setterMsg


viewDisabledLabel :
    { a | identifier : String, selected : IsSelected, label : String, disabledLabelCss : List Style }
    -> (String -> Html.Html msg)
    -> Svg
    -> Html.Html msg
viewDisabledLabel model labelView icon =
    Html.Styled.label
        [ Attributes.for model.identifier
        , labelClass model.selected
        , css
            [ positioning
            , textStyle
            , outline none
            , cursor auto
            , Css.batch model.disabledLabelCss
            ]
        ]
        [ viewIcon [ opacity (num 0.4) ] icon
        , labelView model.label
        ]


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
    List.map (\a -> ( "checkbox-V5__" ++ a, True )) >> Attributes.classList


positioning : Style
positioning =
    batch
        [ display inlineBlock
        , padding4 (px 13) zero (px 13) (px 40)
        , position relative
        ]


textStyle : Style
textStyle =
    batch
        [ Fonts.baseFont
        , fontSize (px 15)
        , fontWeight (int 600)
        , color Colors.navy
        ]


{-| -}
viewIcon : List Style -> Svg -> Html.Html msg
viewIcon styles icon =
    Html.div
        [ css
            [ position absolute
            , left zero
            , top (calc (pct 50) minus (px 18))
            , border3 (px 2) solid transparent
            , padding (px 2)
            , borderRadius (px 3)
            , height (Css.px 27)
            , boxSizing contentBox
            ]
        , Attributes.class "checkbox-icon-container"
        ]
        [ Html.div
            [ css
                [ display inlineBlock
                , backgroundColor Colors.white
                , height (Css.px 27)
                , borderRadius (px 4)
                ]
            ]
            [ Nri.Ui.Svg.V1.toHtml (Nri.Ui.Svg.V1.withCss styles icon)
            ]
        ]


{-| -}
checkboxLockOnInside : String -> Svg
checkboxLockOnInside =
    CheckboxIcons.lockOnInside
