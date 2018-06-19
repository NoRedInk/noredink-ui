module Nri.Ui.Radio.V3
    exposing
        ( IsSelected(..)
        , Model
        , view
        , viewWithLabel
        )

{-|

@docs Model, IsSelected

@docs view, viewWithLabel

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html.Events exposing (defaultOptions)
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V2 as HtmlExtra


{-| -}
type alias Model msg =
    { identifier : String
    , label : String
    , setterMsg : Bool -> msg
    , selected : IsSelected
    , disabled : Bool
    , noOpMsg : msg
    }


{-|

    = Selected --  Checked (rendered with a checkmark)
    | NotSelected -- Not Checked (rendered blank)

-}
type IsSelected
    = Selected
    | NotSelected


selectedToMaybe : IsSelected -> Maybe Bool
selectedToMaybe selected =
    case selected of
        Selected ->
            Just True

        NotSelected ->
            Just False


{-| Shows a checkbox (the label is only used for accessibility hints)
-}
view : Assets a -> Model msg -> Html.Html msg
view assets model =
    buildRadio assets [] model <|
        Html.span [ Accessibility.Styled.Style.invisible ]
            [ Html.text model.label ]


{-| Shows a checkbox and its label text
-}
viewWithLabel : Assets a -> Model msg -> Html.Html msg
viewWithLabel assets model =
    buildRadio assets [] model <|
        Html.span [] [ Html.text model.label ]


buildRadio : Assets a -> List String -> Model msg -> Html.Html msg -> Html.Html msg
buildRadio assets modifierClasses model labelContent =
    let
        toClassList =
            List.map (\a -> ( "checkbox-" ++ a, True )) >> Attributes.classList
    in
    viewCheckbox model
        { containerClasses = toClassList (modifierClasses ++ [ "RoundClass" ])
        , labelStyles =
            css
                [ alignItems center
                , backgroundRepeat noRepeat
                , color Colors.gray20
                , if model.disabled then
                    cursor auto
                  else
                    cursor pointer
                , displayFlex
                , Fonts.baseFont
                , fontSize (px 16)
                , minHeight (px 42) -- container height
                , outline none
                , padding2 (px 13) zero
                , property "background-position" "left center"
                , before
                    [ property "content" "''"
                    , width (px 24)
                    , height (px 24)
                    , marginRight (px 8)
                    , borderRadius (pct 100)
                    ]
                , Css.batch <|
                    case model.selected of
                        Selected ->
                            [ before
                                [ backgroundColor Colors.green
                                , border3 (px 2) solid Colors.green
                                , property "background-image"
                                    (Nri.Ui.AssetPath.Css.url assets.iconCheck_png)
                                , property "background-repeat" "no-repeat"
                                , property "background-position" "center center"
                                ]
                            ]

                        NotSelected ->
                            [ before
                                [ border3 (px 2) solid Colors.blue
                                , backgroundColor Colors.white
                                ]
                            ]
                ]
        , labelClasses = labelClass model.selected
        , labelContent = labelContent
        }


labelClass : IsSelected -> Html.Styled.Attribute msg
labelClass isSelected =
    case isSelected of
        Selected ->
            Attributes.classList
                [ ( "checkbox-Label", True )
                , ( "checkbox-Checked", True )
                ]

        NotSelected ->
            Attributes.classList
                [ ( "checkbox-Label", True )
                , ( "checkbox-Unchecked", True )
                ]


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
            , descendants [ Css.Foreign.input [ display none ] ]
            ]
        , config.containerClasses
        , Attributes.id <| model.identifier ++ "-container"
        , -- This is necessary to prevent event propagation.
          -- See https://github.com/elm-lang/html/issues/96
          Attributes.map (always model.noOpMsg) <|
            Events.onWithOptions "click"
                { defaultOptions | stopPropagation = True }
                (Json.Decode.succeed "stop click propagation")
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
        | iconCheck_png : Asset
    }
