module Nri.Ui.Checkbox.V3
    exposing
        ( ColorTheme(..)
        , IsSelected(..)
        , Model
        , PremiumConfig
        , Theme(..)
        , premium
        , view
        , viewWithLabel
        )

{-|

@docs Model, Theme, ColorTheme, IsSelected

@docs view, viewWithLabel


## Premium

@docs PremiumConfig, premium

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html as RootHtml
import Html.Attributes as RootAttributes
import Html.Events exposing (defaultOptions)
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Json.Encode
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
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
    buildCheckbox assets [] model <|
        Html.span [ Accessibility.Styled.Style.invisible ]
            [ Html.text model.label ]


{-| Shows a checkbox and its label text
-}
viewWithLabel : Assets a -> Model msg -> Html.Html msg
viewWithLabel assets model =
    buildCheckbox assets [] model <|
        Html.span [] [ Html.text model.label ]


{-|

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
type alias PremiumConfig msg =
    { label : String
    , id : String
    , selected : IsSelected
    , disabled : Bool
    , teacherPremiumLevel : PremiumLevel
    , contentPremiumLevel : PremiumLevel
    , showFlagWhenLocked : Bool
    , onChange : Bool -> msg
    , onLockedClick : msg
    , noOpMsg : msg
    }


{-| A checkbox that should be used for premium content

This checkbox is locked when the premium level of the content is greater than the premium level of the teacher

-}
premium : Assets a -> PremiumConfig msg -> Html.Html msg
premium assets config =
    let
        isLocked =
            not <|
                PremiumLevel.allowedFor
                    config.contentPremiumLevel
                    config.teacherPremiumLevel

        modifierClasses =
            List.concat
                [ if config.showFlagWhenLocked && config.contentPremiumLevel /= Free then
                    [ "PremiumClass" ]
                  else
                    []
                ]

        theme =
            if isLocked then
                LockOnInside
            else if config.contentPremiumLevel /= Free then
                Premium
            else
                Square Default
    in
    buildCheckbox assets
        modifierClasses
        { identifier = config.id
        , label = config.label
        , setterMsg =
            if isLocked then
                \_ -> config.onLockedClick
            else
                config.onChange
        , selected = config.selected
        , disabled = config.disabled
        , theme = theme
        , noOpMsg = config.noOpMsg
        }
    <|
        Html.span [] [ Html.text config.label ]


buildCheckbox : Assets a -> List String -> Model msg -> Html.Html msg -> Html.Html msg
buildCheckbox assets modifierClasses model labelContent =
    let
        toClassList =
            List.map (\a -> ( "checkbox-" ++ a, True )) >> Attributes.classList
    in
    viewCheckbox model <|
        case model.theme of
            Square colorTheme ->
                { containerClasses = toClassList (modifierClasses ++ [ "SquareClass" ])
                , labelStyles =
                    css
                        [ backgroundRepeat noRepeat
                        , color Colors.gray20
                        , if model.disabled then
                            cursor auto
                          else
                            cursor pointer
                        , Fonts.baseFont
                        , fontSize (px 16)
                        , minHeight (px 42) -- container height
                        , outline none
                        , padding2 (px 13) zero
                        , paddingLeft (px (29 + 6)) -- checkbox width + padding
                        , property "background-position" "left center"
                        , verticalAlign middle
                        , display inlineBlock
                        , case model.selected of
                            Selected ->
                                backgroundImage (assetPathUrl assets.checkboxChecked_svg)

                            NotSelected ->
                                backgroundImage (assetPathUrl assets.checkboxUnchecked_svg)

                            PartiallySelected ->
                                backgroundImage (assetPathUrl assets.checkboxCheckedPartially_svg)
                        , Css.batch <|
                            case colorTheme of
                                Gray ->
                                    [ color Colors.gray45 ]

                                Default ->
                                    []
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Locked ->
                { containerClasses = toClassList (modifierClasses ++ [ "LockedClass" ])
                , labelStyles =
                    css
                        [ backgroundImage (assetPathUrl assets.iconPremiumLocked_png)
                        , backgroundRepeat noRepeat
                        , color Colors.gray20
                        , display inlineBlock
                        , Fonts.baseFont
                        , fontSize (px 16)
                        , minHeight (px 42) -- container height
                        , outline none
                        , padding2 (px 13) zero
                        , paddingLeft (px (29 + 6)) -- checkbox width + padding
                        , property "background-position" "left center"
                        , verticalAlign middle
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            LockOnInside ->
                { containerClasses = toClassList (modifierClasses ++ [ "LockOnInsideClass" ])
                , labelStyles =
                    css
                        [ backgroundImage (assetPathUrl assets.checkboxLockOnInside_svg)
                        , backgroundRepeat noRepeat
                        , backgroundSize (px 24)
                        , color Colors.gray20
                        , if model.disabled then
                            cursor auto
                          else
                            cursor pointer
                        , Fonts.baseFont
                        , fontSize (px 16)
                        , minHeight (px 42) -- container height
                        , outline none
                        , padding2 (px 13) zero
                        , paddingLeft (px 35)
                        , property "background-position" "left center"
                        , verticalAlign middle
                        , display inlineBlock
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            --disabledStyles =
            --    [ cursor pointer
            --    , opacity (num 0.4)
            --    , outline none
            --    ]
            Premium ->
                { containerClasses = toClassList (modifierClasses ++ [ "SquareClass", "PremiumClass" ])
                , labelStyles = premiumLabelStyles assets.iconPremiumFlag_svg model
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

        PartiallySelected ->
            Attributes.classList
                [ ( "checkbox-Label", True )
                , ( "checkbox-Indeterminate", True )
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


premiumLabelStyles :
    Asset
    -> { a | disabled : Bool }
    -> Html.Styled.Attribute msg
premiumLabelStyles image model =
    let
        baseStyles =
            [ -- Positioning
              alignItems center
            , displayFlex

            -- Focus & Hover
            , cursor pointer
            , outline none

            -- Icon
            , icon
            ]

        icon =
            after
                [ property "content" "''"
                , display inlineBlock
                , width (px 26)
                , height (px 24)
                , marginLeft (px 8)
                , backgroundImage (assetPathUrl image)
                , backgroundRepeat noRepeat
                , backgroundPosition Css.center
                ]
    in
    css
        (if model.disabled then
            opacity (num 0.4) :: baseStyles
         else
            baseStyles
        )


assetPathUrl : Asset -> BackgroundImage {}
assetPathUrl =
    url << Nri.Ui.AssetPath.Css.url


indeterminateAttr : RootHtml.Attribute msg
indeterminateAttr =
    RootAttributes.property "indeterminate" (Json.Encode.bool True)


{-| -}
type Theme
    = Square ColorTheme
    | Locked
    | LockOnInside
    | Premium


{-| -}
type ColorTheme
    = Default
    | Gray


{-| The assets used in this module.
-}
type alias Assets r =
    { r
        | checkboxUnchecked_svg : Asset
        , checkboxChecked_svg : Asset
        , checkboxCheckedPartially_svg : Asset
        , iconPremiumUnlocked_png : Asset
        , iconPremiumLocked_png : Asset
        , checkboxLockOnInside_svg : Asset
        , iconPremiumFlag_svg : Asset
    }
