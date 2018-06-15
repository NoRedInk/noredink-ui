module Nri.Ui.Checkbox.V3
    exposing
        ( ColorTheme(..)
        , IsSelected(..)
        , Model
        , PremiumConfig
        , Theme(..)
        , disabled
        , premium
        , styles
        , view
        , viewWithLabel
        )

{-|

@docs Model, Theme, ColorTheme

@docs view, viewWithLabel, disabled


## Premium

@docs PremiumConfig, IsSelected, premium

-}

import Accessibility exposing (checkbox, div, label, span, text)
import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Accessibility.Widget
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
import Nri.Ui.Html.Attributes.Extra as RootAttributes
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V2 as HtmlExtra
import Nri.Ui.Styles.V1


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


{-| Show a disabled checkbox.
-}
disabled : String -> String -> RootHtml.Html msg
disabled identifier labelText =
    span
        [ styles.class [ Container, SquareClass, Opacified ]
        , RootAttributes.id <| identifier ++ "-container"
        ]
        [ checkbox identifier
            (Just False)
            [ Accessibility.Widget.label labelText
            , styles.class [ Checkbox ]
            , RootAttributes.id identifier
            , RootAttributes.disabled True
            ]
        , label
            [ RootAttributes.for identifier
            , styles.class [ Label, Indeterminate ]
            ]
            [ RootHtml.text labelText
            ]
        ]


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
                , if config.disabled then
                    [ "Opacified" ]
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
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "SquareClass" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        , paddingLeft (px (29 + 6)) -- checkbox width + padding
                        , case model.selected of
                            Selected ->
                                backgroundImage assets CheckboxChecked

                            NotSelected ->
                                backgroundImage assets CheckboxUnchecked

                            PartiallySelected ->
                                backgroundImage assets CheckboxCheckedPartially
                        , Css.batch <|
                            case colorTheme of
                                Gray ->
                                    [ color Colors.gray45 ]

                                Orange ->
                                    [ color Colors.ochre
                                    , displayFlex
                                    , alignItems center
                                    , after
                                        [ property "content" "''"
                                        , width (px 26)
                                        , height (px 24)
                                        , marginLeft (px 8)
                                        , backgroundImage assets PremiumUnlocked
                                        ]
                                    ]

                                Default ->
                                    []
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Locked ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "LockedClass" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            LockOnInside ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "LockOnInsideClass" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Unlockable ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "UnlockableClass" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Round ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "RoundClass" ])
                , labelStyles =
                    css
                        [ if model.disabled then
                            cursor auto
                          else
                            cursor pointer
                        , outline none
                        , displayFlex
                        , alignItems center
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
                                        , backgroundImage assets CheckWhite
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

                                PartiallySelected ->
                                    -- it's kinda weird that we have round "checkboxes"
                                    -- that can't be indeterminate that we nonetheless
                                    -- model as Maybes. what can you do.
                                    []
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Disabled ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "SquareClass", "Opacified" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Premium ->
                { containerStyles = css containerStyles
                , containerClasses = toClassList (modifierClasses ++ [ "SquareClass", "PremiumClass" ])
                , labelStyles =
                    css
                        [ cursor pointer
                        , outline none
                        ]
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }


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
        { containerStyles : Html.Attribute msg
        , containerClasses : Html.Attribute msg
        , labelStyles : Html.Attribute msg
        , labelClasses : Html.Attribute msg
        , labelContent : Html.Html msg
        }
    -> Html.Html msg
viewCheckbox model config =
    Html.Styled.span
        [ config.containerStyles
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


indeterminateAttr : RootHtml.Attribute msg
indeterminateAttr =
    RootAttributes.property "indeterminate" (Json.Encode.bool True)


type CssClasses
    = Container
    | Checkbox
    | Unchecked
    | Checked
    | Indeterminate
    | SquareClass
    | RoundClass
    | GrayClass
    | OrangeClass
    | LockedClass
    | LockOnInsideClass
    | UnlockableClass
    | Label
    | Opacified
    | PremiumClass


type CheckboxImage
    = CheckboxUnchecked
    | CheckboxChecked
    | CheckboxCheckedPartially
    | PremiumUnlocked
    | PremiumFlag
    | CheckWhite
    | PremiumLocked
    | PremiumKey
    | CheckboxLockOnInside


{-| -}
type Theme
    = Square ColorTheme
    | Round
    | Locked
    | LockOnInside
    | Unlockable
    | Disabled
    | Premium


{-| -}
type ColorTheme
    = Default
    | Gray
    | Orange


containerStyles =
    [ display block
    , height inherit
    , descendants
        [ Css.Foreign.label
            [ display inlineBlock
            , verticalAlign middle
            , minHeight (px 42) -- container height
            , padding2 (px 13) zero
            , fontSize (px 16)
            , Fonts.baseFont
            , color Colors.gray20
            , property "background-position" "left center"
            , property "background-repeat" "no-repeat"
            ]
        , Css.Foreign.input [ display none ]
        , selector ":disabled + label"
            [ cursor auto
            ]
        ]
    ]


lockedStyles assets =
    [ descendants
        [ Css.Foreign.label
            [ paddingLeft (px (29 + 6)) -- checkbox width + padding
            , backgroundImage assets PremiumLocked
            , property "cursor" "auto"
            ]
        ]
    ]


lockOnInsideStyles assets =
    [ descendants
        [ Css.Foreign.label
            [ paddingLeft (px 35)
            , backgroundImage assets CheckboxLockOnInside
            , backgroundSize (px 24)
            , backgroundRepeat noRepeat
            , property "cursor" "pointer"
            ]
        ]
    ]


unlockableStyles assets =
    [ descendants
        [ Css.Foreign.label
            [ paddingLeft (px (29 + 6)) -- checkbox width + padding
            , backgroundImage assets PremiumKey
            , property "cursor" "auto"
            ]
        ]
    ]


premiumStyles assets =
    [ children
        [ Css.Foreign.label
            [ displayFlex
            , alignItems center
            ]
        , selector "label::after"
            [ property "content" "''"
            , display inlineBlock
            , width (px 26)
            , height (px 24)
            , marginLeft (px 8)
            , backgroundImage assets PremiumFlag
            , backgroundRepeat noRepeat
            , backgroundPosition Css.center
            ]
        ]
    ]


opacifiedStyles =
    [ descendants [ everything [ opacity (num 0.4) ] ] ]


{-| -}
styles : Nri.Ui.Styles.V1.StylesWithAssets Never CssClasses msg (Assets r)
styles =
    (\assets -> [] |> List.concat)
        |> Nri.Ui.Styles.V1.stylesWithAssets "checkbox-"


{-| The assets used in this module.
-}
type alias Assets r =
    { r
        | checkboxUnchecked_svg : Asset
        , checkboxChecked_svg : Asset
        , checkboxCheckedPartially_svg : Asset
        , iconPremiumUnlocked_png : Asset
        , iconCheck_png : Asset
        , iconPremiumLocked_png : Asset
        , checkboxLockOnInside_svg : Asset
        , iconPremiumKey_png : Asset
        , iconPremiumFlag_svg : Asset
    }


backgroundImage : Assets r -> CheckboxImage -> Css.Style
backgroundImage assets checkboxImage =
    property "background-image" (Nri.Ui.AssetPath.Css.url <| checkboxAssetPath assets checkboxImage)


checkboxAssetPath : Assets r -> CheckboxImage -> Asset
checkboxAssetPath assets checkboxImage =
    case checkboxImage of
        CheckboxUnchecked ->
            assets.checkboxUnchecked_svg

        CheckboxChecked ->
            assets.checkboxChecked_svg

        CheckboxCheckedPartially ->
            assets.checkboxCheckedPartially_svg

        PremiumUnlocked ->
            assets.iconPremiumUnlocked_png

        CheckWhite ->
            assets.iconCheck_png

        PremiumLocked ->
            assets.iconPremiumLocked_png

        CheckboxLockOnInside ->
            assets.checkboxLockOnInside_svg

        PremiumKey ->
            assets.iconPremiumKey_png

        PremiumFlag ->
            assets.iconPremiumFlag_svg
