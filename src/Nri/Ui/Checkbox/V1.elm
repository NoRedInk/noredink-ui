module Nri.Ui.Checkbox.V1 exposing
    ( Model, Theme(..), ColorTheme(..)
    , view, viewWithLabel, viewAttention, disabled
    , IsSelected(..)
    , keyframeCss, styles
    , PremiumConfig, premium
    )

{-|

@docs Model, Theme, ColorTheme

@docs view, viewWithLabel, viewAttention, disabled
@docs IsSelected
@docs keyframeCss, styles


## Premium

@docs PremiumConfig, premium

-}

import Accessibility exposing (..)
import Accessibility.Aria exposing (controls)
import Accessibility.Style
import Accessibility.Widget as Widget
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Nri.Ui.Styles.V1 exposing (Keyframe, StylesWithAssets)
import Html
import Html.Attributes as Attributes
import Html.Events as Events exposing (defaultOptions)
import Json.Decode
import Json.Encode
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.Extra as Attributes
import Nri.Ui.Html.Extra exposing (onEnter, onKeyUp)


{-|

  - isChecked : Maybe Bool
      - Just True == Checked (rendered checkmark)
      - Just False == Not Checked (rendered blank)
      - Nothing == Indeterminate (rendered dash)

-}
type alias Model msg =
    { identifier : String
    , label : String
    , setterMsg : Bool -> msg
    , isChecked : Maybe Bool
    , disabled : Bool
    , theme : Theme
    , noOpMsg : msg
    }


customView : List CssClasses -> Bool -> Model msg -> Html msg
customView modifierClasses showLabels model =
    let
        containerClasses =
            List.concat
                [ [ Container ]
                , modifierClasses
                , case model.theme of
                    Square Gray ->
                        [ SquareClass, GrayClass ]

                    Square Orange ->
                        [ SquareClass, OrangeClass ]

                    Square Default ->
                        [ SquareClass ]

                    Locked ->
                        [ LockedClass ]

                    LockOnInside ->
                        [ LockOnInsideClass ]

                    Unlockable ->
                        [ UnlockableClass ]

                    Round _ ->
                        [ RoundClass ]

                    Disabled ->
                        [ SquareClass, Opacified ]

                    Premium ->
                        [ SquareClass, PremiumClass ]
                ]
    in
    Html.span
        [ styles.class containerClasses
        , Attributes.id <| model.identifier ++ "-container"
        , -- This is necessary to prevent event propagation.
          -- See https://github.com/elm-lang/html/issues/96
          Attributes.map (always model.noOpMsg) <|
            Events.onWithOptions "click"
                { defaultOptions | stopPropagation = True }
                (Json.Decode.succeed "stop click propagation")
        ]
        [ checkbox model.identifier
            model.isChecked
            [ Widget.label model.label
            , styles.class [ Checkbox ]
            , Events.onCheck model.setterMsg
            , Attributes.id model.identifier
            , Attributes.disabled model.disabled
            ]
        , Html.label
            [ Attributes.for model.identifier
            , getLabelClass model.isChecked
            , controls model.identifier
            , Widget.disabled model.disabled
            , Widget.checked model.isChecked
            , if not model.disabled then
                Attributes.tabindex 0

              else
                Attributes.none
            , if not model.disabled then
                Nri.Ui.Html.Extra.onKeyUp
                    { defaultOptions | preventDefault = True }
                    (\keyCode ->
                        -- 32 is the space bar, 13 is enter
                        if (keyCode == 32 || keyCode == 13) && not model.disabled then
                            Just <| model.setterMsg (Maybe.map not model.isChecked |> Maybe.withDefault True)

                        else
                            Nothing
                    )

              else
                Attributes.none
            ]
            [ span
                (if showLabels then
                    []

                 else
                    [ Accessibility.Style.invisible ]
                )
                [ Html.text model.label ]
            ]
        ]


{-| Shows a checkbox (the label is only used for accessibility hints)
-}
view : Model msg -> Html msg
view model =
    customView [] False model


{-| Shows a checkbox and its label text
-}
viewWithLabel : Model msg -> Html msg
viewWithLabel model =
    customView [] True model


{-| Show a disabled checkbox.
-}
disabled : String -> String -> Html msg
disabled identifier labelText =
    span
        [ styles.class [ Container, SquareClass, Opacified ]
        , Attributes.id <| identifier ++ "-container"
        ]
        [ checkbox identifier
            (Just False)
            [ Widget.label labelText
            , styles.class [ Checkbox ]
            , Attributes.id identifier
            , Attributes.disabled True
            ]
        , label
            [ Attributes.for identifier
            , getLabelClass (Just False)
            ]
            [ Html.text labelText
            ]
        ]


{-| -}
type IsSelected
    = Selected
    | NotSelected
    | PartiallySelected


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
premium : PremiumConfig msg -> Html msg
premium config =
    let
        isLocked =
            not <|
                PremiumLevel.allowedFor
                    config.contentPremiumLevel
                    config.teacherPremiumLevel

        isChecked =
            case config.selected of
                Selected ->
                    Just True

                NotSelected ->
                    Just False

                PartiallySelected ->
                    Nothing

        modifierClasses =
            List.concat
                [ if config.showFlagWhenLocked && config.contentPremiumLevel /= Free then
                    [ PremiumClass ]

                  else
                    []
                , if config.disabled then
                    [ Opacified ]

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
    customView modifierClasses
        True
        { identifier = config.id
        , label = config.label
        , setterMsg =
            if isLocked then
                \_ -> config.onLockedClick

            else
                config.onChange
        , isChecked = isChecked
        , disabled = config.disabled
        , theme = theme
        , noOpMsg = config.noOpMsg
        }


{-| -}
viewAttention : Model msg -> Html msg
viewAttention model =
    customView [ WithPulsing ] False model


getLabelClass : Maybe Bool -> Html.Attribute msg
getLabelClass maybeChecked =
    styles.class
        [ Label
        , case maybeChecked of
            Just True ->
                Checked

            Just False ->
                Unchecked

            Nothing ->
                Indeterminate
        ]


indeterminateAttr : Html.Attribute msg
indeterminateAttr =
    Attributes.property "indeterminate" (Json.Encode.bool True)


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
    | WithPulsing
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
    | Round Bool
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


mainSnippet : List Snippet
mainSnippet =
    [ Css.Foreign.class Container
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
    , Css.Foreign.class Checkbox
        [ cursor pointer ]
    , Css.Foreign.class Label
        [ cursor pointer
        , outline none
        ]
    ]


square : Assets r -> List Snippet
square assets =
    [ Css.Foreign.class SquareClass
        [ children
            [ Css.Foreign.label
                [ paddingLeft (px (29 + 6)) -- checkbox width + padding
                ]
            , Css.Foreign.class Unchecked [ backgroundImage assets CheckboxUnchecked ]
            , Css.Foreign.class Checked [ backgroundImage assets CheckboxChecked ]
            , Css.Foreign.class Indeterminate [ backgroundImage assets CheckboxCheckedPartially ]
            ]
        ]
    ]


gray : List Snippet
gray =
    [ Css.Foreign.class GrayClass
        [ children
            [ Css.Foreign.label [ color Colors.gray45 ] ]
        ]
    ]


orange : Assets r -> List Snippet
orange assets =
    [ Css.Foreign.class OrangeClass
        [ children
            [ Css.Foreign.label
                [ color Colors.ochre
                , displayFlex
                , alignItems center
                ]
            , selector "label::after"
                [ property "content" "''"
                , width (px 26)
                , height (px 24)
                , marginLeft (px 8)
                , backgroundImage assets PremiumUnlocked
                ]
            ]
        ]
    ]


round : Assets r -> List Snippet
round assets =
    [ Css.Foreign.class RoundClass
        [ children
            [ Css.Foreign.label
                [ displayFlex
                , alignItems center
                , property "cursor" "pointer"
                ]
            , selector "label::before"
                [ property "content" "''"
                , width (px 24)
                , height (px 24)
                , marginRight (px 8)
                , borderRadius (pct 100)
                ]
            , selector ".checkbox-Unchecked::before"
                [ border3 (px 2) solid Colors.blue
                , backgroundColor Colors.white
                ]
            , selector ".checkbox-Checked::before"
                [ backgroundColor Colors.green
                , border3 (px 2) solid Colors.green
                , backgroundImage assets CheckWhite
                , property "background-repeat" "no-repeat"
                , property "background-position" "center center"
                ]
            , selector ":disabled + label"
                [ property "cursor" "auto"
                ]
            ]
        ]
    , Css.Foreign.class WithPulsing
        [ property "-webkit-animation" "pulsate 1s infinite"
        , property "-moz-animation" "pulsate 1s infinite"
        , property "animation" "pulsate 1s infinite"
        ]
    ]


locked : Assets r -> List Snippet
locked assets =
    [ Css.Foreign.class LockedClass
        [ descendants
            [ Css.Foreign.label
                [ paddingLeft (px (29 + 6)) -- checkbox width + padding
                , backgroundImage assets PremiumLocked
                , property "cursor" "auto"
                ]
            ]
        ]
    ]


lockOnInside : Assets r -> List Snippet
lockOnInside assets =
    [ Css.Foreign.class LockOnInsideClass
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
    ]


unlockable : Assets r -> List Snippet
unlockable assets =
    [ Css.Foreign.class UnlockableClass
        [ descendants
            [ Css.Foreign.label
                [ paddingLeft (px (29 + 6)) -- checkbox width + padding
                , backgroundImage assets PremiumKey
                , property "cursor" "auto"
                ]
            ]
        ]
    ]


premiumStyles : Assets r -> List Snippet
premiumStyles assets =
    [ Css.Foreign.class PremiumClass
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
    ]


opacified : List Snippet
opacified =
    [ Css.Foreign.class Opacified
        [ descendants [ everything [ opacity (num 0.4) ] ] ]
    ]


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


{-| -}
keyframeCss : Keyframe
keyframeCss =
    DEPRECATED.Nri.Ui.Styles.V1.keyframes "pulsate"
        [ ( "0%", "transform: scale(1, 1);" )
        , ( "50%", "transform: scale(1.2);" )
        , ( "100%", "transform: scale(1, 1);" )
        ]


{-| -}
styles : StylesWithAssets Never CssClasses msg (Assets r)
styles =
    (\assets ->
        [ mainSnippet
        , square assets
        , gray
        , orange assets
        , round assets
        , locked assets
        , lockOnInside assets
        , unlockable assets
        , opacified
        , premiumStyles assets
        ]
            |> List.concat
    )
        |> DEPRECATED.Nri.Ui.Styles.V1.stylesWithAssets "checkbox-"


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
