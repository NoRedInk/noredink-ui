module Nri.Ui.PremiumCheckbox.V1 exposing (PremiumConfig, premium)

{-|

@docs PremiumConfig, premium

-}

import Accessibility.Styled as Html
import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Checkbox.V3 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))


{-|

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
type alias PremiumConfig msg =
    { label : String
    , id : String
    , selected : Checkbox.IsSelected
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

        theme =
            if isLocked then
                Checkbox.LockOnInside
            else if config.contentPremiumLevel /= Free then
                Checkbox.Premium
            else
                Checkbox.Square Checkbox.Default
    in
    Html.span []
        [ Checkbox.viewWithLabel assets
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
        , if
            (isLocked && config.showFlagWhenLocked)
                || (not isLocked && config.contentPremiumLevel /= Free)
          then
            Html.div
                [ Attributes.class "checkbox-PremiumClass"
                , css
                    [ property "content" "''"
                    , display inlineBlock
                    , width (px 26)
                    , height (px 24)
                    , marginLeft (px 8)
                    , backgroundImage assets.iconPremiumFlag_svg
                    , backgroundRepeat noRepeat
                    , backgroundPosition Css.center
                    ]
                ]
                []
          else
            Html.text ""
        ]


{-| The assets used in this module.
-}
type alias Assets r =
    { r
        | checkboxUnchecked_svg : Asset
        , checkboxChecked_svg : Asset
        , checkboxCheckedPartially_svg : Asset
        , checkboxLockOnInside_svg : Asset
        , iconPremiumFlag_svg : Asset
    }


backgroundImage : Asset -> Style
backgroundImage =
    Nri.Ui.AssetPath.Css.url
        >> property "background-image"
