module Nri.Ui.PremiumCheckbox.V1 exposing (PremiumConfig, premium)

{-|

@docs PremiumConfig, premium

-}

import Accessibility.Styled as Html
import Nri.Ui.AssetPath exposing (Asset(..))
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
    Checkbox.viewWithLabel assets
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
