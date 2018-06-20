module Spec.Nri.Ui.Checkbox.V3 exposing (spec)

import Html.Styled
import Nri.Ui.AssetPath exposing (Asset(Asset))
import Nri.Ui.Checkbox.V3 as Checkbox exposing (IsSelected(..))
import Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Checkbox.V1"
        []


assets =
    { checkboxUnchecked_svg = Asset "checkboxUnchecked reference"
    , checkboxChecked_svg = Asset "checkboxChecked reference"
    , checkboxCheckedPartially_svg = Asset "checkboxCheckedPartially reference"
    , iconPremiumUnlocked_png = Asset "iconPremiumUnlocked reference"
    , iconCheck_png = Asset "iconCheck reference"
    , iconPremiumLocked_png = Asset "iconPremiumLocked reference"
    , checkboxLockOnInside_svg = Asset "checkboxLockOnInside reference"
    , iconPremiumKey_png = Asset "iconPremiumKey reference"
    , iconPremiumFlag_svg = Asset "iconPremiumFlag reference"
    }
