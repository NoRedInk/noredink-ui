module Spec.Nri.Ui.PremiumCheckbox.V1 exposing (spec)

import Html.Styled
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Checkbox.V3 exposing (IsSelected(..))
import Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V1 as PremiumCheckbox
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


premiumView config =
    PremiumCheckbox.premium assets
        { label = config.label
        , id = "id"
        , selected = config.selected
        , disabled = config.disabled
        , teacherPremiumLevel = config.teacherPremiumLevel
        , contentPremiumLevel = config.contentPremiumLevel
        , showFlagWhenLocked = config.showFlagWhenLocked
        , onChange = \_ -> ()
        , onLockedClick = ()
        , noOpMsg = ()
        }
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


spec : Test
spec =
    describe "Nri.Ui.PremiumCheckbox.V1"
        [ describe "premium"
            [ test "displays the label" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = Selected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.text "i am label" ]
            , test "appears selected when Selected is passed in" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = Selected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Checked" ]
            , test "appears unselected when NotSelected is passed in" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = NotSelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Unchecked" ]
            , test "appears partially selected when PartiallySelected is passed in" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Indeterminate" ]
            , test "appears locked when teacherPremiumLevel < contentPremiumLevel" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Premium
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Locked" ]
            , test "appears unlocked when teacherPremiumLevel >= contentPremiumLevel" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Premium
                        , contentPremiumLevel = Premium
                        , showFlagWhenLocked = True
                        }
                        |> Query.hasNot [ Selector.class "checkbox-V3__Locked" ]
            , test "appears with P flag when teacherPremiumLevel >= contentPremiumLevel" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Premium
                        , contentPremiumLevel = Premium
                        , showFlagWhenLocked = False
                        }
                        |> Query.has [ Selector.class "premium-checkbox-V1__PremiumClass" ]
            , test "does not appear with P flag when teacherPremiumLevel < contentPremiumLevel and showFlagWhenLocked = False" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Premium
                        , showFlagWhenLocked = False
                        }
                        |> Query.hasNot [ Selector.class "premium-checkbox-V1__PremiumClass" ]
            , test "appears with P flag for Premium content when teacherPremiumLevel < contentPremiumLevel and showFlagWhenLocked = True" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Premium
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.class "premium-checkbox-V1__PremiumClass" ]
            , test "never shows P flag for nonPremium content" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.hasNot [ Selector.class "premium-checkbox-V1__PremiumClass" ]
            , test "is not disabled when disabled = False and the checkbox is unlocked" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = False
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.disabled False ]
            , test "is disabled when disabled = True and the checkbox is unlocked" <|
                \() ->
                    premiumView
                        { label = "i am label"
                        , selected = PartiallySelected
                        , disabled = True
                        , teacherPremiumLevel = Free
                        , contentPremiumLevel = Free
                        , showFlagWhenLocked = True
                        }
                        |> Query.has [ Selector.disabled True ]
            ]
        ]


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
