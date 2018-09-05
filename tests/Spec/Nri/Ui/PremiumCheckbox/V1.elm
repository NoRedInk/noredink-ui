module Spec.Nri.Ui.PremiumCheckbox.V1 exposing (spec)

import Html.Attributes
import Html.Styled
import Nri.Ui.AssetPath exposing (Asset(Asset))
import Nri.Ui.Checkbox.V3 exposing (IsSelected(..))
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V1 as PremiumCheckbox
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


premiumView config =
    PremiumCheckbox.premium assets
        { label = "i am label"
        , id = "id"
        , selected = config.selected
        , disabled = config.disabled
        , isLocked = config.isLocked
        , pennant = config.pennant
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
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.text "i am label" ]
            , test "appears selected when Selected is passed in" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Checked" ]
            , test "appears unselected when NotSelected is passed in" <|
                \() ->
                    premiumView
                        { selected = NotSelected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Unchecked" ]
            , test "appears partially selected when PartiallySelected is passed in" <|
                \() ->
                    premiumView
                        { selected = PartiallySelected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Indeterminate" ]
            , test "appears locked when isLocked = True" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = True
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.class "checkbox-V3__Locked" ]
            , test "appears unlocked when isLocked = False" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.hasNot [ Selector.class "checkbox-V3__Locked" ]
            , test "appears with P flag when Premium pennant is passed in" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Just PremiumCheckbox.Premium
                        }
                        |> Query.find [ Selector.tag "style" ]
                        |> Query.has [ Selector.text "iconPremiumFlag reference" ]
            , test "appears with P+ flag when Premium pennant is passed in" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Just PremiumCheckbox.PremiumWithWriting
                        }
                        |> Query.find [ Selector.tag "style" ]
                        |> Query.has [ Selector.text "iconPremiumWritingFlag reference" ]
            , test "is not disabled when disabled = False" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , pennant = Nothing
                        }
                        |> Query.has [ Selector.disabled False ]
            , test "is disabled when disabled = True" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = True
                        , isLocked = False
                        , pennant = Nothing
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
    , iconPremiumWithWritingFlag_svg = Asset "iconPremiumWritingFlag reference"
    }
