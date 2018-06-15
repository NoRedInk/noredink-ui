module Spec.Nri.Ui.Checkbox.V3 exposing (spec)

import Nri.Ui.Checkbox.V3 as Checkbox exposing (IsSelected(..))
import Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


premiumView config =
    Checkbox.premium
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
        |> Query.fromHtml


spec : Test
spec =
    describe "Nri.Ui.Checkbox.V1"
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
                        |> Query.has [ Selector.class "checkbox-Checked" ]
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
                        |> Query.has [ Selector.class "checkbox-Unchecked" ]
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
                        |> Query.has [ Selector.class "checkbox-Indeterminate" ]
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
                        |> Query.has [ Selector.class "checkbox-LockOnInsideClass" ]
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
                        |> Query.hasNot [ Selector.class "checkbox-LockOnInsideClass" ]
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
                        |> Query.has [ Selector.class "checkbox-PremiumClass" ]
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
                        |> Query.hasNot [ Selector.class "checkbox-PremiumClass" ]
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
                        |> Query.has [ Selector.class "checkbox-PremiumClass" ]
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
                        |> Query.hasNot [ Selector.class "checkbox-PremiumClass" ]
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
