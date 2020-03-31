module Spec.Nri.Ui.PremiumCheckbox.V6 exposing (spec)

import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Checkbox.V5 as Checkbox exposing (IsSelected(..))
import Nri.Ui.PremiumCheckbox.V6 as PremiumCheckbox
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type Msg
    = OnLocked
    | OnChange Bool


premiumView config =
    PremiumCheckbox.view
        { label = "i am label"
        , id = "id"
        , selected = config.selected
        , disabled = config.disabled
        , isLocked = config.isLocked
        , isPremium = config.isPremium
        , onChange = OnChange
        , onLockedClick = OnLocked
        }
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


spec : Test
spec =
    describe "Nri.Ui.PremiumCheckbox.V6"
        [ describe "premium"
            [ test "displays the label" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.has [ Selector.text "i am label" ]
            , test "appears selected when Selected is passed in" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.has [ Selector.attribute (Attributes.checked True) ]
            , test "appears unselected when NotSelected is passed in" <|
                \() ->
                    premiumView
                        { selected = NotSelected
                        , disabled = False
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.has [ Selector.attribute (Attributes.checked False) ]
            , test "triggers onLockedClick when isLocked = True" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = True
                        , isPremium = False
                        }
                        |> Query.find [ Selector.tag "input" ]
                        |> Event.simulate (Event.check False)
                        |> Event.expect OnLocked
            , test "triggers onChange when isLocked = False" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.find [ Selector.tag "input" ]
                        |> Event.simulate (Event.check False)
                        |> Event.expect (OnChange False)
            , test "appears with P flag when Premium pennant is passed in" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , isPremium = True
                        }
                        |> Query.has [ Selector.attribute (Attributes.attribute "aria-label" "Premium") ]
            , test "is not disabled when disabled = False" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = False
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.has [ Selector.disabled False ]
            , test "is disabled when disabled = True" <|
                \() ->
                    premiumView
                        { selected = Selected
                        , disabled = True
                        , isLocked = False
                        , isPremium = False
                        }
                        |> Query.has [ Selector.disabled True ]
            ]
        ]
