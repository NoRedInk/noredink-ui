module Examples.Tabs exposing (example)

{-|

@docs example

-}

import Assets exposing (assets)
import Html.Styled as Html
import List.Zipper
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Tabs.V2 as Tabs


type Tab
    = First
    | Second


example : msg -> ModuleExample msg
example noOp =
    { filename = "Nri.Ui.Tabs.V2"
    , category = Behaviors
    , content =
        [ Tabs.view
            { title = Nothing
            , onSelect = \_ -> noOp
            , tabs = List.Zipper.Zipper [ Tabs.Tab "First tab" First ] (Tabs.Tab "Second tab" Second) []
            , content =
                \id ->
                    case id of
                        First ->
                            Html.text "First"

                        Second ->
                            Html.text "Second"
            , alignment = Tabs.Center
            }
        ]
    }
