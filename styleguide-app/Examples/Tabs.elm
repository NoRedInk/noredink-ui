module Examples.Tabs exposing
    ( example
    , Tab(..)
    )

{-|

@docs example

-}

import Html.Styled as Html
import List.Zipper
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Tabs.V3 as Tabs


type Tab
    = First
    | Second


example : (Tab -> msg) -> Tab -> ModuleExample msg
example changeTab tab =
    { name = "Nri.Ui.Tabs.V3"
    , category = Widgets
    , content =
        [ Tabs.view
            { title = Nothing
            , onSelect = changeTab
            , tabs =
                case tab of
                    First ->
                        List.Zipper.Zipper [] (Tabs.Tab "First tab" First) [ Tabs.Tab "Second tab" Second ]

                    Second ->
                        List.Zipper.Zipper [ Tabs.Tab "First tab" First ] (Tabs.Tab "Second tab" Second) []
            , content =
                \id ->
                    case id of
                        First ->
                            Html.text "First"

                        Second ->
                            Html.text "Second"
            , alignment = Tabs.Center
            }
        , Tabs.links
            { title = Nothing
            , content = Html.text "Links"
            , alignment = Tabs.Left
            , tabs =
                List.Zipper.Zipper [] (Tabs.TabLink "Nowhere" Nothing) [ Tabs.TabLink "Elm" (Just "http://elm-lang.org") ]
            }
        ]
    }
