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
import Nri.Ui.Tabs.V4 as Tabs


type Tab
    = First
    | Second


example : (Tab -> msg) -> Tab -> ModuleExample msg
example changeTab tab =
    { name = "Nri.Ui.Tabs.V4"
    , category = Layout
    , content =
        [ Tabs.view
            { title = Nothing
            , onSelect = changeTab
            , tabs =
                case tab of
                    First ->
                        List.Zipper.from []
                            (Tabs.Tab "First tab" First)
                            [ Tabs.Tab "Second tab" Second ]

                    Second ->
                        List.Zipper.from []
                            (Tabs.Tab "Second tab" Second)
                            [ Tabs.Tab "First tab" First ]
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
                List.Zipper.from
                    []
                    (Tabs.NormalLink { label = "Nowhere", href = Nothing })
                    [ Tabs.NormalLink { label = "Elm", href = Just "http://elm-lang.org" }
                    , Tabs.SpaLink { label = "Spa", href = "/#category/Layout", msg = changeTab Second }
                    ]
            }
        ]
    }
