module Examples.Tabs exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Html.Styled as Html
import List.Zipper
import Nri.Ui.Tabs.V4 as Tabs


type State
    = First
    | Second


type alias Msg =
    State


example : Example State Msg
example =
    { name = "Nri.Ui.Tabs.V4"
    , categories = [ Layout ]
    , state = First
    , update = \newTab oldTab -> ( newTab, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \tab ->
            [ Tabs.view
                { title = Nothing
                , onSelect = identity
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
                        , Tabs.SpaLink { label = "Spa", href = "/#category/Layout", msg = Second }
                        ]
                }
            ]
    }
