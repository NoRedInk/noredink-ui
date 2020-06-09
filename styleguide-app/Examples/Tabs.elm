module Examples.Tabs exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Category exposing (Category(..))
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html exposing (Html, fromUnstyled)
import List.Zipper
import Nri.Ui.Tabs.V5 as Tabs exposing (Alignment(..))


type alias State =
    { selected : Id
    , settings : Control Settings
    }


init : State
init =
    { selected = First
    , settings = initSettings
    }


type alias Settings =
    { title : Maybe String
    , alignment : Alignment
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "title" (Control.maybe False (Control.string "Title"))
        |> Control.field "alignment"
            (Control.choice
                [ ( "Left", Control.value Left )
                , ( "Center", Control.value Center )
                , ( "Right", Control.value Right )
                ]
            )


type Id
    = First
    | Second


type Msg
    = SelectTab Id
    | SetSettings (Control Settings)


update : Msg -> State -> State
update msg model =
    case msg of
        SelectTab id ->
            { model | selected = id }

        SetSettings settings ->
            { model | settings = settings }


example : Example State Msg
example =
    { name = "Nri.Ui.Tabs.V5"
    , categories = [ Layout ]
    , state = init
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \model ->
            let
                settings =
                    Control.currentValue model.settings
            in
            [ Control.view SetSettings model.settings |> fromUnstyled
            , Tabs.view
                { title = settings.title
                , onSelect = SelectTab
                , tabs =
                    case model.selected of
                        First ->
                            List.Zipper.from
                                []
                                (Tabs.Tab "First tab" First)
                                [ Tabs.Tab "Second tab" Second ]

                        Second ->
                            List.Zipper.from
                                [ Tabs.Tab "First tab" First ]
                                (Tabs.Tab "Second tab" Second)
                                []
                , content =
                    \id ->
                        case id of
                            First ->
                                Html.text "First"

                            Second ->
                                Html.text "Second"
                , alignment = settings.alignment
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
                        , Tabs.SpaLink { label = "Spa", href = "/#category/Layout", msg = SelectTab Second }
                        ]
                }
            ]
    }
