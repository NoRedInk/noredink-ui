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
import List.Zipper exposing (Zipper)
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
    | Third
    | Fourth


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
                    List.Zipper.from [] First [ Second, Third, Fourth ]
                        |> List.Zipper.find ((==) model.selected)
                        |> Maybe.withDefault (List.Zipper.from [] First [ Second, Third, Fourth ])
                , idToString = idToString
                , viewTab = viewTab
                , viewPanel = viewPanel
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


idToString : Id -> String
idToString id =
    case id of
        First ->
            "tab-0"

        Second ->
            "tab-1"

        Third ->
            "tab-2"

        Fourth ->
            "tab-3"


viewTab : Id -> Html msg
viewTab id =
    case id of
        First ->
            Html.text "First Tab"

        Second ->
            Html.text "Second Tab"

        Third ->
            Html.text "Third Tab"

        Fourth ->
            Html.text "Fourth Tab"


viewPanel : Id -> Html msg
viewPanel id =
    case id of
        First ->
            Html.text "First"

        Second ->
            Html.text "Second"

        Third ->
            Html.text "Third"

        Fourth ->
            Html.text "Fourth"
