module Examples.Tabs exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import List.Zipper exposing (Zipper)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Tabs.V5 as Tabs exposing (Alignment(..), Tab)
import Nri.Ui.UiIcon.V1 as UiIcon


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
                , alignment = settings.alignment
                , onSelect = SelectTab
                , selected = model.selected
                , tabs = allTabs
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


allTabs : List (Tab Id Msg)
allTabs =
    [ { id = First
      , idString = "tab-0"
      , tabView = Tabs.viewTabDefault "First Tab"
      , panelView = Html.text "First Panel"
      }
    , { id = Second
      , idString = "tab-1"
      , tabView = Tabs.viewTabDefault "Second Tab"
      , panelView = Html.text "Second Panel"
      }
    , { id = Third
      , idString = "tab-2"
      , tabView =
            UiIcon.bulb
                |> Svg.withWidth (Css.px 40)
                |> Svg.withHeight (Css.px 40)
                |> Svg.withCss [ Css.padding2 Css.zero (Css.px 6) ]
                |> Svg.toHtml
      , panelView = Html.text "Third Panel"
      }
    , { id = Fourth
      , idString = "tab-3"
      , tabView = Tabs.viewTabDefault "Fourth Tab"
      , panelView = Html.text "Fourth Panel"
      }
    ]
