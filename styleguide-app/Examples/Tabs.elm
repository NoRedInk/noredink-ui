module Examples.Tabs exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
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
import Task


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
    , customSpacing : Maybe Float
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
        |> Control.field "customSpacing"
            (Control.maybe False
                (Control.choice
                    [ ( "2", Control.value 2 )
                    , ( "3", Control.value 3 )
                    , ( "4", Control.value 4 )
                    , ( "8", Control.value 8 )
                    , ( "16", Control.value 16 )
                    ]
                )
            )


type Id
    = First
    | Second
    | Third
    | Fourth


type Msg
    = SelectTab Id
    | Focus String
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        SelectTab id ->
            ( { model | selected = id }, Cmd.none )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused error ->
            ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Cmd.none )


example : Example State Msg
example =
    { name = "Nri.Ui.Tabs.V5"
    , categories = [ Layout ]
    , atomicDesignType = Molecule
    , state = init
    , update = update
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
                , customSpacing = settings.customSpacing
                , onSelect = SelectTab
                , onFocus = Focus
                , selected = model.selected
                , tabs = allTabs
                }
            ]
    }


allTabs : List (Tab Id Msg)
allTabs =
    [ { id = First
      , idString = "tab-0"
      , spaHref = Just "/#/doodad/Nri.Ui.Tabs.V5"
      , tabView = Tabs.viewTabDefault "Link example"
      , panelView = Html.text "First Panel"
      }
    , { id = Second
      , idString = "tab-1"
      , spaHref = Nothing
      , tabView = Tabs.viewTabDefault "Second Tab"
      , panelView = Html.text "Second Panel"
      }
    , { id = Third
      , idString = "tab-2"
      , spaHref = Nothing
      , tabView =
            UiIcon.bulb
                |> Svg.withLabel "Lightbulb"
                |> Svg.withWidth (Css.px 40)
                |> Svg.withHeight (Css.px 40)
                |> Svg.withCss [ Css.padding2 Css.zero (Css.px 6) ]
                |> Svg.toHtml
      , panelView = Html.text "Third Panel"
      }
    , { id = Fourth
      , idString = "tab-3"
      , spaHref = Nothing
      , tabView = Tabs.viewTabDefault "Fourth Tab"
      , panelView = Html.text "Fourth Panel"
      }
    ]
