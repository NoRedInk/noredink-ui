module Examples.Carousel exposing
    ( example
    , State
    , Msg
    )

{-|

@docs example
@docs State, msg

-}

import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (fromUnstyled)
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Carousel.V1 as Carousel exposing (Tab, TabPosition(..))
import Nri.Ui.Colors.V1 as Colors
import Task


type Id
    = First
    | Second
    | Third
    | Fourth


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
    { tabListPosition : Carousel.TabPosition }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "tabListPosition"
            (Control.choice
                [ ( "Before", Control.value Before )
                , ( "After", Control.value After )
                ]
            )


type Msg
    = FocusAndSelectTab { select : Id, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        FocusAndSelectTab { select, focus } ->
            ( { model | selected = select }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        Focused error ->
            ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Cmd.none )


moduleName : String
moduleName =
    "Carousel"


version : Int
version =
    1


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Move focus to the currently-selected Tab's tab panel"
          }
        , { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Select the tab to the left of the currently-selected Tab"
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Select the tab to the right of the currently-selected Tab"
          }
        ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        []
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = "RootHtml.Html String"
                , extraImports = []
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Tab-list Before example"
                          , code = "TODO"
                          }
                        , { sectionName = "Tab-list After example"
                          , code = "TODO"
                          }
                        ]
                }
            , Carousel.view
                { focusAndSelect = FocusAndSelectTab
                , selected = model.selected
                , tabListStyles = tabListStyles
                , tabStyles = tabStyles
                , containerStyles = containerStyles
                , tabListPosition = settings.tabListPosition
                , tabs = allTabs
                }
            ]
    }


containerStyles : List Style
containerStyles =
    [ Css.margin (Css.px 20) ]


tabListStyles : List Style
tabListStyles =
    [ Css.displayFlex
    , Css.property "gap" "20px"
    ]


tabStyles : Int -> Bool -> List Css.Style
tabStyles _ isSelected =
    let
        ( backgroundColor, textColor ) =
            if isSelected then
                ( Colors.azure, Colors.white )

            else
                ( Colors.gray92, Colors.gray20 )
    in
    [ Css.padding2 (Css.px 10) (Css.px 20)
    , Css.backgroundColor backgroundColor
    , Css.borderRadius (Css.px 8)
    , Css.border Css.zero
    , Css.color textColor
    , Css.cursor Css.pointer
    ]


slideStyles : List Style
slideStyles =
    [ Css.marginTop (Css.px 20)
    , Css.padding2 (Css.px 10) (Css.px 30)
    , Css.borderRadius (Css.px 4)
    ]


allTabs : List (Tab Id Msg)
allTabs =
    [ ( First, "first", "1" )
    , ( Second, "second", "2" )
    , ( Third, "third", "3" )
    , ( Fourth, "fourth", "4" )
    ]
        |> List.map
            (\( id, idString, buttonText ) ->
                Carousel.buildTab { id = id, idString = idString ++ "-slide" }
                    [ Carousel.tabHtml (Html.text buttonText)
                    , Carousel.slideHtml (Html.div [ Attributes.css slideStyles ] [ Html.text <| idString ++ " slide" ])
                    ]
            )
