module Examples.Panel exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Panel.V1 as Panel


moduleName : String
moduleName =
    "Panel"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ panelPreview Colors.navy
        , panelPreview Colors.gray45
        ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.control

                attributes =
                    List.map Tuple.second settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.list (List.map Tuple.first settings)
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Panel.view attributes
            ]
    }


panelPreview : Css.Color -> Html msg
panelPreview headingColor =
    div [ css [ Css.marginTop (Css.px 16), Css.firstChild [ Css.marginTop Css.zero ] ] ]
        [ div
            [ css
                [ Css.backgroundColor headingColor
                , Css.borderTopLeftRadius (Css.px 8)
                , Css.borderTopRightRadius (Css.px 8)
                , Css.width (Css.pct 100)
                , Css.minHeight (Css.px 20)
                , Css.fontSize (Css.px 12)
                , Css.color Colors.white
                , Fonts.baseFont
                , Css.padding2 (Css.px 2) (Css.px 8)
                ]
            ]
            [ text "Panel name" ]
        , div
            [ css
                [ Css.backgroundColor Colors.white
                , Css.width (Css.pct 100)
                , Css.minHeight (Css.px 30)
                ]
            ]
            []
        ]


{-| -}
type alias State =
    { control : Control (Settings Msg)
    }


init : State
init =
    let
        controlStyles name f styles =
            CommonControls.css_ name
                styles
                { moduleName = moduleName, use = f }
    in
    { control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "theme"
                (CommonControls.choice moduleName
                    [ ( "secondary", Panel.secondary )
                    , ( "primary", Panel.primary )
                    ]
                )
            |> ControlExtra.listItem "header"
                (Control.map
                    (\v ->
                        ( Code.fromModule moduleName "header " ++ Code.string v
                        , Panel.header v
                        )
                    )
                    (Control.string "Header")
                )
            |> ControlExtra.listItem "content"
                (CommonControls.content
                    { moduleName = moduleName
                    , paragraph = Just Panel.paragraph
                    , plaintext = Panel.plaintext
                    , markdown = Just Panel.markdown
                    , html = Panel.html
                    , httpError = Nothing
                    }
                )
            |> controlStyles "containerCss"
                Panel.containerCss
                ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
                , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
                )
            |> controlStyles "headerCss"
                Panel.headerCss
                ( "[ Css.border3 (Css.px 4) Css.solid Colors.aqua ]"
                , [ Css.border3 (Css.px 4) Css.solid Colors.aqua ]
                )
            |> controlStyles "css"
                Panel.css
                ( "[ Css.border3 (Css.px 4) Css.dotted Colors.orange ]"
                , [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]
                )
    }


type alias Settings msg =
    List ( String, Panel.Attribute msg )


{-| -}
type Msg
    = UpdateControl (Control (Settings Msg))


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
