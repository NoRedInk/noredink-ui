module Examples.Spacing exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing


moduleName : String
moduleName =
    "Spacing"


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
    , preview = []
    , view = view
    }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        settings =
            Control.currentValue state.settings

        ( exampleCode, exampleView ) =
            container
                (List.filterMap identity
                    [ settings.topContainerStyle
                    , settings.horizontalContainerStyle
                    , settings.bottomContainerStyle
                    ]
                )
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControl
        , settings = state.settings
        , renderExample = Code.unstyledView
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , toExampleCode =
            \_ ->
                [ { sectionName = "Example"
                  , code = exampleCode
                  }
                ]
        }
    , Heading.h2 [ Heading.plaintext "Example" ]
    , fakePage [ exampleView ]
    ]


fakePage : List (Html msg) -> Html msg
fakePage =
    div
        [ css
            [ Css.border3 (Css.px 4) Css.dotted Colors.gray20
            , Css.backgroundColor Colors.gray96
            ]
        ]


container : List ( String, Css.Style ) -> ( String, Html msg )
container styles =
    ( [ "div [ css " ++ Code.listMultiline (List.map Tuple.first styles) 2
      , "]"
      , "[ Container.view [ Container.paragraph \"Content...\" ]"
      ]
        |> String.join (Code.newlineWithIndent 1)
    , div [ css (List.map Tuple.second styles) ]
        [ Container.view [ Container.paragraph "Content..." ]
        ]
    )


{-| -}
type alias State =
    { settings : Control Settings }


init : State
init =
    { settings = controlSettings }


type alias Settings =
    { topContainerStyle : Maybe ( String, Style )
    , horizontalContainerStyle : Maybe ( String, Style )
    , bottomContainerStyle : Maybe ( String, Style )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "pageTopWhitespace"
            (Control.maybe True
                (Control.value
                    ( Code.fromModule moduleName "pageTopWhitespace"
                    , Spacing.pageTopWhitespace
                    )
                )
            )
        |> Control.field "Horizontal container styling"
            (Control.maybe True
                ([ ( "centeredContentWithSidePadding", Spacing.centeredContentWithSidePadding )
                    |> asChoice
                 , ( "centeredContent", Spacing.centeredContent )
                    |> asChoice
                 , ( "centeredQuizEngineContentWithSidePadding", Spacing.centeredQuizEngineContentWithSidePadding )
                    |> asChoice
                 , ( "quizEngineCenteredContent", Spacing.quizEngineCenteredContent )
                    |> asChoice
                 , ( "centeredContentWithSidePaddingAndCustomWidth"
                   , Control.map
                        (\value ->
                            ( Code.fromModule moduleName "centeredContentWithSidePaddingAndCustomWidth"
                                ++ " (Css.px "
                                ++ String.fromFloat value
                                ++ ")"
                            , Spacing.centeredContentWithSidePaddingAndCustomWidth (Css.px value)
                            )
                        )
                        (ControlExtra.float 400)
                   )
                 , ( "centeredContentWithCustomWidth"
                   , Control.map
                        (\value ->
                            ( Code.fromModule moduleName "centeredContentWithCustomWidth"
                                ++ " (Css.px "
                                ++ String.fromFloat value
                                ++ ")"
                            , Spacing.centeredContentWithCustomWidth (Css.px value)
                            )
                        )
                        (ControlExtra.float 400)
                   )
                 ]
                    |> Control.choice
                )
            )
        |> Control.field "pageBottomWhitespace"
            (Control.maybe True
                (Control.value
                    ( Code.fromModule moduleName "pageBottomWhitespace"
                    , Spacing.pageBottomWhitespace
                    )
                )
            )


asChoice : ( String, Style ) -> ( String, Control ( String, Style ) )
asChoice ( name, value ) =
    ( name, Control.value ( Code.fromModule moduleName name, value ) )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        UpdateControl settings ->
            ( { model | settings = settings }
            , Cmd.none
            )
