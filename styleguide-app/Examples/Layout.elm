module Examples.Layout exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Code
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Layout.V1 as Layout
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "Layout"


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
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControl
        , settings = state.settings
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , toExampleCode = \_ -> []
        }
    , Heading.h2 [ Heading.plaintext "Example" ]
    , fakePage
        [ container
            (List.filterMap identity
                [ Maybe.map Tuple.second settings.topContainerStyle
                , Maybe.map Tuple.second settings.horizontalContainerStyle
                , Maybe.map Tuple.second settings.bottomContainerStyle
                ]
            )
            (List.repeat settings.childCount child)
        ]
    ]


fakePage : List (Html msg) -> Html msg
fakePage =
    div
        [ css
            [ Css.border3 (Css.px 4) Css.dotted Colors.gray20
            , Css.backgroundColor Colors.gray96
            ]
        ]


container : List Css.Style -> List (Html msg) -> Html msg
container styles =
    div
        [ css
            [ Css.border3 (Css.px 2) Css.dashed Colors.greenDarkest
            , Css.backgroundColor Colors.greenLightest
            , Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.batch styles
            ]
        ]


child : Html msg
child =
    div
        [ css
            [ Css.border3 (Css.px 1) Css.solid Colors.ochreDark
            , Css.backgroundColor Colors.sunshine
            , Css.flexBasis (Css.px 150)
            , Css.height (Css.px 150)
            ]
        ]
        []


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
    , childCount : Int
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "pageTopWhitespace"
            (Control.maybe True
                (Control.value
                    ( Code.fromModule moduleName "pageTopWhitespace"
                    , Layout.pageTopWhitespace
                    )
                )
            )
        |> Control.field "Horizontal container styling"
            (Control.maybe True
                ([ ( "centeredContentWithSidePadding", Layout.centeredContentWithSidePadding )
                    |> asChoice
                 , ( "centeredContent", Layout.centeredContent )
                    |> asChoice
                 , ( "centeredQuizEngineContentWithSidePadding", Layout.centeredQuizEngineContentWithSidePadding )
                    |> asChoice
                 , ( "quizEngineCenteredContent", Layout.quizEngineCenteredContent )
                    |> asChoice
                 , ( "centeredContentWithSidePaddingAndCustomWidth"
                   , Control.map
                        (\value ->
                            ( Code.fromModule moduleName "centeredContentWithSidePaddingAndCustomWidth"
                                ++ " (Css.px "
                                ++ String.fromFloat value
                                ++ ")"
                            , Layout.centeredContentWithSidePaddingAndCustomWidth (Css.px value)
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
                            , Layout.centeredContentWithCustomWidth (Css.px value)
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
                    , Layout.pageBottomWhitespace
                    )
                )
            )
        |> Control.field "Child count" (ControlExtra.int 3)


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
