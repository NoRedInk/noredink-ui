module Examples.Balloon exposing (example, State, Msg)

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
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (Html, text)
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Colors.V1 as Colors


moduleName : String
moduleName =
    "Balloon"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Balloon.view
            [ Balloon.onTop
            , Balloon.navy
            , Balloon.paddingPx 15
            , Balloon.plaintext "This is a balloon."
            ]
        ]
    , view = view
    }


{-| -}
type alias State =
    Control Settings


init : State
init =
    controlSettings


type alias Settings =
    List ( String, Balloon.Attribute Msg )


controlSettings : Control Settings
controlSettings =
    ControlExtra.list
        |> ControlExtra.listItem "content"
            (CommonControls.content
                { moduleName = moduleName
                , plaintext = Balloon.plaintext
                , markdown = Just Balloon.markdown
                , html = Balloon.html
                , httpError = Nothing
                }
            )
        |> ControlExtra.optionalListItem "theme" themeOptions
        |> ControlExtra.optionalListItem "position" positionOptions
        |> ControlExtra.optionalListItem "padding" paddingOptions
        |> CommonControls.css_ "containerCss"
            ( "[ Css.backgroundColor Colors.magenta ]", [ Css.backgroundColor Colors.magenta ] )
            { moduleName = moduleName, use = Balloon.containerCss }
        |> CommonControls.css { moduleName = moduleName, use = Balloon.css }
        |> CommonControls.mobileCss { moduleName = moduleName, use = Balloon.mobileCss }
        |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Balloon.quizEngineMobileCss }
        |> CommonControls.notMobileCss { moduleName = moduleName, use = Balloon.notMobileCss }


themeOptions : Control ( String, Balloon.Attribute msg )
themeOptions =
    Control.choice
        [ ( "green", Control.value ( "Balloon.green", Balloon.green ) )
        , ( "purple", Control.value ( "Balloon.purple", Balloon.purple ) )
        , ( "orange", Control.value ( "Balloon.orange", Balloon.orange ) )
        , ( "white", Control.value ( "Balloon.white", Balloon.white ) )
        , ( "navy", Control.value ( "Balloon.navy", Balloon.navy ) )
        ]


positionOptions : Control ( String, Balloon.Attribute msg )
positionOptions =
    Control.choice
        [ ( "onBottom", Control.value ( "Balloon.onBottom", Balloon.onBottom ) )
        , ( "onLeft", Control.value ( "Balloon.onLeft", Balloon.onLeft ) )
        , ( "onRight", Control.value ( "Balloon.onRight", Balloon.onRight ) )
        , ( "onTop", Control.value ( "Balloon.onTop", Balloon.onTop ) )
        ]


paddingOptions : Control ( String, Balloon.Attribute msg )
paddingOptions =
    Control.map
        (\w -> ( "Balloon.paddingPx " ++ String.fromFloat w, Balloon.paddingPx w ))
        (ControlExtra.float 10)


{-| -}
type Msg
    = SetAttributes (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetAttributes attributes ->
            ( attributes
            , Cmd.none
            )


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        attributes =
            Control.currentValue state
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetAttributes
        , settings = state
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = "Balloon"
                  , code =
                        Code.fromModule moduleName "view"
                            ++ Code.list (List.map Tuple.first attributes)
                  }
                ]
        }
    , Balloon.view (List.map Tuple.second attributes)
    ]
