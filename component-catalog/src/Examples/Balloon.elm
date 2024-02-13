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
import Examples.Colors
import Guidance
import Html.Styled exposing (Html)
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing


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
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Balloon.view
            [ Balloon.onTop
            , Balloon.navy
            , Balloon.plaintext "This is a balloon."
            ]
        ]
    , about = [ Guidance.useATACGuide moduleName ]
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
    Control.list
        |> ControlExtra.listItem "content"
            (CommonControls.content
                { moduleName = moduleName
                , paragraph = Just Balloon.paragraph
                , plaintext = Balloon.plaintext
                , markdown = Just Balloon.markdown
                , html = Balloon.html
                , httpError = Nothing
                }
            )
        |> ControlExtra.listItems "Arrow & Balloon Direction"
            (Control.list
                |> ControlExtra.optionalListItemDefaultChecked "position" positionOptions
                |> ControlExtra.optionalListItem "arrowAlignment" arrowAlignmentOptions
                |> ControlExtra.optionalListItem "arrowHeight"
                    (Control.map
                        (\v ->
                            ( "Balloon.arrowHeight " ++ String.fromFloat v ++ ""
                            , Balloon.arrowHeight v
                            )
                        )
                        (Control.float 20)
                    )
            )
        |> ControlExtra.listItems "Theme & Custom CSS"
            (Control.list
                |> ControlExtra.optionalListItem "theme" themeOptions
                |> CommonControls.css_ "containerCss"
                    ( "[ Css.backgroundColor Colors.magenta ]", [ Css.backgroundColor Colors.magenta ] )
                    { moduleName = moduleName, use = Balloon.containerCss }
                |> CommonControls.css { moduleName = moduleName, use = Balloon.css }
                |> CommonControls.mobileCss { moduleName = moduleName, use = Balloon.mobileCss }
                |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Balloon.quizEngineMobileCss }
                |> CommonControls.notMobileCss { moduleName = moduleName, use = Balloon.notMobileCss }
            )


themeOptions : Control ( String, Balloon.Attribute msg )
themeOptions =
    Control.choice
        [ ( "green", Control.value ( "Balloon.green", Balloon.green ) )
        , ( "purple", Control.value ( "Balloon.purple", Balloon.purple ) )
        , ( "orange", Control.value ( "Balloon.orange", Balloon.orange ) )
        , ( "white", Control.value ( "Balloon.white", Balloon.white ) )
        , ( "navy", Control.value ( "Balloon.navy", Balloon.navy ) )
        , ( "customTheme", controlCustomTheme )
        ]


controlCustomTheme : Control ( String, Balloon.Attribute msg )
controlCustomTheme =
    Examples.Colors.backgroundHighlightColors
        |> List.map
            (\( name, value, _ ) ->
                ( name
                , Control.value
                    ( "Balloon.customTheme { backgroundColor = Colors." ++ name ++ ", color = Colors.gray20 }"
                    , Balloon.customTheme { backgroundColor = value, color = Colors.gray20 }
                    )
                )
            )
        |> ControlExtra.rotatedChoice 0


positionOptions : Control ( String, Balloon.Attribute msg )
positionOptions =
    Control.choice
        [ ( "onTop", Control.value ( "Balloon.onTop", Balloon.onTop ) )
        , ( "onRight", Control.value ( "Balloon.onRight", Balloon.onRight ) )
        , ( "onBottom", Control.value ( "Balloon.onBottom", Balloon.onBottom ) )
        , ( "onLeft", Control.value ( "Balloon.onLeft", Balloon.onLeft ) )
        ]


arrowAlignmentOptions : Control ( String, Balloon.Attribute msg )
arrowAlignmentOptions =
    Control.choice
        [ ( "alignArrowStart", Control.value ( "Balloon.alignArrowStart", Balloon.alignArrowStart ) )
        , ( "alignArrowMiddle", Control.value ( "Balloon.alignArrowMiddle", Balloon.alignArrowMiddle ) )
        , ( "alignArrowEnd", Control.value ( "Balloon.alignArrowEnd", Balloon.alignArrowEnd ) )
        ]


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
                        Code.fromModule moduleName "view "
                            ++ Code.list (List.map Tuple.first attributes)
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable Example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Balloon.view (List.map Tuple.second attributes)
    ]
