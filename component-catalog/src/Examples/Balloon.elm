module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls exposing (quickBrownFox)
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Examples.Colors
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table


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
    , about = Guidance.useATACGuide moduleName
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
        |> ControlExtra.listItems "Theme"
            (Control.list
                |> ControlExtra.optionalListItem "theme" themeOptions
            )
        |> ControlExtra.listItems "CSS"
            (Control.list
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
        (( "customTheme", controlCustomTheme )
            :: List.map
                (\( name, prop ) ->
                    ( name, Control.value ( Code.fromModule moduleName name, prop ) )
                )
                builtInThemes
        )


builtInThemes : List ( String, Balloon.Attribute msg )
builtInThemes =
    [ ( "green", Balloon.green )
    , ( "purple", Balloon.purple )
    , ( "orange", Balloon.orange )
    , ( "white", Balloon.white )
    , ( "navy", Balloon.navy )
    ]


controlCustomTheme : Control ( String, Balloon.Attribute msg )
controlCustomTheme =
    Control.record
        (\( backgroundName, backgroundColor ) ( colorName, color ) ->
            ( "Balloon.customTheme { backgroundColor = Colors." ++ backgroundName ++ ", color = " ++ colorName ++ " }"
            , Balloon.customTheme { backgroundColor = backgroundColor, color = color }
            )
        )
        |> Control.field "backgroundColor" controlBackgroundColor
        |> Control.field "color" controlColor


controlBackgroundColor : Control ( String, Css.Color )
controlBackgroundColor =
    Examples.Colors.backgroundHighlightColors
        |> List.map
            (\( name, value, _ ) ->
                ( name
                , Control.value ( name, value )
                )
            )
        |> ControlExtra.rotatedChoice 0


controlColor : Control ( String, Css.Color )
controlColor =
    CommonControls.choice "Colors"
        [ ( "gray20", Colors.gray20 )
        , ( "navy", Colors.navy )
        , ( "white", Colors.white )
        ]


positionOptions : Control ( String, Balloon.Attribute msg )
positionOptions =
    CommonControls.choice moduleName positions


positions : List ( String, Balloon.Attribute msg )
positions =
    [ ( "onTop", Balloon.onTop )
    , ( "onRight", Balloon.onRight )
    , ( "onBottom", Balloon.onBottom )
    , ( "onLeft", Balloon.onLeft )
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
    , div
        [ css
            [ Css.minHeight (Css.px 200)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]
        ]
        [ Balloon.view (List.map Tuple.second attributes) ]
    , Heading.h2
        [ Heading.plaintext "Position & Alignment Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Table.view []
        [ Table.custom
            { header = text "Position"
            , view = Tuple.first >> text
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "alignArrowStart"
            , view =
                \( _, position ) ->
                    Balloon.view
                        [ Balloon.plaintext quickBrownFox
                        , position
                        , Balloon.alignArrowStart
                        ]
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "alignArrowMiddle (default)"
            , view =
                \( _, position ) ->
                    Balloon.view
                        [ Balloon.plaintext quickBrownFox
                        , position
                        , Balloon.alignArrowMiddle
                        ]
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "alignArrowEnd"
            , view =
                \( _, position ) ->
                    Balloon.view
                        [ Balloon.plaintext quickBrownFox
                        , position
                        , Balloon.alignArrowEnd
                        ]
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        ]
        positions
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]
    , Heading.h2
        [ Heading.plaintext "Theme Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Table.view []
        [ Table.custom
            { header = text "Theme"
            , view = Tuple.first >> text
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = \( _, theme ) -> Balloon.view [ theme, Balloon.plaintext "A b c…" ]
            , width = Css.pct 50
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        ]
        builtInThemes
    ]
