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
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Svg.Styled
import Svg.Styled.Attributes


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
    , preview = preview
    , about = []
    , view = view
    }


preview : List (Html msg)
preview =
    [ Svg.Styled.svg
        [ Svg.Styled.Attributes.viewBox "0 0 100 100"
        ]
        [ Svg.Styled.rect
            [ Svg.Styled.Attributes.width "100"
            , Svg.Styled.Attributes.height "100"
            , Svg.Styled.Attributes.fill Colors.white.value
            ]
            []
        , Svg.Styled.rect
            [ Svg.Styled.Attributes.x "15"
            , Svg.Styled.Attributes.y "30"
            , Svg.Styled.Attributes.width "70"
            , Svg.Styled.Attributes.height "20"
            , Svg.Styled.Attributes.fill Colors.gray96.value
            ]
            []
        , Svg.Styled.text_
            [ Svg.Styled.Attributes.fill Colors.gray20.value
            , Svg.Styled.Attributes.css [ Fonts.baseFont, Css.fontSize (Css.px 8) ]
            , Svg.Styled.Attributes.x "20"
            , Svg.Styled.Attributes.y "43"
            ]
            [ Svg.Styled.text "Content" ]
        , -- Top red line indicator
          Svg.Styled.g []
            [ Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "50"
                , Svg.Styled.Attributes.x2 "50"
                , Svg.Styled.Attributes.y1 "1"
                , Svg.Styled.Attributes.y2 "29"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "45"
                , Svg.Styled.Attributes.x2 "55"
                , Svg.Styled.Attributes.y1 "1"
                , Svg.Styled.Attributes.y2 "1"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "45"
                , Svg.Styled.Attributes.x2 "55"
                , Svg.Styled.Attributes.y1 "29"
                , Svg.Styled.Attributes.y2 "29"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.text_
                [ Svg.Styled.Attributes.fill Colors.red.value
                , Svg.Styled.Attributes.css [ Fonts.baseFont, Css.fontSize (Css.px 8) ]
                , Svg.Styled.Attributes.x "53"
                , Svg.Styled.Attributes.y "20"
                ]
                [ Svg.Styled.text "30px" ]
            ]
        , -- Bottom red line indicator
          Svg.Styled.g []
            [ Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "50"
                , Svg.Styled.Attributes.x2 "50"
                , Svg.Styled.Attributes.y1 "51"
                , Svg.Styled.Attributes.y2 "99"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "45"
                , Svg.Styled.Attributes.x2 "55"
                , Svg.Styled.Attributes.y1 "51"
                , Svg.Styled.Attributes.y2 "51"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "45"
                , Svg.Styled.Attributes.x2 "55"
                , Svg.Styled.Attributes.y1 "99"
                , Svg.Styled.Attributes.y2 "99"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.text_
                [ Svg.Styled.Attributes.fill Colors.red.value
                , Svg.Styled.Attributes.css [ Fonts.baseFont, Css.fontSize (Css.px 8) ]
                , Svg.Styled.Attributes.x "53"
                , Svg.Styled.Attributes.y "75"
                ]
                [ Svg.Styled.text "50px" ]
            ]
        , -- Right red line indicator
          Svg.Styled.g []
            [ Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "86"
                , Svg.Styled.Attributes.x2 "99"
                , Svg.Styled.Attributes.y1 "40"
                , Svg.Styled.Attributes.y2 "40"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "86"
                , Svg.Styled.Attributes.x2 "86"
                , Svg.Styled.Attributes.y1 "38"
                , Svg.Styled.Attributes.y2 "42"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "99"
                , Svg.Styled.Attributes.x2 "99"
                , Svg.Styled.Attributes.y1 "38"
                , Svg.Styled.Attributes.y2 "42"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            ]
        , -- Left red line indicator
          Svg.Styled.g []
            [ Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "1"
                , Svg.Styled.Attributes.x2 "14"
                , Svg.Styled.Attributes.y1 "40"
                , Svg.Styled.Attributes.y2 "40"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "1"
                , Svg.Styled.Attributes.x2 "1"
                , Svg.Styled.Attributes.y1 "38"
                , Svg.Styled.Attributes.y2 "42"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            , Svg.Styled.line
                [ Svg.Styled.Attributes.x1 "14"
                , Svg.Styled.Attributes.x2 "14"
                , Svg.Styled.Attributes.y1 "38"
                , Svg.Styled.Attributes.y2 "42"
                , Svg.Styled.Attributes.stroke Colors.red.value
                ]
                []
            ]
        ]
    ]


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
    , Heading.h2 [ Heading.plaintext "Example", Heading.css [ Css.marginTop Spacing.verticalSpacerPx ] ]
    , fakePage [ exampleView ]
    , Heading.h2 [ Heading.plaintext "Content alignment", Heading.css [ Css.marginTop Spacing.verticalSpacerPx ] ]
    , Table.view []
        [ Table.string
            { header = "Name"
            , value = .name
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.string
            { header = "Content alignment"
            , value = .alignment
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.string
            { header = "Content max-width"
            , value = .maxWidth
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.string
            { header = "Side padding"
            , value = .sidePadding
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        ]
        [ { name = "centeredContentWithSidePadding", alignment = "Centered", maxWidth = "1000px", sidePadding = "when viewport <= 970px" }
        , { name = "centeredContent", alignment = "Centered", maxWidth = "1000px", sidePadding = "0px" }
        , { name = "centeredQuizEngineContentWithSidePadding", alignment = "Centered", maxWidth = "750px", sidePadding = "when viewport <= 720px" }
        , { name = "centeredQuizEngineContent", alignment = "Centered", maxWidth = "750px", sidePadding = "0px" }
        , { name = "centeredContentWithSidePaddingAndCustomWidth", alignment = "Centered", maxWidth = "(customizable)", sidePadding = "when viewport <= (custom breakpoint value - 30)" }
        , { name = "centeredContentWithCustomWidth", alignment = "Centered", maxWidth = "(customizable)", sidePadding = "0px" }
        ]
    , Heading.h2 [ Heading.plaintext "Constants", Heading.css [ Css.marginTop Spacing.verticalSpacerPx ] ]
    , Table.view []
        [ Table.string
            { header = "Name"
            , value = .name
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.string
            { header = "Value"
            , value = .value
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        ]
        [ { name = "pageTopWhitespacePx", value = "30px" }
        , { name = "pageBottomWhitespacePx", value = "50px" }
        , { name = "pageSideWhitespacePx", value = "15px" }
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
