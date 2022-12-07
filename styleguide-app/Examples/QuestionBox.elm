port module Examples.QuestionBox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode
import Json.Encode as Encode
import Markdown
import Nri.Ui.Block.V1 as Block
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Highlightable.V1 as Highlightable
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.QuestionBox.V1 as QuestionBox
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "QuestionBox"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = subscriptions
    , categories = [ Interactions ]
    , keyboardSupport = []
    , preview = []
    , view = view
    }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        attributes =
            Control.currentValue state.attributes
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControls
        , settings = state.attributes
        , mainType = Nothing
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = Code.fromModule moduleName "view"
                  , code =
                        Code.fromModule moduleName "view "
                            ++ Code.list (List.map Tuple.first attributes)
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , viewExamplesTable state
    ]


viewExamplesTable : State -> Html Msg
viewExamplesTable state =
    Table.view
        [ Table.string
            { header = "Pattern"
            , value = .pattern
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "About"
            , view = .description >> Markdown.toHtml Nothing >> List.map fromUnstyled >> span []
            , width = Css.px 50
            , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.pct 75
            , cellStyles = always [ Css.textAlign Css.center ]
            , sort = Nothing
            }
        ]
        [ { pattern = "QuestionBox.viewStandalone"
          , description = "???"
          , example =
                div [ css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ] ]
                    [ QuestionBox.viewStandalone
                        { id = "fake-standalone-id-string"
                        , markdown = "Which words tell you **when** the party is?"
                        , actions =
                            [ { label = "is having", onClick = NoOp }
                            , { label = "after the football game", onClick = NoOp }
                            ]
                        }
                    ]
          }
        , { pattern = "QuestionBox.viewAnchored"
          , description = "???"
          , example =
                div []
                    [ QuestionBox.viewAnchored
                        { id = anchoredExampleId
                        , markdown = "Not quite. Let’s back up a bit."
                        , actions = [ { label = "Show me", onClick = NoOp } ]
                        }
                        state.viewAnchoredExampleMeasurements
                        [ viewHighlighterExample ]
                    , Button.button "Measure & render"
                        [ Button.onClick GetAnchoredExampleMeasurements
                        , Button.css [ Css.marginBottom Spacing.verticalSpacerPx ]
                        ]
                    ]
          }
        , { pattern = "QuestionBox.viewPointingTo"
          , description = "???"
          , example =
                [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                , [ QuestionBox.viewPointingTo
                        (Block.view
                            [ Block.plaintext "above his TV"
                            , Block.label "where"
                            , Block.yellow
                            ]
                        )
                        { id = "view-pointing-to"
                        , markdown = "Which word is the preposition?"
                        , actions =
                            [ { label = "above", onClick = NoOp }
                            , { label = "his", onClick = NoOp }
                            , { label = "TV", onClick = NoOp }
                            ]
                        }
                  ]
                , Block.view [ Block.plaintext "." ]
                ]
                    |> List.concat
                    |> p [ css [ Css.marginTop (Css.px 50) ] ]
          }
        ]


viewHighlighterExample : Html msg
viewHighlighterExample =
    Highlighter.static
        { id = highlighterExampleId
        , highlightables =
            [ ( "Spongebob", Nothing )
            , ( "has", Nothing )
            , ( "a", Nothing )
            , ( "beautiful"
              , Just
                    (Tool.buildMarker
                        { highlightColor = Colors.highlightYellow
                        , hoverColor = Colors.highlightYellow
                        , hoverHighlightColor = Colors.highlightYellow
                        , kind = ()
                        , name = Nothing
                        }
                    )
              )
            , ( "plant", Nothing )
            , ( "above", Nothing )
            , ( "his", Nothing )
            , ( "TV.", Nothing )
            ]
                |> List.intersperse ( " ", Nothing )
                |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
        }


highlighterExampleId : String
highlighterExampleId =
    "question-box-anchored-highlighter-example"


anchoredExampleId : String
anchoredExampleId =
    "question-box-anchored-with-offset-example"


{-| -}
init : State
init =
    { attributes = initAttributes
    , viewAnchoredExampleMeasurements = QuestionBox.initAnchoredBoxState
    }


{-| -}
type alias State =
    { attributes : Control (List ( String, () ))
    , viewAnchoredExampleMeasurements : QuestionBox.AnchoredBoxMeasurementState
    }


initAttributes : Control (List ( String, () ))
initAttributes =
    ControlExtra.list


{-| -}
type Msg
    = UpdateControls (Control (List ( String, () )))
    | NoOp
    | GetAnchoredExampleMeasurements
    | GotAnchoredExampleMeasurements QuestionBox.Measurements


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls configuration ->
            ( { state | attributes = configuration }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )

        GetAnchoredExampleMeasurements ->
            ( state
            , getAnchoredExampleMeasurements
                (Encode.object
                    [ ( "questionBoxId"
                      , Encode.string (QuestionBox.containerId anchoredExampleId)
                      )
                    , ( "containerId"
                      , Encode.string highlighterExampleId
                      )
                    ]
                )
            )

        GotAnchoredExampleMeasurements measurements ->
            ( { state | viewAnchoredExampleMeasurements = QuestionBox.updateAnchoredBoxState measurements }
            , Cmd.none
            )


port getAnchoredExampleMeasurements : Encode.Value -> Cmd msg


port gotAnchoredExampleMeasurements : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions state =
    gotAnchoredExampleMeasurements
        (QuestionBox.decodeMeasurements
            >> Result.map GotAnchoredExampleMeasurements
            >> Result.withDefault NoOp
        )
