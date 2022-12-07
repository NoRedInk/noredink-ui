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
import Nri.Ui.Svg.V1
import Nri.Ui.Table.V6 as Table
import Svg.Styled
import Svg.Styled.Attributes


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
        [ { pattern = "QuestionBox.standalone"
          , description = "???"
          , example =
                div [ css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ] ]
                    [ QuestionBox.view
                        [ QuestionBox.standalone
                        , QuestionBox.markdown "Which words tell you **when** the party is?"
                        , QuestionBox.actions
                            [ { label = "is having", onClick = NoOp }
                            , { label = "after the football game", onClick = NoOp }
                            ]
                        , QuestionBox.character { name = "Panda", icon = pandaIcon }
                        ]
                    ]
          }
        , { pattern = "QuestionBox.anchoredTo"
          , description = "???"
          , example =
                div []
                    [ QuestionBox.view
                        [ QuestionBox.anchoredTo [ viewHighlighterExample ] state.viewAnchoredExampleMeasurements
                        , QuestionBox.id anchoredExampleId
                        , QuestionBox.markdown "Not quite. Let’s back up a bit."
                        , QuestionBox.actions [ { label = "Show me", onClick = NoOp } ]
                        , QuestionBox.character { name = "Panda", icon = pandaIcon }
                        ]
                    , Button.button "Measure & render"
                        [ Button.onClick GetAnchoredExampleMeasurements
                        , Button.css [ Css.marginBottom Spacing.verticalSpacerPx ]
                        ]
                    ]
          }
        , { pattern = "QuestionBox.pointingTo"
          , description = "???"
          , example =
                [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                , [ QuestionBox.view
                        [ QuestionBox.pointingTo
                            (Block.view
                                [ Block.plaintext "above his TV"
                                , Block.label "where"
                                , Block.yellow
                                ]
                            )
                        , QuestionBox.markdown "Which word is the preposition?"
                        , QuestionBox.actions
                            [ { label = "above", onClick = NoOp }
                            , { label = "his", onClick = NoOp }
                            , { label = "TV", onClick = NoOp }
                            ]
                        , QuestionBox.character { name = "Panda", icon = pandaIcon }
                        ]
                  ]
                , Block.view [ Block.plaintext "." ]
                ]
                    |> List.concat
                    |> p [ css [ Css.marginTop (Css.px 50) ] ]
          }
        ]


pandaIcon : Nri.Ui.Svg.V1.Svg
pandaIcon =
    Nri.Ui.Svg.V1.init "0 0 10 10"
        [ Svg.Styled.image
            [ Svg.Styled.Attributes.xlinkHref "https://ucf018b22d191439face31e68b6b.previews.dropboxusercontent.com/p/thumb/ABtCu0ijqrgkNHZS6mWw4c6YQwA-qVtF_W5Gxy8wMGJujaEqn2LcA95f0Si7mwoCj30tlZIIHJhbxe4lStQ8tsMDe9gkg96lbSFItuMbfBF_hmOXunQEw8ns1Q9YwaW3FIjpUWY24K7TfqooigX6VZcIE0MMI1tttLB0N0sPssIFJsJ9vm_MZ2QfHQbdMICQWNxCEOBrdbHfrMyd3TW0RrClfSWAjZWLUArn6ZQVPbAfORss-uJINVI2nI5vxNqIpeIPVG9Fh0xR6hTWcgZUzaMd9HfRZeNI4YR3LWFmRGzg9CHQHJwwv9ixK5EHbbEpDot_W_VZHnJdMsSVFdZ0yCK1M8FSVnpKVQNuRQRxBl8xoH43xFqxohpqcPWVc55lE_MNlA5isTV2lKJ1L3nby1J7vrXrGZvHvgjEYlC-EK1UyA/p.png"
            , Svg.Styled.Attributes.x "0"
            , Svg.Styled.Attributes.y "0"
            , Svg.Styled.Attributes.width "10"
            , Svg.Styled.Attributes.height "10"
            ]
            []
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
    "question-box-example-anchored-with-offset-example"


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
                    [ ( "questionBoxId", Encode.string anchoredExampleId )
                    , ( "containerId", Encode.string highlighterExampleId )
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
