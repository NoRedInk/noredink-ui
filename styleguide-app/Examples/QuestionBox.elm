module Examples.QuestionBox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Browser.Dom as Dom exposing (Element)
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Markdown
import Nri.Ui.Block.V2 as Block
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.QuestionBox.V2 as QuestionBox
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V6 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


moduleName : String
moduleName =
    "QuestionBox"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Interactions ]
    , keyboardSupport = []
    , preview = [ QuestionBox.view [ QuestionBox.markdown "Is good?" ] ]
    , view = view
    }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        attributes =
            ( Code.fromModule moduleName "id " ++ Code.string interactiveExampleId
            , QuestionBox.id interactiveExampleId
            )
                :: Control.currentValue state.attributes
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
        [ Heading.plaintext "Interactive example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.flexStart
            , Css.minHeight (Css.px 350)
            ]
        ]
        [ QuestionBox.view (List.map Tuple.second attributes) ]
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Button.button "Measure & render"
        [ Button.onClick GetMeasurements
        , Button.small
        , Button.secondary
        , Button.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Text.caption
        [ Text.plaintext "Click \"Measure & render\" to reposition the noninteractive examples' labels and question boxes to avoid overlaps given the current viewport."
        , Text.css [ Css.marginBottom Spacing.verticalSpacerPx |> Css.important ]
        ]
    , viewExamplesTable state
    ]


viewExamplesTable : State -> Html Msg
viewExamplesTable state =
    let
        offsets =
            Block.getLabelPositions state.labelMeasurementsById

        getBottomSpacingFor id =
            Dict.get id state.questionBoxMeasurementsById
                |> Maybe.map (.element >> .height)
    in
    Table.view
        [ Table.string
            { header = "QuestionBox type"
            , value = .pattern
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.string
            { header = "Block pattern"
            , value = .shownWith
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.pct 50
            , cellStyles = always []
            , sort = Nothing
            }
        ]
        [ { pattern = "standalone"
          , shownWith = ""
          , example =
                div [ css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ] ]
                    [ QuestionBox.view
                        [ QuestionBox.standalone
                        , QuestionBox.markdown "Which words tell you **when** the party is?"
                        , QuestionBox.actions
                            [ { label = "is having", onClick = NoOp }
                            , { label = "after the football game", onClick = NoOp }
                            ]
                        ]
                    ]
          }
        , { pattern = "pointingTo"
          , shownWith = "Emphasis block"
          , example =
                inParagraph
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , [ QuestionBox.view
                            [ QuestionBox.id "question-box-1"
                            , QuestionBox.pointingTo
                                (Block.view
                                    [ Block.plaintext "above"
                                    , Block.emphasize
                                    , Block.bottomSpacingPx (getBottomSpacingFor "question-box-1")
                                    ]
                                )
                            , QuestionBox.markdown "“Above” is a preposition."
                            , QuestionBox.actions [ { label = "Try again", onClick = NoOp } ]
                            ]
                      ]
                    , Block.view [ Block.plaintext " his TV." ]
                    ]
          }
        , { pattern = "pointingTo"
          , shownWith = "Label block"
          , example =
                inParagraph
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , [ QuestionBox.view
                            [ QuestionBox.id "question-box-2"
                            , QuestionBox.pointingTo
                                (Block.view
                                    [ Block.plaintext "above his TV"
                                    , Block.label "where"
                                    , Block.labelId "label-1"
                                    , Block.labelPosition (Dict.get "label-1" offsets)
                                    , Block.bottomSpacingPx (getBottomSpacingFor "question-box-2")
                                    , Block.yellow
                                    ]
                                )
                            , QuestionBox.markdown "Which word is the preposition?"
                            , QuestionBox.actions
                                [ { label = "above", onClick = NoOp }
                                , { label = "his", onClick = NoOp }
                                , { label = "TV", onClick = NoOp }
                                ]
                            ]
                      ]
                    , Block.view [ Block.plaintext "." ]
                    ]
          }
        , { pattern = "pointingTo"
          , shownWith = "Blank block"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.plaintext "Superman"
                        , Block.label "subject"
                        , Block.labelId "label-2"
                        , Block.labelPosition (Dict.get "label-2" offsets)
                        ]
                    , Block.view [ Block.plaintext " " ]
                    , [ QuestionBox.view
                            [ QuestionBox.id "question-box-3"
                            , QuestionBox.pointingTo
                                (Block.view
                                    [ Block.bottomSpacingPx (getBottomSpacingFor "question-box-3")
                                    ]
                                )
                            , QuestionBox.markdown "Which verb matches the subject?"
                            , QuestionBox.actions
                                [ { label = "wrap", onClick = NoOp }
                                , { label = "wraps", onClick = NoOp }
                                ]
                            ]
                      ]
                    , Block.view [ Block.plaintext " gifts with comic book pages." ]
                    ]
          }
        , { pattern = "pointingTo"
          , shownWith = "Labelled blank block"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.plaintext "Dave"
                        , Block.label "subject"
                        , Block.labelId "label-3"
                        , Block.labelPosition (Dict.get "label-3" offsets)
                        , Block.yellow
                        ]
                    , Block.view [ Block.plaintext " " ]
                    , [ QuestionBox.view
                            [ QuestionBox.id "question-box-4"
                            , QuestionBox.pointingTo
                                (Block.view
                                    [ Block.label "verb"
                                    , Block.labelId "label-4"
                                    , Block.labelPosition (Dict.get "label-4" offsets)
                                    , Block.bottomSpacingPx (getBottomSpacingFor "question-box-4")
                                    , Block.cyan
                                    ]
                                )
                            , QuestionBox.markdown "What did he do?"
                            , QuestionBox.actions
                                [ { label = "scared", onClick = NoOp }
                                , { label = "scarred", onClick = NoOp }
                                , { label = "scarified", onClick = NoOp }
                                ]
                            ]
                      ]
                    , Block.view [ Block.plaintext " his replacement cousin coming out of his room wearing a gorilla mask." ]
                    ]
          }
        , { pattern = "pointingTo"
          , shownWith = "Blank with emphasis block"
          , example =
                inParagraph
                    [ [ QuestionBox.view
                            [ QuestionBox.id "question-box-5"
                            , QuestionBox.pointingTo
                                (Block.view
                                    [ Block.label "verb"
                                    , Block.labelId "label-5"
                                    , Block.labelPosition (Dict.get "label-5" offsets)
                                    , Block.cyan
                                    , Block.content (Block.phrase "Dave " ++ [ Block.blank ])
                                    , Block.bottomSpacingPx (getBottomSpacingFor "question-box-5")
                                    ]
                                )
                            , QuestionBox.markdown "What did he do?"
                            , QuestionBox.actions
                                [ { label = "scared", onClick = NoOp }
                                , { label = "scarred", onClick = NoOp }
                                , { label = "scarified", onClick = NoOp }
                                ]
                            ]
                      ]
                    , Block.view [ Block.plaintext " his replacement cousin coming out of his room wearing a gorilla mask." ]
                    ]
          }
        ]


inParagraph : List (List (Html msg)) -> Html msg
inParagraph =
    List.concat
        >> p
            [ css
                [ Css.margin2 Spacing.verticalSpacerPx Css.zero
                , Fonts.quizFont
                , Css.fontSize (Css.px 30)
                ]
            ]


interactiveExampleId : String
interactiveExampleId =
    "interactive-example-question-box"


anchorId : String
anchorId =
    "interactive-example-anchor-icon"


{-| -}
init : State
init =
    { attributes = initAttributes
    , labelMeasurementsById = Dict.empty
    , questionBoxMeasurementsById = Dict.empty
    }


{-| -}
type alias State =
    { attributes : Control (List ( String, QuestionBox.Attribute Msg ))
    , labelMeasurementsById :
        Dict
            String
            { label : Element
            , labelContent : Element
            }
    , questionBoxMeasurementsById : Dict String Element
    }


initAttributes : Control (List ( String, QuestionBox.Attribute Msg ))
initAttributes =
    ControlExtra.list
        |> ControlExtra.listItem "markdown"
            (Control.map
                (\str ->
                    ( Code.fromModule moduleName "markdown " ++ Code.string str
                    , QuestionBox.markdown str
                    )
                )
                (Control.stringTextarea initialMarkdown)
            )
        |> ControlExtra.listItem "actions"
            (Control.map
                (\i ->
                    let
                        ( code, actions ) =
                            List.range 1 i
                                |> List.map
                                    (\i_ ->
                                        ( Code.record
                                            [ ( "label", Code.string ("Button " ++ String.fromInt i_) )
                                            , ( "onClick", "NoOp" )
                                            ]
                                        , { label = "Button " ++ String.fromInt i_, onClick = NoOp }
                                        )
                                    )
                                |> List.unzip
                    in
                    ( Code.fromModule moduleName "actions " ++ Code.list code
                    , QuestionBox.actions actions
                    )
                )
                (ControlExtra.int 2)
            )
        |> ControlExtra.optionalListItem "character"
            ([ { name = "(none)", icon = ( "Nothing", Nothing ) }
             , { name = "Gumby"
               , icon =
                    ( "<| Just (Svg.withColor Colors.mustard UiIcon.stretch)"
                    , Just (Svg.withColor Colors.mustard UiIcon.stretch)
                    )
               }
             ]
                |> List.map
                    (\{ name, icon } ->
                        ( name
                        , Control.value
                            ( "QuestionBox.character " ++ Tuple.first icon
                            , QuestionBox.character
                                (Maybe.map (\i -> { name = name, icon = i })
                                    (Tuple.second icon)
                                )
                            )
                        )
                    )
                |> Control.choice
            )
        |> ControlExtra.optionalListItem "type"
            (CommonControls.choice moduleName
                [ ( "pointingTo []", QuestionBox.pointingTo [ anchor ] )
                , ( "standalone", QuestionBox.standalone )
                ]
            )
        |> CommonControls.css_ "containerCss"
            ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
            , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
            )
            { moduleName = moduleName, use = QuestionBox.containerCss }


anchor : Html msg
anchor =
    div [ Attributes.id anchorId ]
        [ UiIcon.sortArrowDown
            |> Svg.withLabel "Anchor"
            |> Svg.withWidth (Css.px 30)
            |> Svg.withHeight (Css.px 30)
            |> Svg.toHtml
        ]


initialMarkdown : String
initialMarkdown =
    """Let’s remember the rules:

- Always capitalize the first word of a title
- Capitalize all words except small words like “and,” “of” and “an.”

**How should this be capitalized?**
"""


{-| -}
type Msg
    = UpdateControls (Control (List ( String, QuestionBox.Attribute Msg )))
    | NoOp
    | GetMeasurements
    | GotBlockLabelMeasurements
        String
        (Result
            Dom.Error
            { label : Element
            , labelContent : Element
            }
        )
    | GotQuestionBoxMeasurements String (Result Dom.Error Element)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls configuration ->
            ( { state | attributes = configuration }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )

        GetMeasurements ->
            ( state
            , Cmd.batch
                (List.map measureBlockLabel
                    [ "label-1"
                    , "label-2"
                    , "label-3"
                    , "label-4"
                    , "label-5"
                    ]
                    ++ List.map measureQuestionBox
                        [ "question-box-1"
                        , "question-box-2"
                        , "question-box-3"
                        , "question-box-4"
                        , "question-box-5"
                        ]
                )
            )

        GotBlockLabelMeasurements id (Ok measurement) ->
            ( { state | labelMeasurementsById = Dict.insert id measurement state.labelMeasurementsById }
            , Cmd.none
            )

        GotBlockLabelMeasurements _ (Err _) ->
            ( state
            , -- in a real application, log an error
              Cmd.none
            )

        GotQuestionBoxMeasurements id (Ok measurement) ->
            ( { state | questionBoxMeasurementsById = Dict.insert id measurement state.questionBoxMeasurementsById }
            , Cmd.none
            )

        GotQuestionBoxMeasurements _ (Err _) ->
            ( state
            , -- in a real application, log an error
              Cmd.none
            )


measureBlockLabel : String -> Cmd Msg
measureBlockLabel labelId =
    Task.map2 (\label labelContent -> { label = label, labelContent = labelContent })
        (Dom.getElement labelId)
        (Dom.getElement (Block.labelContentId labelId))
        |> Task.attempt (GotBlockLabelMeasurements labelId)


measureQuestionBox : String -> Cmd Msg
measureQuestionBox id =
    Task.attempt (GotQuestionBoxMeasurements id) (Dom.getElement id)
