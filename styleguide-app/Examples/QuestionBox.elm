module Examples.QuestionBox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Browser.Dom as Dom exposing (Element)
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
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

        offsets =
            Block.getLabelPositions state.labelMeasurementsById

        getBottomSpacingFor id =
            Dict.get id state.questionBoxMeasurementsById
                |> Maybe.map (.element >> .height >> (+) 8)
    in
    [ -- absolutely positioned elements that overflow in the x direction
      -- cause a horizontal scrollbar unless you explicitly hide overflowing x content
      Css.Global.global [ Css.Global.selector "body" [ Css.overflowX Css.hidden ] ]
    , ControlView.view
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
    , div [ css [ Css.minHeight (Css.px 350) ] ]
        [ div
            [ css
                [ Css.position Css.relative
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]
            [ UiIcon.sortArrowDown
                |> Svg.withLabel "Anchor"
                |> Svg.withWidth (Css.px 30)
                |> Svg.withHeight (Css.px 30)
                |> Svg.toHtml
            , QuestionBox.view (List.map Tuple.second attributes)
            ]
        ]
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
        ]
    , Heading.h3
        [ Heading.plaintext "QuestionBox.standalone"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , QuestionBox.view
        [ QuestionBox.standalone
        , QuestionBox.markdown "Which words tell you **when** the party is?"
        , QuestionBox.actions
            [ { label = "is having", onClick = NoOp }
            , { label = "after the football game", onClick = NoOp }
            ]
        ]
    , Heading.h3
        [ Heading.plaintext "QuestionBox.pointingTo"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , inParagraph
        [ Block.view
            [ Block.plaintext "A"
            , Block.magenta
            , Block.label "this is an article. \"An\" is also an article."
            , Block.labelId "article-label-id"
            , Block.labelPosition (Dict.get "article-label-id" offsets)
            , Block.bottomSpacingPx (getBottomSpacingFor "left-viewport-question-box-example")
            , Block.withQuestionBox
                [ QuestionBox.id "left-viewport-question-box-example"
                , QuestionBox.pointingTo (Dict.get "left-viewport-question-box-example" state.questionBoxMeasurementsById)
                , QuestionBox.markdown "Articles are important to get right! Is “the” an article?"
                , QuestionBox.actions
                    [ { label = "Yes", onClick = NoOp }
                    , { label = "No", onClick = NoOp }
                    ]
                ]
            ]
        , Block.view [ Block.plaintext " " ]
        , Block.view
            [ Block.plaintext "tricky"
            , Block.label "this is an adjective"
            , Block.yellow
            , Block.labelId "tricky-label-id"
            , Block.labelPosition (Dict.get "tricky-label-id" offsets)
            ]
        , Block.view [ Block.plaintext " " ]
        , Block.view
            [ Block.plaintext "example"
            , Block.cyan
            ]
        , Block.view [ Block.plaintext ". Be sure to be " ]
        , Block.view
            [ Block.plaintext "careful with content that can get cut off on the left side of the viewport!"
            , Block.label "warning"
            , Block.green
            , Block.labelId "warning-label-id"
            , Block.labelPosition (Dict.get "warning-label-id" offsets)
            ]
        ]
    , inParagraph
        [ Block.view [ Block.plaintext "You also need to be careful with content that can get cut off on the right side of the viewport" ]
        , Block.view
            [ Block.plaintext "!"
            , Block.label "warning"
            , Block.brown
            , Block.labelId "warning-2-label-id"
            , Block.labelPosition (Dict.get "warning-2-label-id" offsets)
            , Block.bottomSpacingPx (getBottomSpacingFor "right-viewport-question-box-example")
            , Block.withQuestionBox
                [ QuestionBox.id "right-viewport-question-box-example"
                , QuestionBox.pointingTo (Dict.get "right-viewport-question-box-example" state.questionBoxMeasurementsById)
                , QuestionBox.markdown "Were you careful? It's important to be careful!"
                , QuestionBox.actions
                    [ { label = "Yes", onClick = NoOp }
                    , { label = "No", onClick = NoOp }
                    ]
                ]
            ]
        ]
    , Table.view
        [ Table.custom
            { header = text "Pattern name & description"
            , view = .description >> Markdown.toHtml Nothing >> List.map fromUnstyled >> div []
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
        , Table.custom
            { header = text "Code"
            , view = \{ pattern } -> code [] [ text pattern ]
            , width = Css.px 50
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    , Css.fontSize (Css.px 12)
                    , Css.whiteSpace Css.preWrap
                    ]
            , sort = Nothing
            }
        ]
        [ { description = "**Emphasis block**"
          , example =
                inParagraph
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , Block.view
                        [ Block.plaintext "above"
                        , Block.emphasize
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-1")
                        , Block.withQuestionBox
                            [ QuestionBox.id "question-box-1"
                            , QuestionBox.pointingTo (Dict.get "question-box-1" state.questionBoxMeasurementsById)
                            , QuestionBox.markdown "“Above” is a preposition."
                            , QuestionBox.actions [ { label = "Try again", onClick = NoOp } ]
                            ]
                        ]
                    , Block.view [ Block.plaintext " his TV." ]
                    ]
          , pattern =
                Code.fromModule "Block" "view"
                    ++ Code.listMultiline
                        [ Code.fromModule "Block" "bottomSpacingPx " ++ "(Just 138) -- typically the questionBoxMeasurement's height + 8"
                        , Code.fromModule "Block" "withQuestionBox"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "id " ++ Code.string "question-box"
                                , Code.fromModule moduleName "pointingTo " ++ "model.questionBoxMeasurement"
                                , Code.fromModule moduleName "markdown " ++ Code.string "“Above” is a preposition."
                                , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                                ]
                                2
                        , "…"
                        ]
                        1
          }
        , { description = "**Label block**"
          , example =
                inParagraph
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , Block.view
                        [ Block.plaintext "above his TV"
                        , Block.label "where"
                        , Block.labelId "label-1"
                        , Block.labelPosition (Dict.get "label-1" offsets)
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-2")
                        , Block.yellow
                        , Block.withQuestionBox
                            [ QuestionBox.id "question-box-2"
                            , QuestionBox.pointingTo (Dict.get "question-box-2" state.questionBoxMeasurementsById)
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
          , pattern =
                Code.fromModule "Block" "view"
                    ++ Code.listMultiline
                        [ Code.fromModule "Block" "bottomSpacingPx " ++ "(Just 230) -- typically the questionBoxMeasurement's height + 8"
                        , Code.fromModule "Block" "withQuestionBox"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "id " ++ Code.string "question-box"
                                , Code.fromModule moduleName "pointingTo " ++ "model.questionBoxMeasurement"
                                , Code.fromModule moduleName "markdown " ++ Code.string "Which word is the preposition?"
                                , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                                ]
                                2
                        , "…"
                        ]
                        1
          }
        , { description = "**Blank block**"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.plaintext "Superman"
                        , Block.label "subject"
                        , Block.labelId "label-2"
                        , Block.labelPosition (Dict.get "label-2" offsets)
                        ]
                    , Block.view [ Block.plaintext " " ]
                    , Block.view
                        [ Block.bottomSpacingPx (getBottomSpacingFor "question-box-3")
                        , Block.withQuestionBox
                            [ QuestionBox.id "question-box-3"
                            , QuestionBox.pointingTo (Dict.get "question-box-3" state.questionBoxMeasurementsById)
                            , QuestionBox.markdown "Which verb matches the subject?"
                            , QuestionBox.actions
                                [ { label = "wrap", onClick = NoOp }
                                , { label = "wraps", onClick = NoOp }
                                ]
                            ]
                        ]
                    , Block.view [ Block.plaintext " gifts with comic book pages." ]
                    ]
          , pattern =
                Code.fromModule "Block" "view"
                    ++ Code.listMultiline
                        [ Code.fromModule "Block" "bottomSpacingPx " ++ "(Just 185) -- typically the questionBoxMeasurement's height + 8"
                        , Code.fromModule "Block" "withQuestionBox"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "id " ++ Code.string "question-box"
                                , Code.fromModule moduleName "pointingTo " ++ "model.questionBoxMeasurement"
                                , Code.fromModule moduleName "markdown " ++ Code.string "Which verb matches the subject?"
                                , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                                ]
                                2
                        , "…"
                        ]
                        1
          }
        , { description = "**Labelled blank block**"
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
                    , Block.view
                        [ Block.label "verb"
                        , Block.labelId "label-4"
                        , Block.labelPosition (Dict.get "label-4" offsets)
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-4")
                        , Block.cyan
                        , Block.withQuestionBox
                            [ QuestionBox.id "question-box-4"
                            , QuestionBox.pointingTo (Dict.get "question-box-4" state.questionBoxMeasurementsById)
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
          , pattern =
                Code.fromModule "Block" "view"
                    ++ Code.listMultiline
                        [ Code.fromModule "Block" "bottomSpacingPx " ++ "(Just 230) -- typically the questionBoxMeasurement's height + 8"
                        , Code.fromModule "Block" "withQuestionBox"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "id " ++ Code.string "question-box"
                                , Code.fromModule moduleName "pointingTo " ++ "model.questionBoxMeasurement"
                                , Code.fromModule moduleName "markdown " ++ Code.string "What did he do?"
                                , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                                ]
                                2
                        , "…"
                        ]
                        1
          }
        , { description = "**Blank with emphasis block** with the question box pointing to the entire emphasis"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.emphasize
                        , Block.cyan
                        , Block.content (Block.phrase "Moana " ++ [ Block.blank ])
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-5")
                        , Block.withQuestionBox
                            [ QuestionBox.id "question-box-5"
                            , QuestionBox.pointingTo (Dict.get "question-box-5" state.questionBoxMeasurementsById)
                            , QuestionBox.markdown "Pointing at the entire emphasis"
                            ]
                        ]
                    ]
          , pattern =
                Code.fromModule "Block" "view"
                    ++ Code.listMultiline
                        [ Code.fromModule "Block" "bottomSpacingPx " ++ "(Just 80) -- typically the questionBoxMeasurement's height + 8"
                        , Code.fromModule "Block" "withQuestionBox"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "id " ++ Code.string "question-box"
                                , Code.fromModule moduleName "pointingTo " ++ "model.questionBoxMeasurement"
                                , Code.fromModule moduleName "markdown " ++ Code.string "Pointing at the entire emphasis"
                                , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                                ]
                                2
                        , "…"
                        ]
                        1
          }
        , { description = "**Blank with emphasis block** with the question box pointing to a particular word"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.emphasize
                        , Block.cyan
                        , Block.content
                            (Block.wordWithQuestionBox "Moana"
                                [ QuestionBox.id "question-box-6"
                                , QuestionBox.pointingTo (Dict.get "question-box-6" state.questionBoxMeasurementsById)
                                , QuestionBox.markdown "Pointing at first word"
                                ]
                                :: [ Block.space, Block.blank ]
                            )
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-6")
                        ]
                    ]
          , pattern = "TODO"
          }
        , { description = "**Blank with emphasis block** with the question box pointing to a blank"
          , example =
                inParagraph
                    [ Block.view
                        [ Block.emphasize
                        , Block.cyan
                        , Block.content
                            (Block.phrase "Moana "
                                ++ [ Block.blankWithQuestionBox
                                        [ QuestionBox.id "question-box-7"
                                        , QuestionBox.pointingTo (Dict.get "question-box-7" state.questionBoxMeasurementsById)
                                        , QuestionBox.markdown "Pointing at the blank"
                                        ]
                                   ]
                            )
                        , Block.bottomSpacingPx (getBottomSpacingFor "question-box-7")
                        ]
                    ]
          , pattern = "TODO"
          }
        ]
    ]


inParagraph : List (Html msg) -> Html msg
inParagraph =
    p
        [ css
            [ Css.margin2 Spacing.verticalSpacerPx Css.zero
            , Fonts.quizFont
            , Css.fontSize (Css.px 30)
            ]
        ]


interactiveExampleId : String
interactiveExampleId =
    "interactive-example-question-box"


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
                [ ( "pointingTo Nothing", QuestionBox.pointingTo Nothing )
                , ( "standalone", QuestionBox.standalone )
                ]
            )
        |> CommonControls.css_ "containerCss"
            ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
            , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
            )
            { moduleName = moduleName, use = QuestionBox.containerCss }


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
                    , "article-label-id"
                    , "tricky-label-id"
                    , "warning-label-id"
                    , "warning-2-label-id"
                    ]
                    ++ List.map measureQuestionBox
                        [ "question-box-1"
                        , "question-box-2"
                        , "question-box-3"
                        , "question-box-4"
                        , "question-box-5"
                        , "question-box-6"
                        , "question-box-7"
                        , "left-viewport-question-box-example"
                        , "right-viewport-question-box-example"
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
