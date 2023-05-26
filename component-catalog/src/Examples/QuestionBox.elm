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
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Markdown
import Nri.Ui.Block.V4 as Block
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.MediaQuery.V1 exposing (..)
import Nri.Ui.QuestionBox.V5 as QuestionBox
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


moduleName : String
moduleName =
    "QuestionBox"


version : Int
version =
    5


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Instructional ]
    , keyboardSupport = []
    , preview = [ QuestionBox.view [ QuestionBox.markdown "Gud?" ] ]
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
            [ div [ Attributes.id anchorId ]
                [ UiIcon.sortArrowDown
                    |> Svg.withLabel "Anchor"
                    |> Svg.withWidth (Css.px 30)
                    |> Svg.withHeight (Css.px 30)
                    |> Svg.toHtml
                ]
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
        [ QuestionBox.markdown "Which words tell you **when** the party is?"
        , QuestionBox.actions
            [ { label = "is having", onClick = NoOp }
            , { label = "after the football game", onClick = NoOp }
            ]
        ]
    , Table.view []
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
        [ { description = "**Plain block**"
          , example =
                tableExample "paragraph-0"
                    [ Block.view
                        [ Block.plaintext "Dave"
                        , Block.id "block-0"
                        ]
                    , Block.view [ Block.plaintext " scared his replacement cousin coming out of his room wearing a gorilla mask." ]
                    ]
                    [ QuestionBox.id "question-box-0"
                    , QuestionBox.markdown "Who?"
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule "Block" "view"
                        ++ Code.listMultiline
                            [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                            , Code.fromModule "Block" "plaintext " ++ Code.string "Dave"
                            ]
                            3
                    , "…"
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "markdown " ++ Code.string "Who?"
                    ]
          }
        ]
    ]


tableExample : String -> List (Html msg) -> List (QuestionBox.Attribute msg) -> Html msg
tableExample paragraphId paragraphContents questionBoxAttributes =
    div [] [ inParagraph paragraphId paragraphContents, QuestionBox.view questionBoxAttributes ]


tableExampleCode : List String -> List String -> String
tableExampleCode blockAttributes questionBoxAttributes =
    "div []"
        ++ Code.listMultiline
            [ "p [ id \"paragraph-id\" ]" ++ Code.listMultiline blockAttributes 2
            , Code.fromModule moduleName "view" ++ Code.listMultiline questionBoxAttributes 2
            ]
            1


inParagraph : String -> List (Html msg) -> Html msg
inParagraph id =
    p
        [ Attributes.id id
        , css
            [ Css.margin Css.zero
            , Fonts.quizFont
            , Css.fontSize (Css.px 30)
            ]
        ]


anchorId : String
anchorId =
    "anchor-id"


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
    , questionBoxMeasurementsById :
        Dict
            String
            { block : Element
            , paragraph : Element
            , questionBox : Element
            , container : Maybe Element
            }
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
        |> ControlExtra.optionalListItem "leftButton"
            ([ ( "Play button"
               , Control.value
                    ( Code.fromModule moduleName "setLeftActions" ++ Code.string """
                        ([ ClickableSvg.button "Play"
                            UiIcon.playInCircle
                            [ ClickableSvg.exactSize 32
                            , ClickableSvg.css
                                [ Css.borderRadius (Css.px 32)
                                , Css.backgroundColor Colors.white
                                ]
                            ]
                         ]
                            |> div
                                [ css
                                    [ Css.position Css.relative
                                    , Css.zIndex (Css.int 1)
                                    , Css.left (Css.px -24)
                                    , Css.top (Css.px 8)
                                    , withMedia [ quizEngineMobile ]
                                        [ Css.left Css.auto
                                        , Css.top Css.auto
                                        , Css.float Css.left
                                        , Css.padding4 Css.zero (Css.px 5) Css.zero Css.zero
                                        , Css.position Css.static
                                        ]
                                    ]
                                ]
                        )                      """
                    , QuestionBox.setLeftActions
                        ([ ClickableSvg.button "Play"
                            UiIcon.playInCircle
                            [ ClickableSvg.exactSize 32
                            , ClickableSvg.css
                                [ Css.borderRadius (Css.px 32)
                                , Css.backgroundColor Colors.white
                                ]
                            ]
                         ]
                            |> div
                                [ css
                                    [ Css.position Css.relative
                                    , Css.zIndex (Css.int 1)
                                    , Css.left (Css.px -24)
                                    , Css.top (Css.px 8)
                                    , withMedia [ quizEngineMobile ]
                                        [ Css.left Css.auto
                                        , Css.top Css.auto
                                        , Css.float Css.left
                                        , Css.padding4 Css.zero (Css.px 5) Css.zero Css.zero
                                        , Css.position Css.static
                                        ]
                                    ]
                                ]
                        )
                    )
               )
             ]
                |> Control.choice
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
        |> ControlExtra.listItem "theme"
            (CommonControls.choice moduleName
                [ ( "neutral", QuestionBox.neutral )
                , ( "correct", QuestionBox.correct )
                , ( "incorrect", QuestionBox.incorrect )
                , ( "tip", QuestionBox.tip )
                ]
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
    | GotQuestionBoxMeasurements
        String
        (Result
            Dom.Error
            { block : Element
            , paragraph : Element
            , questionBox : Element
            , container : Maybe Element
            }
        )


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
                    , "label-3"
                    ]
                    ++ List.map measureQuestionBox
                        [ { paragraphId = "paragraph-0", blockId = "block-0", questionBoxId = "question-box-0", containerId = Nothing }
                        , { paragraphId = "paragraph-1", blockId = "block-1", questionBoxId = "question-box-1", containerId = Nothing }
                        , { paragraphId = "paragraph-2", blockId = "block-2", questionBoxId = "question-box-2", containerId = Nothing }
                        , { paragraphId = "paragraph-3", blockId = "block-3", questionBoxId = "question-box-3", containerId = Nothing }
                        , { paragraphId = "paragraph-4", blockId = "block-4", questionBoxId = "question-box-4", containerId = Nothing }
                        , { paragraphId = "paragraph-5", blockId = "block-5", questionBoxId = "question-box-5", containerId = Nothing }
                        , { paragraphId = "paragraph-6", blockId = "block-6", questionBoxId = "question-box-6", containerId = Nothing }
                        , { paragraphId = "paragraph-7", blockId = "block-7", questionBoxId = "question-box-7", containerId = Nothing }
                        , { paragraphId = "paragraph-8", blockId = "block-8", questionBoxId = "left-viewport-question-box-example", containerId = Nothing }
                        , { paragraphId = "paragraph-9", blockId = "block-9", questionBoxId = "right-viewport-question-box-example", containerId = Nothing }
                        , { paragraphId = "paragraph-10", blockId = "block-10", questionBoxId = "question-box-10", containerId = Just "container-10" }
                        , { paragraphId = "paragraph-11", blockId = "block-11", questionBoxId = "question-box-11", containerId = Just "container-11" }
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


measureQuestionBox : { paragraphId : String, blockId : String, questionBoxId : String, containerId : Maybe String } -> Cmd Msg
measureQuestionBox { paragraphId, blockId, questionBoxId, containerId } =
    Task.map4
        (\paragraph block questionBox container ->
            { block = block
            , paragraph = paragraph
            , questionBox = questionBox
            , container = container
            }
        )
        (Dom.getElement paragraphId)
        (Dom.getElement blockId)
        (Dom.getElement questionBoxId)
        (Maybe.map (Dom.getElement >> Task.map Just) containerId
            |> Maybe.withDefault (Task.succeed Nothing)
        )
        |> Task.attempt (GotQuestionBoxMeasurements questionBoxId)
