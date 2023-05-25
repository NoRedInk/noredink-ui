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
    4


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
        [ QuestionBox.standalone
        , QuestionBox.markdown "Which words tell you **when** the party is?"
        , QuestionBox.actions
            [ { label = "is having", onClick = NoOp }
            , { label = "after the football game", onClick = NoOp }
            ]
        ]
    , Heading.h3
        [ Heading.plaintext "QuestionBox.pointingTo (positioned against the viewport)"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , inParagraph "paragraph-8"
        [ Block.view [ Block.plaintext "A", Block.emphasize, Block.id "block-8" ]
        , Block.view [ Block.plaintext " " ]
        , Block.view [ Block.plaintext "tricky" ]
        , Block.view [ Block.plaintext " " ]
        , Block.view [ Block.plaintext "example" ]
        , Block.view [ Block.plaintext ". Be sure to be " ]
        , Block.view [ Block.plaintext "careful with content that can get cut off on the left side of the viewport!" ]
        ]
    , QuestionBox.view
        [ QuestionBox.pointingTo "block-8" (Dict.get "left-viewport-question-box-example" state.questionBoxMeasurementsById)
        , QuestionBox.id "left-viewport-question-box-example"
        , QuestionBox.markdown "Articles are important to get right! Is “A” an article?"
        , QuestionBox.actions
            [ { label = "Yes", onClick = NoOp }
            , { label = "No", onClick = NoOp }
            ]
        ]
    , inParagraph "paragraph-9"
        [ Block.view [ Block.plaintext "You also need to be careful with content that can get cut off on the right side of the viewport" ]
        , Block.view
            [ Block.plaintext "!"
            , Block.emphasize
            , Block.id "block-9"
            ]
        ]
    , QuestionBox.view
        [ QuestionBox.pointingTo "block-9" (Dict.get "right-viewport-question-box-example" state.questionBoxMeasurementsById)
        , QuestionBox.id "right-viewport-question-box-example"
        , QuestionBox.markdown "Were you careful? It's important to be careful!"
        , QuestionBox.actions
            [ { label = "Yes", onClick = NoOp }
            , { label = "No", onClick = NoOp }
            ]
        ]
    , Heading.h3
        [ Heading.plaintext "QuestionBox.pointingTo (positioned against an ancestor)"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Container.view
        [ Container.id "container-10"
        , Container.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.position Css.relative
            , Css.maxWidth Css.fitContent
            ]
        , Container.html
            [ inParagraph "paragraph-10"
                [ Block.view
                    [ Block.plaintext "Dave"
                    , Block.id "block-10"
                    , Block.emphasize
                    ]
                , Block.view [ Block.plaintext " scared his replacement cousin coming out of his room wearing a gorilla mask." ]
                ]
            , QuestionBox.view
                [ QuestionBox.pointingTo "block-10" (Dict.get "question-box-10" state.questionBoxMeasurementsById)
                , QuestionBox.id "question-box-10"
                , QuestionBox.markdown
                    """
If we shadows have offended,
Think but this, and all is mended,
That you have but slumbered here
While these visions did appear.
"""
                ]
            ]
        ]
    , Container.view
        [ Container.id "container-11"
        , Container.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.position Css.relative
            , Css.maxWidth Css.fitContent
            ]
        , Container.html
            [ inParagraph "paragraph-11"
                [ Block.view [ Block.plaintext "Bottom scared a bunch of people when it turned out he wasn't wearing a donkey " ]
                , Block.view [ Block.plaintext "mask", Block.id "block-11", Block.emphasize ]
                , Block.view [ Block.plaintext "." ]
                ]
            , QuestionBox.view
                [ QuestionBox.pointingTo "block-11" (Dict.get "question-box-11" state.questionBoxMeasurementsById)
                , QuestionBox.id "question-box-11"
                , QuestionBox.markdown "Give me your hands, if we be friends,\nAnd Robin shall restore amends."
                ]
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
                    [ QuestionBox.pointingTo "block-0" (Dict.get "question-box-0" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-0"
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
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Who?"
                    ]
          }
        , { description = "**Emphasis block**"
          , example =
                tableExample "paragraph-1"
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , Block.view
                        [ Block.plaintext "above"
                        , Block.emphasize
                        , Block.id "block-1"
                        ]
                    , Block.view [ Block.plaintext " his TV." ]
                    ]
                    [ QuestionBox.pointingTo "block-1" (Dict.get "question-box-1" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-1"
                    , QuestionBox.markdown "“Above” is a preposition."
                    , QuestionBox.actions [ { label = "Try again", onClick = NoOp } ]
                    ]
          , pattern =
                tableExampleCode
                    [ "…"
                    , Code.fromModule "Block" "view"
                        ++ Code.listMultiline
                            [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                            , Code.fromModule "Block" "emphasize"
                            , Code.fromModule "Block" "plaintext " ++ Code.string "above"
                            ]
                            3
                    , "…"
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "“Above” is a preposition."
                    , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                    ]
          }
        , { description = "**Label block**"
          , example =
                tableExample "paragraph-2"
                    [ Block.view [ Block.plaintext "Spongebob has a beautiful plant " ]
                    , Block.view
                        [ Block.plaintext "above his TV"
                        , Block.label "where"
                        , Block.id "block-2"
                        , Block.labelId "label-1"
                        , Block.labelPosition (Dict.get "label-1" offsets)
                        , Block.yellow
                        ]
                    , Block.view [ Block.plaintext "." ]
                    ]
                    [ QuestionBox.pointingTo "block-2" (Dict.get "question-box-2" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-2"
                    , QuestionBox.markdown "Which word is the preposition?"
                    , QuestionBox.actions
                        [ { label = "above", onClick = NoOp }
                        , { label = "his", onClick = NoOp }
                        , { label = "TV", onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ "…"
                    , Code.fromModule "Block" "view"
                        ++ Code.listMultiline
                            [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                            , Code.fromModule "Block" "label " ++ Code.string "where"
                            , Code.fromModule "Block" "plaintext " ++ Code.string "above his TV"
                            ]
                            3
                    , "…"
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Which word is the preposition?"
                    , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                    ]
          }
        , { description = "**Blank block**"
          , example =
                tableExample "paragraph-3"
                    [ Block.view [ Block.plaintext "Superman " ]
                    , Block.view [ Block.id "block-3" ]
                    , Block.view [ Block.plaintext " gifts with comic book pages." ]
                    ]
                    [ QuestionBox.pointingTo "block-3" (Dict.get "question-box-3" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-3"
                    , QuestionBox.markdown "Which verb matches the subject?"
                    , QuestionBox.actions
                        [ { label = "wrap", onClick = NoOp }
                        , { label = "wraps", onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ "…"
                    , Code.fromModule "Block" "view "
                        ++ Code.list [ Code.fromModule "Block" "id " ++ Code.string "block-id" ]
                    , "…"
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Which verb matches the subject?"
                    , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                    ]
          }
        , { description = "**Labelled blank block**"
          , example =
                tableExample "paragraph-4"
                    [ Block.view [ Block.plaintext "Dave " ]
                    , Block.view
                        [ Block.label "verb"
                        , Block.id "block-4"
                        , Block.labelId "label-3"
                        , Block.labelPosition (Dict.get "label-3" offsets)
                        ]
                    , Block.view [ Block.plaintext " his replacement cousin coming out of his room wearing a gorilla mask." ]
                    ]
                    [ QuestionBox.pointingTo "block-4" (Dict.get "question-box-4" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-4"
                    , QuestionBox.markdown "What did he do?"
                    , QuestionBox.actions
                        [ { label = "scared", onClick = NoOp }
                        , { label = "scarred", onClick = NoOp }
                        , { label = "scarified", onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ "…"
                    , Code.fromModule "Block" "view "
                        ++ Code.listMultiline
                            [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                            , Code.fromModule "Block" "label " ++ Code.string "verb"
                            , "…"
                            ]
                            3
                    , "…"
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "What did he do?"
                    , Code.fromModule moduleName "actions " ++ Code.list [ "…" ]
                    ]
          }
        , { description = "**Blank with emphasis block** with the question box pointing to the entire emphasis"
          , example =
                tableExample "paragraph-5"
                    [ Block.view
                        [ Block.emphasize
                        , Block.content (Block.phrase "Moana " ++ [ Block.blank ])
                        , Block.id "block-5"
                        ]
                    ]
                    [ QuestionBox.pointingTo "block-5" (Dict.get "question-box-5" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-5"
                    , QuestionBox.markdown "Pointing at the entire emphasis"
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule "Block" "view "
                        ++ Code.listMultiline
                            [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                            , Code.fromModule "Block" "emphasize"
                            , Code.fromModule "Block" "content "
                                ++ Code.newlineWithIndent 4
                                ++ Code.withParens (Code.fromModule "Block" "phrase " ++ Code.string "Moana ++ " ++ Code.list [ Code.fromModule "Block" "blank" ])
                            ]
                            3
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Pointing at the entire emphasis"
                    ]
          }
        , { description = "**Blank with emphasis block** with the question box pointing to a particular word"
          , example =
                tableExample "paragraph-6"
                    [ Block.view
                        [ Block.emphasize
                        , Block.content
                            (Block.wordWithId
                                { word = "Moana"
                                , id = "block-6"
                                }
                                :: [ Block.space, Block.blank ]
                            )
                        ]
                    ]
                    [ QuestionBox.pointingTo "block-6" (Dict.get "question-box-6" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-6"
                    , QuestionBox.markdown "Pointing at the first word"
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                    , Code.fromModule "Block" "emphasize"
                    , Code.fromModule "Block" "content "
                        ++ Code.newlineWithIndent 3
                        ++ Code.withParens
                            (Code.fromModule "Block" "wordWithId "
                                ++ Code.recordMultiline
                                    [ ( "word", Code.string "Moana" )
                                    , ( "id", Code.string "block-id" )
                                    ]
                                    4
                                ++ Code.newlineWithIndent 4
                                ++ ":: "
                                ++ Code.list [ Code.fromModule "Block" "space", Code.fromModule "Block" "blank" ]
                                ++ Code.newlineWithIndent 3
                            )
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Pointing at the first word"
                    ]
          }
        , { description = "**Blank with emphasis block** with the question box pointing to a blank"
          , example =
                tableExample "paragraph-7"
                    [ Block.view
                        [ Block.emphasize
                        , Block.content
                            (Block.phrase "Moana "
                                ++ [ Block.blankWithId "block-7" ]
                            )
                        ]
                    ]
                    [ QuestionBox.pointingTo "block-7" (Dict.get "question-box-7" state.questionBoxMeasurementsById)
                    , QuestionBox.id "question-box-7"
                    , QuestionBox.markdown "Pointing at the blank"
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule "Block" "id " ++ Code.string "block-id"
                    , Code.fromModule "Block" "emphasize"
                    , Code.fromModule "Block" "content "
                        ++ Code.newlineWithIndent 3
                        ++ Code.withParens
                            (Code.fromModule "Block" "phrase "
                                ++ Code.string "Moana "
                                ++ Code.newlineWithIndent 4
                                ++ " ++ "
                                ++ Code.list [ Code.fromModule "Block" "blankWithId " ++ Code.string "block-id" ]
                                ++ Code.newlineWithIndent 3
                            )
                    ]
                    [ Code.fromModule moduleName "id " ++ Code.string "question-box-id"
                    , Code.fromModule moduleName "pointingTo "
                        ++ Code.string "block-id"
                        ++ (Code.newlineWithIndent 3 ++ Code.withParens "Dict.get \"question-box-id\" model.questionBoxMeasurement")
                    , Code.fromModule moduleName "markdown " ++ Code.string "Pointing at the blank"
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
                [ ( "pointingTo \"anchor-id\" Nothing", QuestionBox.pointingTo anchorId Nothing )
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
