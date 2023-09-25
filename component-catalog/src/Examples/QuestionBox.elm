module Examples.QuestionBox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Markdown
import Nri.Ui.Button.V10 as Button
import Nri.Ui.CharacterIcon.V2 as CharacterIcon
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.QuestionBox.V6 as QuestionBox
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "QuestionBox"


version : Int
version =
    6


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
    , preview =
        [ QuestionBox.view
            [ QuestionBox.markdown "You got it! 🎉"
            , QuestionBox.correct
            ]
        ]
    , about = []
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
        , extraCode =
            [ "import Nri.Ui.CharacterIcon.V2 as CharacterIcon"
            , "import Nri.Ui.Button.V10 as Button"
            , Code.newlines
            , Code.unionType "Msg" [ "NoOp" ]
            ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = Code.fromModule moduleName "view"
                  , code =
                        Code.fromModule moduleName "view "
                            ++ Code.list ("QuestionBox.characterPosition { width = 80, height = 103, top = -62 }" :: List.map Tuple.first attributes)
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable example"
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
            , QuestionBox.view
                (QuestionBox.characterPosition { width = 80, height = 103, top = -62 }
                    :: List.map Tuple.second attributes
                )
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Non-customizable examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
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
            , width = Css.pct 40
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "Code"
            , view = \{ pattern } -> code [] [ text pattern ]
            , width = Css.pct 45
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
        [ { description = "**Neutral Step**"
          , example =
                tableExample
                    [ QuestionBox.markdown "**Adjectives describe nouns.** <br/><br/>Which word describes the noun **lollipop**?"
                    , QuestionBox.actions
                        [ { label = "won", theme = Button.secondary, onClick = NoOp }
                        , { label = "huge", theme = Button.secondary, onClick = NoOp }
                        , { label = "T’Challa", theme = Button.secondary, onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule moduleName "markdown " ++ Code.string "**Adjectives describe nouns.** <br/><br/>Which word describes the noun **lollipop**?"
                    , Code.fromModule moduleName "actions"
                        ++ Code.listMultiline
                            [ secondaryButtonCode "won"
                            , secondaryButtonCode "huge"
                            , secondaryButtonCode "T’Challa"
                            ]
                            2
                    ]
          }
        , { description = "**Correct Step**"
          , example =
                tableExample
                    [ QuestionBox.markdown "**That's right!** 🎉 <br/><br/> **Scary** tells us **what kind** of painting. <br/> That means **scary** is an **adjective**."
                    , QuestionBox.correct
                    , QuestionBox.actions
                        [ { label = "try this question again", theme = Button.primary, onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule moduleName "markdown " ++ Code.string "**That's right!** 🎉 <br/><br/> **Scary** tells us **what kind** of painting. <br/> That means **scary** is an **adjective**."
                    , Code.fromModule moduleName "correct"
                    , Code.fromModule moduleName "actions"
                        ++ Code.listMultiline
                            [ primaryButtonCode "try this question again"
                            ]
                            2
                    ]
          }
        , { description = "**Incorrect Step**"
          , example =
                tableExample
                    [ QuestionBox.markdown "**Not quite.** <br/><br /> **Past** doesn't tell us **what kind** of painting it is. <br/><br/> Look for word like these: \n- funny \n- pretty"
                    , QuestionBox.incorrect
                    , QuestionBox.actions
                        [ { label = "Try again", theme = Button.secondary, onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule moduleName "markdown " ++ Code.string "**Not quite.** <br/><br /> **Past** doesn't tell us **what kind** of painting it is. <br/><br/> Look for word like these: \n- funny \n- pretty"
                    , Code.fromModule moduleName "incorrect"
                    , Code.fromModule moduleName "actions"
                        ++ Code.listMultiline
                            [ secondaryButtonCode "Try again"
                            ]
                            2
                    ]
          }
        , { description = "**Retry Tip**"
          , example =
                tableExample
                    [ QuestionBox.markdown "This list has **three items**, so you need two commas to separate them. **Put commas between the items.**"
                    , QuestionBox.tip
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule moduleName "markdown " ++ Code.string "This list has **three items**, so you need two commas to separate them. **Put commas between the items.**"
                    , Code.fromModule moduleName "tip"
                    ]
          }
        , { description = "**Retry Incorrect**"
          , example =
                tableExample
                    [ QuestionBox.markdown "**Not quite.** <br/><br/> Remember, **adjectives** are **describing words**. In this sentence, **scary** is an adjective. It tells us **what kind** of **painting**."
                    , QuestionBox.incorrect
                    , QuestionBox.actionsHorizontal
                    , QuestionBox.actions
                        [ { label = "Next question", theme = Button.primary, onClick = NoOp }
                        , { label = "Review tutorial", theme = Button.secondary, onClick = NoOp }
                        ]
                    ]
          , pattern =
                tableExampleCode
                    [ Code.fromModule moduleName "markdown " ++ Code.string "**Not quite.** <br/><br/> Remember, **adjectives** are **describing words**. In this sentence, **scary** is an adjective. It tells us **what kind** of **painting**."
                    , Code.fromModule moduleName "incorrect"
                    , Code.fromModule moduleName "actionsHorizontal"
                    , Code.fromModule moduleName "actions"
                        ++ Code.listMultiline
                            [ primaryButtonCode "Next question"
                            , secondaryButtonCode "Review tutorial"
                            ]
                            2
                    ]
          }
        ]
    ]


tableExample : List (QuestionBox.Attribute msg) -> Html msg
tableExample questionBoxAttributes =
    div [ css [ Css.padding2 (Css.px 15) Css.zero ] ] [ QuestionBox.view questionBoxAttributes ]


tableExampleCode : List String -> String
tableExampleCode questionBoxAttributes =
    Code.fromModule moduleName "view" ++ Code.listMultiline questionBoxAttributes 1


primaryButtonCode : String -> String
primaryButtonCode label =
    Code.recordMultiline [ ( "label", Code.string label ), ( "theme", Code.fromModule "Button" "primary" ), ( "onClick", "NoOp" ) ] 3


secondaryButtonCode : String -> String
secondaryButtonCode label =
    Code.recordMultiline [ ( "label", Code.string label ), ( "theme", Code.fromModule "Button" "secondary" ), ( "onClick", "NoOp" ) ] 3


leftActionIconsCode : List ( String, String ) -> String
leftActionIconsCode icons =
    let
        iconCode ( label, icon ) =
            [ Code.fromModule "ClickableSvg" "button"
            , Code.string label
            , Code.fromModule "UiIcon" icon
            , Code.listMultiline
                [ Code.apply [ Code.fromModule "ClickableSvg" "exactSize", Code.int 24 ]
                , Code.apply
                    [ Code.fromModule "ClickableSvg" "css"
                    , Code.listMultiline
                        [ Code.apply [ Code.fromModule "Css" "borderRadius", Code.withParens (Code.apply [ Code.fromModule "Css" "px", Code.int 24 ]) ]
                        , Code.apply [ Code.fromModule "Css" "backgroundColor", Code.fromModule "Colors" "white" ]
                        ]
                        5
                    ]
                ]
                4
            ]
                |> Code.apply
    in
    Code.withParens <| Code.apply [ "div", "[]", Code.listMultiline (List.map iconCode icons) 3 ]


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
    }


{-| -}
type alias State =
    { attributes : Control (List ( String, QuestionBox.Attribute Msg ))
    }


initAttributes : Control (List ( String, QuestionBox.Attribute Msg ))
initAttributes =
    ControlExtra.list
        |> ControlExtra.listItem "content"
            (Control.choice
                [ ( "markdown"
                  , Control.map
                        (\str ->
                            ( Code.fromModule moduleName "markdown " ++ Code.stringMultiline str
                            , QuestionBox.markdown str
                            )
                        )
                        (Control.stringTextarea initialMarkdown)
                  )
                , ( "html"
                  , Control.value
                        ( "QuestionBox.html = ... "
                        , QuestionBox.html
                            (div []
                                [ p []
                                    [ text "Let's remember the rules "
                                    , ul []
                                        [ li [] [ text "Always capitalize the first word of a title" ]
                                        , li [] [ text "Capitalize all words except small words like “and,” “of” and “an.”" ]
                                        ]
                                    , strong [] [ text "How should this be capitalized?" ]
                                    ]
                                ]
                            )
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "leftButton"
            ([ ( "Play button"
               , Control.value
                    ( Code.fromModule moduleName "leftActions" ++ Code.newlineWithIndent 2 ++ leftActionIconsCode [ ( "Play", "playInCircle" ) ]
                    , QuestionBox.leftActions
                        (div []
                            [ ClickableSvg.button "Play"
                                UiIcon.playInCircle
                                [ ClickableSvg.exactSize 24
                                , ClickableSvg.css
                                    [ Css.borderRadius (Css.px 24)
                                    , Css.backgroundColor Colors.white
                                    ]
                                ]
                            ]
                        )
                    )
               )
             , ( "Pause/Stop button"
               , Control.value
                    ( Code.fromModule moduleName "leftActions" ++ Code.newlineWithIndent 2 ++ leftActionIconsCode [ ( "Pause", "pauseInCircle" ), ( "Stop", "stopInCircle" ) ]
                    , QuestionBox.leftActions
                        ([ ClickableSvg.button "Pause"
                            UiIcon.pauseInCircle
                            [ ClickableSvg.exactSize 24
                            , ClickableSvg.css
                                [ Css.borderRadius (Css.px 24)
                                , Css.backgroundColor Colors.white
                                ]
                            ]
                         , ClickableSvg.button "Stop"
                            UiIcon.stopInCircle
                            [ ClickableSvg.exactSize 24
                            , ClickableSvg.css
                                [ Css.borderRadius (Css.px 24)
                                , Css.backgroundColor Colors.white
                                ]
                            ]
                         ]
                            |> div
                                [ css
                                    [ Css.displayFlex
                                    , Css.flexDirection Css.column
                                    , Css.property "gap" "6px"
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
                                            , ( "theme"
                                              , if i_ == 1 then
                                                    Code.fromModule "Button" "primary"

                                                else
                                                    Code.fromModule "Button" "secondary"
                                              )
                                            , ( "onClick", "NoOp" )
                                            ]
                                        , { label = "Button " ++ String.fromInt i_
                                          , theme =
                                                if i_ == 1 then
                                                    Button.primary

                                                else
                                                    Button.secondary
                                          , onClick = NoOp
                                          }
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
        |> ControlExtra.listItem "actionsOrientation"
            (CommonControls.choice moduleName
                [ ( "actionsHorizontal", QuestionBox.actionsVertical )
                , ( "actionsHorizontal", QuestionBox.actionsHorizontal )
                ]
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
            ([ { name = "Lindy"
               , icon =
                    ( "CharacterIcon.lindyHeadshot"
                    , CharacterIcon.lindyHeadshot
                    )
               }
             , { name = "Red"
               , icon =
                    ( "CharacterIcon.redHeadshot"
                    , CharacterIcon.redHeadshot
                    )
               }
             , { name = "Sal"
               , icon =
                    ( "CharacterIcon.salHeadshot"
                    , CharacterIcon.salHeadshot
                    )
               }
             ]
                |> List.map
                    (\{ name, icon } ->
                        ( name
                        , Control.value
                            ( "QuestionBox.character " ++ Tuple.first icon
                            , QuestionBox.character
                                { name = name
                                , icon = Tuple.second icon
                                }
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


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls configuration ->
            ( { state | attributes = configuration }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )
