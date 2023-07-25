module Spec.Nri.Ui.QuestionBox exposing (spec)

import Expect
import Html.Styled
import Nri.Ui.QuestionBox.V6 as QuestionBox
import Nri.Ui.UiIcon.V1 as UiIcon
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.QuestionBox.V6"
        [ test "renders markdown as character guidance" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.tip
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has exampleGuidance
                        , Query.has [ character "Panda" ]
                        ]
        , test "renders markdown as guidance with custom character" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.tip
                , QuestionBox.character (Just { name = "Apply", icon = UiIcon.apple })
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has exampleGuidance
                        , Query.has [ character "Apply" ]
                        ]
        , test "renders markdown as guidance without character" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.character Nothing
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has exampleGuidance
                        , Query.hasNot [ character "Panda" ]
                        ]
        , test "renders extra HTML content with default character" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.tip
                , QuestionBox.leftActions (Html.Styled.text readAloudContent)
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has [ text readAloudContent ]
                        , Query.has exampleGuidance
                        , Query.has [ character "Panda" ]
                        ]
        , test "renders extra HTML content with custom character" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.leftActions (Html.Styled.text readAloudContent)
                , QuestionBox.tip
                , QuestionBox.character (Just { name = "Apply", icon = UiIcon.apple })
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has [ text readAloudContent ]
                        , Query.has exampleGuidance
                        , Query.has [ character "Apply" ]
                        ]
        , test "renders extra HTML content without character" <|
            \() ->
                [ QuestionBox.markdown exampleGuidanceContent
                , QuestionBox.leftActions (Html.Styled.text readAloudContent)
                , QuestionBox.character Nothing
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has [ text readAloudContent ]
                        , Query.has exampleGuidance
                        , Query.hasNot [ character "Panda" ]
                        ]
        ]


exampleGuidanceContent : String
exampleGuidanceContent =
    "This is **important**!"


exampleGuidance : List Selector
exampleGuidance =
    [ text "This is "
    , all [ tag "strong", containing [ text "important" ] ]
    ]


readAloudContent : String
readAloudContent =
    "Read aloud play/pause/continue interface"


character : String -> Selector
character name =
    all [ tag "svg", containing [ tag "title", text (name ++ " says") ] ]


testContext : List (QuestionBox.Attribute m) -> Query.Single m
testContext =
    QuestionBox.view
        >> Html.Styled.toUnstyled
        >> Query.fromHtml
