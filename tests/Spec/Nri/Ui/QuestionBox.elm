module Spec.Nri.Ui.QuestionBox exposing (spec)

import Expect
import Html.Styled
import Nri.Ui.QuestionBox.V3 as QuestionBox
import Nri.Ui.UiIcon.V1 as UiIcon
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.QuestionBox.V4"
        [ test "renders markdown as character guidance" <|
            \() ->
                [ QuestionBox.markdown "This is **important**!" ]
                    |> testContext
                    |> Expect.all
                        [ Query.has
                            [ text "This is "
                            , all [ tag "strong", containing [ text "important" ] ]
                            ]
                        , Query.has [ character "Panda" ]
                        ]
        , test "renders markdown as guidance with custom character" <|
            \() ->
                [ QuestionBox.markdown "This is **important**!"
                , QuestionBox.character (Just { name = "Apply", icon = UiIcon.apple })
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has
                            [ text "This is "
                            , all [ tag "strong", containing [ text "important" ] ]
                            ]
                        , Query.has [ character "Apply" ]
                        ]
        , test "renders markdown as guidance without character" <|
            \() ->
                [ QuestionBox.markdown "This is **important**!"
                , QuestionBox.character Nothing
                ]
                    |> testContext
                    |> Expect.all
                        [ Query.has
                            [ text "This is "
                            , all [ tag "strong", containing [ text "important" ] ]
                            ]
                        , Query.hasNot [ character "Panda" ]
                        ]
        ]


character : String -> Selector
character name =
    all [ tag "svg", containing [ tag "title", text (name ++ " says") ] ]


testContext : List (QuestionBox.Attribute m) -> Query.Single m
testContext =
    QuestionBox.view
        >> Html.Styled.toUnstyled
        >> Query.fromHtml
