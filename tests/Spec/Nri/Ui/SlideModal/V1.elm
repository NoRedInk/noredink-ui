module Spec.Nri.Ui.SlideModal.V1 exposing (all)

import Css
import Expect exposing (Expectation)
import Html.Styled as Html
import Json.Encode
import Nri.Ui.SlideModal.V1 as SlideModal
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


all : Test
all =
    describe "Nri.Ui.SlideModal.V1"
        [ test "shows first panel when open" <|
            \() ->
                SlideModal.open
                    |> SlideModal.view
                        { panels = threePanels
                        , height = Css.vh 60
                        , parentMsg = identity
                        }
                    |> Html.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ text "Title1", text "Content1" ]
                        , Query.hasNot [ text "Title2", text "Content2" ]
                        , Query.hasNot [ text "Title3", text "Content3" ]
                        ]
        , test "shows no panel when closed" <|
            \() ->
                SlideModal.closed
                    |> SlideModal.view
                        { panels = threePanels
                        , height = Css.vh 60
                        , parentMsg = identity
                        }
                    |> Html.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.hasNot [ text "Title1", text "Content1" ]
                        , Query.hasNot [ text "Title2", text "Content2" ]
                        , Query.hasNot [ text "Title3", text "Content3" ]
                        ]
        , test "can click through" <|
            \() ->
                { panels = threePanels
                , height = Css.vh 60
                , parentMsg = identity
                }
                    |> initTest
                    |> click "Continue1"
                    |> click "Continue2"
                    |> click "Continue3"
                    |> assertAndFinish
                        [ Query.hasNot [ text "Title1", text "Content1" ]
                        , Query.hasNot [ text "Title2", text "Content2" ]
                        , Query.hasNot [ text "Title3", text "Content3" ]
                        ]
        , test "can navigate back using the dots" <|
            \() ->
                { panels = threePanels
                , height = Css.vh 60
                , parentMsg = identity
                }
                    |> initTest
                    |> click "Continue1"
                    |> click "Go to Title1"
                    |> assertAndFinish
                        [ Query.has [ text "Title1", text "Content1" ]
                        , Query.hasNot [ text "Title2", text "Content2" ]
                        , Query.hasNot [ text "Title3", text "Content3" ]
                        ]
        , test "cannot navigate forward using the dots" <|
            \() ->
                { panels = threePanels
                , height = Css.vh 60
                , parentMsg = identity
                }
                    |> initTest
                    |> click "Continue1"
                    |> assertAndFinish
                        [ Query.has [ tag "button", containing [ text "Go to Title1" ] ]
                        , Query.hasNot [ tag "button", containing [ text "Go to Title2" ] ]
                        , Query.has [ tag "button", disabled True, containing [ text "Go to Title3" ] ]
                        ]
        ]


threePanels : List (SlideModal.Panel msg)
threePanels =
    [ { icon = Html.text "Icon1"
      , title = "Title1"
      , content = Html.text "Content1"
      , buttonLabel = "Continue1"
      }
    , { icon = Html.text "Icon2"
      , title = "Title2"
      , content = Html.text "Content 2"
      , buttonLabel = "Continue2"
      }
    , { icon = Html.text "Icon3"
      , title = "Title3"
      , content = Html.text "Content 3"
      , buttonLabel = "Continue3"
      }
    ]


type alias TestContext =
    { view : SlideModal.State -> Query.Single SlideModal.State
    , state : Result String SlideModal.State
    }


initTest : SlideModal.Config SlideModal.State -> TestContext
initTest config =
    { view = SlideModal.view config >> Html.toUnstyled >> Query.fromHtml
    , state = Ok SlideModal.open
    }


click : String -> TestContext -> TestContext
click buttonText =
    simulate
        (Query.find [ tag "button", containing [ text buttonText ] ])
        Event.click


simulate :
    (Query.Single SlideModal.State -> Query.Single SlideModal.State)
    -> ( String, Json.Encode.Value )
    -> TestContext
    -> TestContext
simulate findElement event testContext =
    { testContext
        | state =
            Result.andThen
                (testContext.view
                    >> findElement
                    >> Event.simulate event
                    >> Event.toResult
                )
                testContext.state
    }


assertAndFinish : List (Query.Single SlideModal.State -> Expectation) -> TestContext -> Expectation
assertAndFinish expectations { view, state } =
    case Result.map view state of
        Ok query ->
            Expect.all expectations query

        Err err ->
            Expect.fail err
