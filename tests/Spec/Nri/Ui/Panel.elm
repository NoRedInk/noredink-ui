module Spec.Nri.Ui.Panel exposing (..)

import Html
import Html.Styled exposing (..)
import Nri.Ui.Panel.V1 as Panel
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Query as Query


spec : Test
spec =
    describe "Nri.Ui.Panel.V1"
        [ describe "renders content" rendersContent
        ]


rendersContent : List Test
rendersContent =
    [ test "renders markdown correctly" <|
        \() ->
            programMarkdown
                |> ensureView
                    (Query.contains
                        [ Html.em [] [ Html.text "italic" ], Html.strong [] [ Html.text "bold" ] ]
                    )
                |> done
    , test "renders html correctly" <|
        \() ->
            programHtml
                |> ensureView
                    (Query.contains
                        [ Html.em [] [ Html.text "italic" ], Html.strong [] [ Html.text "bold" ] ]
                    )
                |> done
    ]


type alias Model =
    {}


init : Model
init =
    {}


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg state =
    case msg of
        NoOp ->
            state


viewMarkdown : Model -> Html Msg
viewMarkdown model =
    Panel.view
        [ Panel.header "Header"
        , Panel.markdown "_italic_ **bold**"
        ]


viewHtml : Model -> Html Msg
viewHtml model =
    Panel.view
        [ Panel.header "Header"
        , Panel.html [ em [] [ text "italic" ], strong [] [ text "bold" ] ]
        ]


type alias TestContext =
    ProgramTest Model Msg ()


programMarkdown : TestContext
programMarkdown =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewMarkdown >> toUnstyled
        }
        |> ProgramTest.start ()


programHtml : TestContext
programHtml =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewHtml >> toUnstyled
        }
        |> ProgramTest.start ()
