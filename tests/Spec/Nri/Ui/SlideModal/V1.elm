module Spec.Nri.Ui.SlideModal.V1 exposing (all)

import Css
import Expect exposing (Expectation)
import Html.Styled as Html
import Nri.Ui.SlideModal.V1 as SlideModal
import Test exposing (..)
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
