module Spec.Nri.Ui.Page exposing (all)

import Accessibility.Key as Key
import Expect
import Html.Styled as Html
import Nri.Ui.Page.V3 as Page
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (..)


all : Test
all =
    describe "Nri.Ui.Page.V3"
        [ describe "button text"
            [ test "handles recovery text for ReturnTo" <|
                \() ->
                    Page.notFound
                        { link = ()
                        , recoveryText = Page.ReturnTo "the main page"
                        }
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ text "Return to the main page" ] ]
            , test "handles recovery text for Reload" <|
                \() ->
                    Page.notFound
                        { link = ()
                        , recoveryText = Page.Reload
                        }
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ text "Try again" ] ]
            , test "handles recovery text for Custom" <|
                \() ->
                    Page.notFound
                        { link = ()
                        , recoveryText = Page.Custom "cats"
                        }
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ text "cats" ] ]
            , test "heading is focusable" <|
                \() ->
                    Page.notFound
                        { link = ()
                        , recoveryText = Page.Reload
                        }
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has
                                [ Selector.all
                                    [ id Page.headingId
                                    , attribute (Key.tabbable False)
                                    ]
                                ]
                            ]
            ]
        ]
