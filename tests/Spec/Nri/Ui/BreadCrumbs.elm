module Spec.Nri.Ui.BreadCrumbs exposing (spec)

import Expect
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.BreadCrumbs.V2"
        [ pageTitle
        ]


pageTitle : Test
pageTitle =
    describe "toPageTitle"
        [ test "1 primary crumb" <|
            \() ->
                BreadCrumbs.toPageTitle home
                    |> Expect.equal "Home | NoRedInk"
        , test "2 primary crumbs" <|
            \() ->
                BreadCrumbs.after home
                    { id = "id-1"
                    , text = "Library"
                    , route = "Library"
                    }
                    []
                    |> BreadCrumbs.toPageTitle
                    |> Expect.equal "Library | Home | NoRedInk"
        , test "1 primary crumb & 1 secondary crumb" <|
            \() ->
                BreadCrumbs.initSecondary home
                    { id = "id-1"
                    , text = "My account"
                    , route = "my-account"
                    }
                    []
                    |> BreadCrumbs.toPageTitle
                    |> Expect.equal "My account | Home | NoRedInk"
        , test "1 primary crumb & 2 secondary crumbs" <|
            \() ->
                BreadCrumbs.initSecondary home
                    { id = "id-1"
                    , text = "secondary 1"
                    , route = "secondary 1"
                    }
                    []
                    |> (\previous ->
                            BreadCrumbs.after previous
                                { id = "id-2"
                                , text = "secondary 2"
                                , route = "secondary 2"
                                }
                                []
                       )
                    |> BreadCrumbs.toPageTitle
                    |> Expect.equal "secondary 2 | secondary 1 | Home | NoRedInk"
        , test "2 primary crumbs & 2 secondary crumbs" <|
            \() ->
                BreadCrumbs.after home
                    { id = "primary-1"
                    , text = "primary 1"
                    , route = "primary 1"
                    }
                    []
                    |> (\previous ->
                            BreadCrumbs.initSecondary previous
                                { id = "id-1"
                                , text = "secondary 1"
                                , route = "secondary 1"
                                }
                                []
                       )
                    |> (\previous ->
                            BreadCrumbs.after previous
                                { id = "id-2"
                                , text = "secondary 2"
                                , route = "secondary 2"
                                }
                                []
                       )
                    |> BreadCrumbs.toPageTitle
                    |> Expect.equal "secondary 2 | secondary 1 | primary 1 | NoRedInk"
        ]


home : BreadCrumbs String
home =
    BreadCrumbs.init
        { id = "id-0"
        , text = "Home"
        , route = "home"
        }
        []
