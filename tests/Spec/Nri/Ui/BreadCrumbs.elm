module Spec.Nri.Ui.BreadCrumbs exposing (spec)

import Expect
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.BreadCrumbs.V2"
        [ pageTitle
        , headerId
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
                BreadCrumbs.toPageTitle library
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
                BreadCrumbs.initSecondary library
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
                    |> Expect.equal "secondary 2 | secondary 1 | Library | NoRedInk"
        ]


headerId : Test
headerId =
    describe "headerId"
        [ test "1 primary crumb" <|
            \() ->
                BreadCrumbs.headerId home
                    |> Expect.equal homeId
        , test "2 primary crumbs" <|
            \() ->
                BreadCrumbs.headerId library
                    |> Expect.equal libraryId
        , test "1 primary crumb & 1 secondary crumb" <|
            \() ->
                BreadCrumbs.initSecondary home
                    { id = "id-1"
                    , text = "My account"
                    , route = "my-account"
                    }
                    []
                    |> BreadCrumbs.headerId
                    |> Expect.equal "id-1"
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
                    |> BreadCrumbs.headerId
                    |> Expect.equal "id-2"
        , test "2 primary crumbs & 2 secondary crumbs" <|
            \() ->
                BreadCrumbs.initSecondary library
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
                    |> BreadCrumbs.headerId
                    |> Expect.equal "id-2"
        ]


home : BreadCrumbs String
home =
    BreadCrumbs.init { id = homeId, text = "Home", route = "home" } []


library : BreadCrumbs String
library =
    BreadCrumbs.after home { id = libraryId, text = "Library", route = "library" } []


homeId : String
homeId =
    "home-id"


libraryId : String
libraryId =
    "library-id"
