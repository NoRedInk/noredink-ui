module Spec.Nri.Ui.SortableTable exposing (spec)

import Expect exposing (Expectation)
import Html.Styled
import Nri.Ui.SortableTable.V2 as SortableTable
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type Column
    = FirstName
    | LastName
    | Coins


type alias Person =
    { firstName : String
    , lastName : String
    }


type Msg
    = NoOp


config : SortableTable.Config Column Person Msg
config =
    { updateMsg = \_ -> NoOp
    , columns =
        [ SortableTable.string
            { id = FirstName
            , header = "First name"
            , value = .firstName
            , width = 125
            , cellStyles = \_ -> []
            }
        , SortableTable.string
            { id = LastName
            , header = "Last name"
            , value = .lastName
            , width = 125
            , cellStyles = \_ -> []
            }
        ]
    }


entries : List Person
entries =
    [ { firstName = "Bob", lastName = "Stevenson" }
    , { firstName = "Alice", lastName = "Vonnegut" }
    , { firstName = "Charlie", lastName = "Dickens" }
    ]


tableView : SortableTable.State Column -> Query.Single Msg
tableView sortState =
    SortableTable.view config sortState entries
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


sortBy : Column -> SortableTable.State Column
sortBy field =
    SortableTable.init field


sortByDescending : Column -> SortableTable.State Column
sortByDescending field =
    SortableTable.initDescending field


ascending =
    sortBy FirstName |> .sortDirection


descending =
    sortByDescending FirstName |> .sortDirection


spec : Test
spec =
    describe "Nri.SortableTable"
        [ describe "invariantSort"
            [ test "sorts in the same way independently of sort direction" <|
                \() ->
                    let
                        sorter =
                            SortableTable.invariantSort .firstName
                    in
                    List.sortWith (sorter ascending) entries
                        |> Expect.equal (List.sortWith (sorter descending) entries)
            ]
        , describe "simpleSort"
            [ test "sorts ascending" <|
                \() ->
                    let
                        sorter =
                            SortableTable.simpleSort .firstName
                    in
                    List.sortWith (sorter ascending) entries
                        |> List.map .firstName
                        |> Expect.equal [ "Alice", "Bob", "Charlie" ]
            , test "sorts descending" <|
                \() ->
                    let
                        sorter =
                            SortableTable.simpleSort .firstName
                    in
                    List.sortWith (sorter descending) entries
                        |> List.map .firstName
                        |> Expect.equal [ "Charlie", "Bob", "Alice" ]
            ]
        , describe "combineSorters"
            [ test "combines multiple sorters" <|
                \() ->
                    let
                        newEntries =
                            { firstName = "Alice", lastName = "InChains" } :: entries

                        sortByFirstName =
                            SortableTable.simpleSort .firstName

                        sortByLastName =
                            SortableTable.simpleSort .lastName

                        sorter =
                            SortableTable.combineSorters [ sortByFirstName, sortByLastName ]
                    in
                    List.sortWith (sorter ascending) newEntries
                        |> List.map (\elem -> elem.firstName ++ " " ++ elem.lastName)
                        |> Expect.equal
                            [ "Alice InChains"
                            , "Alice Vonnegut"
                            , "Bob Stevenson"
                            , "Charlie Dickens"
                            ]
            ]
        , describe "view"
            [ test "displays one row for each entry" <|
                \() ->
                    tableView (sortBy FirstName)
                        |> Query.find [ Selector.tag "tbody" ]
                        |> Query.findAll [ Selector.tag "tr" ]
                        |> Query.count (Expect.equal 3)
            , test "displays entries sorted by first name" <|
                \() ->
                    tableView (sortBy FirstName)
                        |> Query.find [ Selector.tag "tbody" ]
                        |> Query.findAll [ Selector.tag "tr" ]
                        |> Expect.all
                            [ Query.index 0 >> Query.has [ Selector.text "Alice" ]
                            , Query.index 1 >> Query.has [ Selector.text "Bob" ]
                            , Query.index 2 >> Query.has [ Selector.text "Charlie" ]
                            ]
            , test "displays entries sorted by last name" <|
                \() ->
                    tableView (sortBy LastName)
                        |> Query.find [ Selector.tag "tbody" ]
                        |> Query.findAll [ Selector.tag "tr" ]
                        |> Expect.all
                            [ Query.index 0 >> Query.has [ Selector.text "Dickens" ]
                            , Query.index 1 >> Query.has [ Selector.text "Stevenson" ]
                            , Query.index 2 >> Query.has [ Selector.text "Vonnegut" ]
                            ]
            , test "displays entries sorted by descending first name" <|
                \() ->
                    tableView (sortByDescending FirstName)
                        |> Query.find [ Selector.tag "tbody" ]
                        |> Query.findAll [ Selector.tag "tr" ]
                        |> Expect.all
                            [ Query.index 0 >> Query.has [ Selector.text "Charlie" ]
                            , Query.index 1 >> Query.has [ Selector.text "Bob" ]
                            , Query.index 2 >> Query.has [ Selector.text "Alice" ]
                            ]
            ]
        ]
