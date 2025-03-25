module Spec.Nri.Ui.SortableTable exposing (spec)

import Expect
import Html.Styled
import Nri.Ui.SortableTable.V6 as SortableTable
import Nri.Ui.Table.V8 exposing (SortDirection(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type Column
    = FirstName
    | LastName


type alias Person =
    { firstName : String
    , lastName : String
    }


columns : List (SortableTable.Column Column Person msg)
columns =
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


entries : List Person
entries =
    [ { firstName = "Bob", lastName = "Stevenson" }
    , { firstName = "Alice", lastName = "Vonnegut" }
    , { firstName = "Charlie", lastName = "Dickens" }
    ]


tableView : SortableTable.Model Column Person (SortableTable.Msg Column) -> Query.Single (SortableTable.Msg Column)
tableView sortModel =
    SortableTable.view { model = sortModel, msgWrapper = identity } []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


sortBy : Column -> SortableTable.Model Column Person msg
sortBy field =
    SortableTable.init field columns (Just entries)


sortByDescending : Column -> SortableTable.Model Column Person msg
sortByDescending field =
    SortableTable.initDescending field columns (Just entries)


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
                    List.sortWith (sorter Ascending) entries
                        |> Expect.equal (List.sortWith (sorter Descending) entries)
            ]
        , describe "simpleSort"
            [ test "sorts ascending" <|
                \() ->
                    let
                        sorter =
                            SortableTable.simpleSort .firstName
                    in
                    List.sortWith (sorter Ascending) entries
                        |> List.map .firstName
                        |> Expect.equal [ "Alice", "Bob", "Charlie" ]
            , test "sorts descending" <|
                \() ->
                    let
                        sorter =
                            SortableTable.simpleSort .firstName
                    in
                    List.sortWith (sorter Descending) entries
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
                    List.sortWith (sorter Ascending) newEntries
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
