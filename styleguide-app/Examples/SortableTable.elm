module Examples.SortableTable exposing (Msg, State, example, init, update)

{-|

    @docs Msg, State, example, init, update

-}

import Category exposing (Category(..))
import Html.Styled as Html
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.SortableTable.V1 as SortableTable
import Sort.Set as Set exposing (Set)


type Column
    = FirstName
    | LastName
    | Coins


{-| -}
type Msg
    = NoOp
    | SetSortState (SortableTable.State Column)


{-| -}
type alias State =
    { sortState : SortableTable.State Column }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage { sortState } =
    { name = "Nri.Ui.SortableTable.V1"
    , categories = Set.fromList Category.sorter <| List.singleton Tables
    , content =
        let
            config =
                { updateMsg = SetSortState
                , columns =
                    [ SortableTable.string
                        { id = FirstName
                        , header = "First name"
                        , value = .firstName
                        , width = 125
                        }
                    , SortableTable.string
                        { id = LastName
                        , header = "Last name"
                        , value = .lastName
                        , width = 125
                        }
                    , SortableTable.custom
                        { id = Coins
                        , header = Html.text "Coins"
                        , view = .coins >> String.fromInt >> Html.text
                        , sorter = SortableTable.simpleSort .coins
                        , width = 125
                        }
                    ]
                }

            data =
                [ { firstName = "First1", lastName = "Last1", coins = 1 }
                , { firstName = "First2", lastName = "Last2", coins = 2 }
                , { firstName = "First3", lastName = "Last3", coins = 3 }
                , { firstName = "First4", lastName = "Last4", coins = 4 }
                , { firstName = "First5", lastName = "Last5", coins = 5 }
                ]
        in
        [ Heading.h3 [] [ Html.text "With sortable headers" ]
        , SortableTable.view config sortState data
        , Heading.h3 [] [ Html.text "Loading" ]
        , SortableTable.viewLoading config sortState
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { sortState = SortableTable.init FirstName }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        SetSortState sortState ->
            ( { state | sortState = sortState }, Cmd.none )
