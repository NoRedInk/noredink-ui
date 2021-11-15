module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Html.Styled as Html
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.SortableTable.V2 as SortableTable


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
example : Example State Msg
example =
    { name = "SortableTable"
    , version = 2
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \{ sortState } ->
            let
                config =
                    { updateMsg = SetSortState
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
                        , SortableTable.custom
                            { id = Coins
                            , header = Html.text "Coins"
                            , view = .coins >> String.fromInt >> Html.text
                            , sorter = SortableTable.simpleSort .coins
                            , width = 125
                            , cellStyles = \_ -> []
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
