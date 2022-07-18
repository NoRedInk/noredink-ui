module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (..)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SortableTable.V3 as SortableTable
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Table.V5 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


type Column
    = FirstName
    | LastName
    | Coins
    | ViewButton


{-| -}
type Msg
    = SetSortState (SortableTable.State Column)


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
    , preview =
        let
            header name =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.alignItems Css.center
                        ]
                    ]
                    [ text name
                    , div
                        [ css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            , Css.marginTop (Css.px -4)
                            ]
                        ]
                        [ renderPreviewArrow UiIcon.sortArrow
                        , renderPreviewArrow UiIcon.sortArrowDown
                        ]
                    ]

            renderPreviewArrow : Svg -> Html msg
            renderPreviewArrow arrow =
                arrow
                    |> Svg.withColor Colors.gray75
                    |> Svg.withWidth (Css.px 12)
                    |> Svg.withHeight (Css.px 12)
                    |> Svg.toHtml
        in
        [ Table.view
            [ Table.custom
                { header = header "X"
                , view = .x >> Html.text
                , width = px 50
                , cellStyles = always []
                }
            , Table.custom
                { header = header "Y"
                , view = .y >> Html.text
                , width = px 50
                , cellStyles = always []
                }
            ]
            [ { x = "Row 1 X"
              , y = "Row 1 Y"
              }
            , { x = "Row 2 X"
              , y = "Row 2 Y"
              }
            ]
        ]
    , view =
        \ellieLinkConfig { sortState } ->
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
                            , sorter = Just (SortableTable.simpleSort .coins)
                            , width = 125
                            , cellStyles = \_ -> []
                            }
                        , SortableTable.custom
                            { id = ViewButton
                            , header = Html.text "View"
                            , view =
                                \_ ->
                                    Button.link "View"
                                        [ Button.small
                                        , Button.fillContainerWidth
                                        ]
                            , sorter = Nothing
                            , width = 25
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
            [ Heading.h2 [] [ Html.text "With sortable headers" ]
            , SortableTable.view config sortState data
            , Heading.h2 [] [ Html.text "Loading" ]
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
        SetSortState sortState ->
            ( { state | sortState = sortState }, Cmd.none )
