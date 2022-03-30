module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (..)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.SortableTable.V2 as SortableTable
import Nri.Ui.Table.V5 as Table
import SolidColor
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


type Column
    = FirstName
    | LastName
    | Coins


{-| -}
type Msg
    = SetSortState (SortableTable.State Column)


{-| -}
type alias State =
    { sortState : SortableTable.State Column }


type Direction
    = Up
    | Down


sortArrow : Direction -> Html msg
sortArrow direction =
    Html.div
        [ css
            [ width (px 8)
            , height (px 6)
            , position relative
            , margin2 (px 1) zero
            ]
        ]
        [ Svg.svg
            [ SvgAttributes.viewBox "0 0 8 6"
            , SvgAttributes.css
                [ position absolute
                , top zero
                , left zero
                , case direction of
                    Up ->
                        Css.batch []

                    Down ->
                        Css.batch [ transform <| rotate (deg 180) ]
                ]
            , Nri.Ui.Colors.V1.gray75
                |> Nri.Ui.Colors.Extra.fromCssColor
                |> SolidColor.toRGBString
                |> SvgAttributes.fill
            ]
            [ Svg.polygon [ SvgAttributes.points "0 6 4 0 8 6 0 6" ] []
            ]
        ]


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
            arrows =
                Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.center
                        ]
                    ]
                    [ sortArrow Up
                    , sortArrow Down
                    ]
        in
        [ Table.view
            [ Table.custom
                { header =
                    div
                        [ css [ displayFlex, justifyContent spaceBetween ]
                        ]
                        [ text "X"
                        , Html.div
                            [ css [ padding (px 2) ] ]
                            [ arrows ]
                        ]
                , view = .a >> Html.text
                , width = px 50
                , cellStyles = always []
                }
            , Table.custom
                { header =
                    div
                        [ css [ displayFlex, justifyContent spaceBetween ]
                        ]
                        [ text "Y"
                        , Html.div
                            [ css [ padding (px 2) ] ]
                            [ arrows ]
                        ]
                , view = .b >> Html.text
                , width = px 50
                , cellStyles = always []
                }
            ]
            [ { a = "Row 1 X"
              , b = "Row 1 Y"
              }
            , { a = "Row 2 X"
              , b = "Row 2 Y"
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
        SetSortState sortState ->
            ( { state | sortState = sortState }, Cmd.none )
