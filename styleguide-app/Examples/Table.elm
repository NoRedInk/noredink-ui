module Examples.Table exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update
-}

import Css exposing (..)
import Html
import Html.Styled
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V1 as Button
import Nri.Ui.Table.V2 as Table


{-| -}
type Msg
    = NoOp


{-| -}
type alias State =
    ()


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Table.elm"
    , category = Layout
    , content =
        let
            columns =
                [ Table.string
                    { header = "First Name"
                    , value = .firstName
                    , width = calc (pct 50) minus (px 125)
                    }
                , Table.string
                    { header = "Last Name"
                    , value = .lastName
                    , width = calc (pct 50) minus (px 125)
                    }
                , Table.custom
                    { header =
                        Html.text "Actions"
                            |> Html.Styled.fromUnstyled
                    , width = px 250
                    , view =
                        \_ ->
                            Button.button
                                { size = Button.Small
                                , style = Button.Primary
                                , onClick = NoOp
                                }
                                { label = "Action"
                                , state = Button.Enabled
                                }
                                |> Html.Styled.fromUnstyled
                    }
                ]

            data =
                [ { firstName = "First1", lastName = "Last1" }
                , { firstName = "First2", lastName = "Last2" }
                , { firstName = "First3", lastName = "Last3" }
                , { firstName = "First4", lastName = "Last4" }
                , { firstName = "First5", lastName = "Last5" }
                ]
        in
            [ Html.Styled.toUnstyled Table.keyframeStyles
            , Html.h4 [] [ Html.text "With header" ]
            , Table.view columns data
                |> Html.Styled.toUnstyled
            , Html.h4 [] [ Html.text "Without header" ]
            , Table.viewWithoutHeader columns data
                |> Html.Styled.toUnstyled
            , Html.h4 [] [ Html.text "Loading" ]
            , Table.viewLoading columns
                |> Html.Styled.toUnstyled
            , Html.h4 [] [ Html.text "Loading without header" ]
            , Table.viewLoadingWithoutHeader columns
                |> Html.Styled.toUnstyled
            ]
                |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    ()


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )
