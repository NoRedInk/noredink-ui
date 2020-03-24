module Examples.Table exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update
-}

import Category exposing (Category(..))
import Css exposing (..)
import Html.Styled as Html
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Button.V5 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Table.V5 as Table


{-| -}
type Msg
    = NoOp


{-| -}
type alias State =
    ()


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Table.V5"
    , category = Tables
    , content =
        let
            columns =
                [ Table.string
                    { header = "First Name"
                    , value = .firstName
                    , width = calc (pct 50) minus (px 250)
                    , cellStyles = always []
                    }
                , Table.string
                    { header = "Last Name"
                    , value = .lastName
                    , width = calc (pct 50) minus (px 250)
                    , cellStyles = always []
                    }
                , Table.string
                    { header = "# Submitted"
                    , value = .submitted >> String.fromInt
                    , width = px 125
                    , cellStyles =
                        \value ->
                            if value.submitted < 5 then
                                [ backgroundColor Colors.redLight
                                , textAlign center
                                ]

                            else
                                [ textAlign center ]
                    }
                , Table.custom
                    { header =
                        Html.text "Actions"
                    , width = px 250
                    , view =
                        \_ ->
                            Button.button
                                { size = Button.Small
                                , style = Button.Primary
                                , onClick = NoOp
                                , width = Button.WidthUnbounded
                                }
                                { label = "Action"
                                , state = Button.Enabled
                                , icon = Nothing
                                }
                    , cellStyles = always []
                    }
                ]

            data =
                [ { firstName = "First1", lastName = "Last1", submitted = 10 }
                , { firstName = "First2", lastName = "Last2", submitted = 0 }
                , { firstName = "First3", lastName = "Last3", submitted = 3 }
                , { firstName = "First4", lastName = "Last4", submitted = 15 }
                , { firstName = "First5", lastName = "Last5", submitted = 8 }
                ]
        in
        [ Heading.h3 [] [ Html.text "With header" ]
        , Table.view columns data
        , Heading.h3 [] [ Html.text "Without header" ]
        , Table.viewWithoutHeader columns data
        , Heading.h3 [] [ Html.text "With additional cell styles" ]
        , Table.view columns data
        , Heading.h3 [] [ Html.text "Loading" ]
        , Table.viewLoading columns
        , Heading.h3 [] [ Html.text "Loading without header" ]
        , Table.viewLoadingWithoutHeader columns
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
