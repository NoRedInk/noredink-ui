module Examples.Table exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Button.V5 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Table.V5 as Table


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Table.V5"
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , categories = [ Tables, Layout ]
    , atomicDesignType = AtomicDesignType.Molecule
    , view =
        \() ->
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
                                    , onClick = ()
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
    }
