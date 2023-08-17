module Examples.RadioButtonDotless exposing
    ( Msg
    , State
    , example
    )

import Category exposing (Category(..))
import Css
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.RadioButtonDotless.V1 as RadioButtonDotless
import Nri.Ui.Table.V7 as Table
import Platform.Sub as Sub


type alias State =
    { shortRadioValue : Maybe String
    , mediumRadioValue : Maybe String
    , longRadioValue : Maybe String
    }


type Msg
    = ShortRadioSelect String
    | MediumRadioSelect String
    | LongRadioSelect String


moduleName : String
moduleName =
    "RadioButtonDotless"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , about = Guidance.useATACGuide moduleName
    , view = view
    , categories = [ Inputs ]
    , keyboardSupport = []
    }


init : State
init =
    { shortRadioValue = Nothing
    , mediumRadioValue = Nothing
    , longRadioValue = Nothing
    }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ShortRadioSelect value ->
            ( { state | shortRadioValue = Just value }, Cmd.none )

        MediumRadioSelect value ->
            ( { state | mediumRadioValue = Just value }, Cmd.none )

        LongRadioSelect value ->
            ( { state | longRadioValue = Just value }, Cmd.none )


preview : List (Html Never)
preview =
    [ div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "5px"
            ]
        ]
        [ RadioButtonDotless.view
            { label = "Unselected"
            , name = "radio-button-dotless"
            , value = 1
            , valueToString = String.fromInt
            , selectedValue = Just 2
            }
            []
        , RadioButtonDotless.view
            { label = "Selected"
            , name = "radio-button-dotless"
            , value = 2
            , valueToString = String.fromInt
            , selectedValue = Just 2
            }
            []
        ]
    ]


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLink state =
    [ Table.view
        []
        [ Table.custom
            { header = text "Description"
            , view = .description >> text
            , width = Css.px 100
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.px 500
            , cellStyles = always []
            , sort = Nothing
            }
        ]
        [ { description = "Short Buttons"
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.row
                        , Css.property "gap" "5px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 5)
                        ]
                    ]
                    ([ "crept", "past", "scary" ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-short"
                                    , value = label
                                    , valueToString = identity
                                    , selectedValue = state.shortRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth 
                                    , RadioButtonDotless.onSelect ShortRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Medium Buttons"
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.property "gap" "5px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 5)
                        ]
                    ]
                    ([ "The amount of homework eaten"
                     , "Who ate the homework"
                     , "The time the homework was eaten"
                     ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-medium"
                                    , value = String.replace " " "-" label
                                    , valueToString = identity
                                    , selectedValue = state.mediumRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth
                                    , RadioButtonDotless.textAlignLeft
                                    , RadioButtonDotless.onSelect MediumRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Long Buttons"
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.property "gap" "5px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 5)
                        ]
                    ]
                    ([ "Future Perfect--an action will be completed before another future action"
                     , "Past Perfect--an action was completed before another past action"
                     , "Present Perfect--an action started in the past and continues, or it has just been completed"
                     ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-long"
                                    , value = String.replace " " "-" label
                                    , valueToString = identity
                                    , selectedValue = state.longRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth
                                    , RadioButtonDotless.textAlignLeft
                                    , RadioButtonDotless.onSelect LongRadioSelect
                                    ]
                            )
                    )
          }
        ]
    ]
