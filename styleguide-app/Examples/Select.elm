module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V8 as Select exposing (Choice)


{-| -}
example : Example State Msg
example =
    { name = "Select"
    , version = 8
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport = []
    , preview =
        [ Select.view "Label" [ Select.custom [ Key.tabbable False ] ]
        , Select.view "Hidden label"
            [ Select.hiddenLabel
            , Select.defaultDisplayText "Hidden label"
            , Select.custom [ Key.tabbable False ]
            ]
        ]
    , view =
        \state ->
            let
                label =
                    Control.currentValue state.label

                ( attributesCode, attributes ) =
                    List.unzip (Control.currentValue state.attributes)
            in
            [ Control.view UpdateLabel state.label
                |> Html.Styled.fromUnstyled
            , Control.view UpdateAttributes state.attributes
                |> Html.Styled.fromUnstyled
            , Html.Styled.div
                [ css [ Css.displayFlex, Css.alignItems Css.flexStart ]
                ]
                [ Html.Styled.code
                    [ css
                        [ Css.display Css.block
                        , Css.margin2 (Css.px 20) Css.zero
                        , Css.whiteSpace Css.preWrap
                        , Css.flexBasis (Css.px 500)
                        ]
                    ]
                    [ Html.Styled.text <|
                        "Select.view \""
                            ++ label
                            ++ "\""
                            ++ "\n    [ "
                            ++ String.join "\n    , " attributesCode
                            ++ "\n    ] "
                    ]
                , Select.view label attributes
                    |> Html.Styled.map ConsoleLog
                ]
            ]
    }


{-| -}
type alias State =
    { label : Control String
    , attributes : Control Settings
    }


{-| -}
init : State
init =
    { label = Control.string "Tortilla Selector"
    , attributes = initControls
    }


type alias Settings =
    List ( String, Select.Attribute String )


initControls : Control Settings
initControls =
    ControlExtra.list
        |> ControlExtra.listItem "choices"
            (Control.map
                (\( code, choices ) ->
                    ( "Select.choices identity" ++ code
                    , Select.choices identity choices
                    )
                )
                initChoices
            )
        |> ControlExtra.optionalListItem "hiddenLabel"
            (Control.value ( "Select.hiddenLabel", Select.hiddenLabel ))
        |> ControlExtra.optionalListItem "defaultDisplayText"
            (Control.map
                (\str ->
                    ( "Select.defaultDisplayText \"" ++ str ++ "\""
                    , Select.defaultDisplayText str
                    )
                )
                (Control.string "Select a tasty tortilla based treat!")
            )
        |> ControlExtra.optionalListItem "containerCss"
            (Control.choice
                [ ( "flex-basis: 300px"
                  , Control.value
                        ( "Select.containerCss [ Css.flexBasis (Css.px 300) ]"
                        , Select.containerCss [ Css.flexBasis (Css.px 300) ]
                        )
                  )
                , ( "background-color: lichen"
                  , Control.value
                        ( "Select.containerCss [ Css.backgroundColor Colors.lichen ]"
                        , Select.containerCss [ Css.backgroundColor Colors.lichen ]
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "noMargin"
            (Control.map
                (\bool ->
                    ( "Select.noMargin " ++ Debug.toString bool
                    , Select.noMargin bool
                    )
                )
                (Control.bool True)
            )
        |> ControlExtra.optionalListItem "errorIf"
            (Control.map
                (\bool ->
                    ( "Select.errorIf " ++ Debug.toString bool
                    , Select.errorIf bool
                    )
                )
                (Control.bool True)
            )
        |> ControlExtra.optionalListItem "errorMessage"
            (Control.map
                (\str ->
                    ( "Select.errorMessage (Just \"" ++ str ++ "\")"
                    , Select.errorMessage (Just str)
                    )
                )
                (Control.string "The right item must be selected.")
            )
        |> ControlExtra.optionalListItem "guidance"
            (Control.map
                (\str ->
                    ( "Select.guidance \"" ++ str ++ "\""
                    , Select.guidance str
                    )
                )
             <|
                Control.string "The right item must be selected."
            )


initChoices : Control ( String, List (Choice String) )
initChoices =
    Control.choice
        [ ( "Short choices"
          , ( """
        [ { label = "Tacos", value = "tacos" }
        , { label = "Burritos", value = "burritos" }
        , { label = "Enchiladas", value = "enchiladas" }
        ]"""
            , [ { label = "Tacos", value = "tacos" }
              , { label = "Burritos", value = "burritos" }
              , { label = "Enchiladas", value = "enchiladas" }
              ]
            )
                |> Control.value
          )
        , ( "Overflowing text choices"
          , ( """
        [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = "design-coastlines" }
        ]"""
            , [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = "design-coastlines" }
              ]
            )
                |> Control.value
          )
        ]


{-| -}
type Msg
    = ConsoleLog String
    | UpdateLabel (Control String)
    | UpdateAttributes (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "SelectExample" message
            in
            ( state, Cmd.none )

        UpdateLabel label ->
            ( { state | label = label }
            , Cmd.none
            )

        UpdateAttributes attributes ->
            ( { state | attributes = attributes }
            , Cmd.none
            )
