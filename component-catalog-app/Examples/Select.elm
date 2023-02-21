module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Select.V9 as Select exposing (Choice)
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "Select"


version : Int
version =
    9


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
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
        \ellieLinkConfig state ->
            let
                label =
                    (Control.currentValue state.control).label

                ( attributesCode, attributes ) =
                    List.unzip (Control.currentValue state.control).attributes
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.control
                , mainType = Just "RootHtml.Html String"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                "Select.view \""
                                    ++ label
                                    ++ "\""
                                    ++ "\n    [ "
                                    ++ String.join "\n    , " attributesCode
                                    ++ "\n    ] "
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Select.view label attributes
                |> Html.Styled.map ChangedTheSelectorValue
            , Text.mediumBody
                [ Text.plaintext <|
                    """
                    Note that if the value is bound (and why would you ever make a Select where it isn't?)
                    then changing the list of options will not change its value.
                    Furthermore, Select will only fire an event when a new, non-default value is selected.
                    The current value is
                """
                        ++ getABetterNameToString state.selectedValue
                        ++ "."
                        ++ """
                   if you change the "choices" above to "Overflowing text choices" there is no way to change the current value
                   to Zaphod (the value of the single option with a very long name).
                """
                ]
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    , selectedValue : GetABetterName
    }


{-| -}
init : State
init =
    { control =
        Control.record Settings
            |> Control.field "label" (Control.string "Tortilla Selector")
            |> Control.field "attributes" initControls
    , selectedValue = TexMex Tacos
    }


type alias Settings =
    { label : String
    , attributes : List ( String, Select.Attribute GetABetterName )
    }


initControls : Control (List ( String, Select.Attribute GetABetterName ))
initControls =
    ControlExtra.list
        |> ControlExtra.listItem "choices"
            (Control.map
                (\( code, choices ) ->
                    ( "Select.choices getABetterNameToString" ++ code
                    , Select.choices getABetterNameToString choices
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
                [ ( "max-width: 300px"
                  , Control.value
                        ( "Select.containerCss [ Css.maxWidth (Css.px 300) ]"
                        , Select.containerCss [ Css.maxWidth (Css.px 300) ]
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
        |> CommonControls.guidanceAndErrorMessage
            { moduleName = moduleName
            , guidance = Select.guidance
            , errorMessage = Select.errorMessage
            , message = "The right item must be selected."
            }
        |> ControlExtra.optionalListItem "disabled"
            (Control.value ( "Select.disabled", Select.disabled ))
        |> ControlExtra.optionalListItem "loading"
            (Control.value ( "Select.loading", Select.loading ))
        |> CommonControls.icon moduleName Select.icon


type TexMex
    = Tacos
    | Burritos
    | Enchiladas


type Hitchhiker
    = Zaphod


type GetABetterName
    = TexMex TexMex
    | Hitchhiker Hitchhiker


getABetterNameToString : GetABetterName -> String
getABetterNameToString gabn =
    case gabn of
        TexMex Tacos ->
            "Tacos"

        TexMex Burritos ->
            "Burritos"

        TexMex Enchiladas ->
            "Enchiladas"

        Hitchhiker Zaphod ->
            "Zaphod"


initChoices : Control ( String, List (Choice GetABetterName) )
initChoices =
    Control.choice
        [ ( "Short choices"
          , ( """
        [ { label = "Tacos", value = TexMex Tacos }
        , { label = "Burritos", value = TexMex Burritos }
        , { label = "Enchiladas", value = TexMex Enchiladas }
        ]"""
            , [ { label = "Tacos", value = TexMex Tacos }
              , { label = "Burritos", value = TexMex Burritos }
              , { label = "Enchiladas", value = TexMex Enchiladas }
              ]
            )
                |> Control.value
          )
        , ( "Overflowing text choices"
          , ( """
        [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = Hitchhiker Zaphod }
        ]"""
            , [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = Hitchhiker Zaphod }
              ]
            )
                |> Control.value
          )
        ]


{-| -}
type Msg
    = UpdateSettings (Control Settings)
    | ChangedTheSelectorValue GetABetterName


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ChangedTheSelectorValue newValue ->
            ( { state | selectedValue = newValue }, Cmd.none )

        UpdateSettings settings ->
            ( { state | control = settings }, Cmd.none )
