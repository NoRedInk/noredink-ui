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
            , Select.view label (Select.value state.selectedValue :: attributes)
                |> Html.Styled.map ChangedTheSelectorValue
            , Text.smallBody
                [ """
                    Note that if the value is bound (and why would you ever make a `Select` where it isn't?)
                    then changing the list of options will not change its value.
                    Furthermore, `Select` will only fire an event when a new value is selected.
                    This means that if the starting value is `Nothing` and there is no `defaultDisplayText` 
                    then you cannot select the first item in the list without first selecting another one.
                    Use the "choices" selector above to get a feel for what that means.
                """
                    |> String.lines
                    |> List.map String.trim
                    |> String.join " "
                    |> Text.markdown
                ]
            , Text.smallBody
                [ ("The current value is "
                    ++ (case state.selectedValue of
                            Just tm ->
                                "`Just " ++ choosableToCodeString tm ++ "`"

                            Nothing ->
                                "`Nothing`"
                       )
                    ++ "."
                  )
                    |> Text.markdown
                ]
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    , selectedValue : Maybe Choosable
    }


{-| -}
init : State
init =
    { control =
        Control.record Settings
            |> Control.field "label" (Control.string "Thematically Incoherent Selector")
            |> Control.field "attributes" initControls
    , selectedValue = Nothing
    }


type alias Settings =
    { label : String
    , attributes : List ( String, Select.Attribute Choosable )
    }


initControls : Control (List ( String, Select.Attribute Choosable ))
initControls =
    ControlExtra.list
        |> ControlExtra.listItem "choices" initChoices
        |> ControlExtra.optionalListItem "hiddenLabel"
            (Control.value ( "Select.hiddenLabel", Select.hiddenLabel ))
        |> ControlExtra.optionalListItem "defaultDisplayText"
            (Control.map
                (\str ->
                    ( "Select.defaultDisplayText \"" ++ str ++ "\""
                    , Select.defaultDisplayText str
                    )
                )
                (Control.string "Select a tortilla-based treat, a 2020 81kg Olympic Weightlifter, or nothing at all")
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


type Choosable
    = Tacos
    | Burritos
    | Enchiladas
    | NixtamalizedCorn
    | LüXiaojun
    | ZacaríasBonnat
    | AntoninoPizzolato
    | HarrisonMaurus
    | TragicSingleton


allTexMex : List Choosable
allTexMex =
    let
        help : List Choosable -> List Choosable
        help list =
            case list of
                [] ->
                    help [ Tacos ]

                Tacos :: _ ->
                    help <| Burritos :: list

                Burritos :: _ ->
                    help <| Enchiladas :: list

                Enchiladas :: _ ->
                    help <| NixtamalizedCorn :: list

                NixtamalizedCorn :: _ ->
                    List.reverse list

                LüXiaojun :: _ ->
                    list

                ZacaríasBonnat :: _ ->
                    list

                AntoninoPizzolato :: _ ->
                    list

                HarrisonMaurus :: _ ->
                    list

                TragicSingleton :: _ ->
                    list
    in
    help []


all81kg2020OlympicWeightlifters : List Choosable
all81kg2020OlympicWeightlifters =
    let
        help : List Choosable -> List Choosable
        help list =
            case list of
                [] ->
                    help [ LüXiaojun ]

                LüXiaojun :: _ ->
                    help <| ZacaríasBonnat :: list

                ZacaríasBonnat :: _ ->
                    help <| AntoninoPizzolato :: list

                AntoninoPizzolato :: _ ->
                    help <| HarrisonMaurus :: list

                HarrisonMaurus :: _ ->
                    List.reverse list

                Tacos :: _ ->
                    list

                Burritos :: _ ->
                    list

                Enchiladas :: _ ->
                    list

                NixtamalizedCorn :: _ ->
                    list

                TragicSingleton :: _ ->
                    list
    in
    help []


choosableToLabel : Choosable -> String
choosableToLabel tm =
    case tm of
        Tacos ->
            "Tacos"

        Burritos ->
            "Burritos"

        Enchiladas ->
            "Enchiladas"

        NixtamalizedCorn ->
            """
                The nixtamalization process was very important in the early Mesoamerican diet,
                as most of the niacin content in unprocessed maize is bound to hemicellulose,
                drastically reducing its bioavailability.
                A population that depends on untreated maize as a staple food risks malnourishment
                and is more likely to develop deficiency diseases such as pellagra, niacin deficiency,
                or kwashiorkor, the absence of certain amino acids that maize is deficient in.
                Maize cooked with lime or other alkali provided bioavailable niacin to Mesoamericans.
                Beans provided the otherwise missing amino acids required to balance maize for complete protein.
            """ |> String.trim |> String.lines |> List.map String.trim |> String.join " "

        LüXiaojun ->
            "Lü Xiaojun"

        ZacaríasBonnat ->
            "Zacarías Bonnat"

        AntoninoPizzolato ->
            "Antonino Pizzolato"

        HarrisonMaurus ->
            "Harrison Maurus"

        TragicSingleton ->
            "Tragic Singleton"


choosableToCodeString : Choosable -> String
choosableToCodeString choosable =
    case choosable of
        Tacos ->
            "Tacos"

        Burritos ->
            "Burritos"

        Enchiladas ->
            "Enchiladas"

        NixtamalizedCorn ->
            "NixtamalizedCorn"

        LüXiaojun ->
            "LüXiaojun"

        ZacaríasBonnat ->
            "ZacaríasBonnat"

        AntoninoPizzolato ->
            "AntoninoPizzolato"

        HarrisonMaurus ->
            "HarrisonMaurus"

        TragicSingleton ->
            "TragicSingleton"


texMexLabel : String
texMexLabel =
    "Tex-Mex"


weightLifterLabel : String
weightLifterLabel =
    "81 kg 2020 Olympic Weightlifters"


toOption : Choosable -> { label : String, value : Choosable }
toOption c =
    { label = choosableToLabel c, value = c }


grouped : List (Select.ChoicesGroup Choosable)
grouped =
    [ { label = texMexLabel, choices = List.map toOption allTexMex }
    , { label = weightLifterLabel, choices = List.map toOption all81kg2020OlympicWeightlifters }
    ]


initChoices : Control ( String, Select.Attribute Choosable )
initChoices =
    let
        toOptionString : Choosable -> String
        toOptionString c =
            "{ value = " ++ choosableToCodeString c ++ ", label = \"" ++ choosableToLabel c ++ "\" } "

        toChoice : List Choosable -> ( String, List { label : String, value : Choosable } )
        toChoice choosables =
            ( """Select.choices
        choosableToLabel
        [ """
                ++ String.join "\n        , " (List.map toOptionString choosables)
                ++ "\n        ]"
            , List.map toOption choosables
            )

        toValue : List Choosable -> Control ( String, Select.Attribute Choosable )
        toValue =
            toChoice >> Tuple.mapSecond (Select.choices choosableToLabel) >> Control.value

        toOptionsString : List Choosable -> String
        toOptionsString choosables =
            "[ "
                ++ (List.map toOptionString choosables |> String.join "\n              , ")
                ++ "\n              ]"
    in
    List.map identity
        [ ( texMexLabel, toValue allTexMex )
        , ( "81 Kg 2020 Olympic Weightlifters", toValue all81kg2020OlympicWeightlifters )
        , ( "Grouped Things"
          , Control.value <|
                ( """Select.groupedChoices
        choosableToLabel
        [ { label = \""""
                    ++ texMexLabel
                    ++ "\"\n          , choices = \n              "
                    ++ toOptionsString allTexMex
                    ++ """
          }
        , { label = \""""
                    ++ weightLifterLabel
                    ++ "\"\n          , choices = \n              "
                    ++ toOptionsString all81kg2020OlympicWeightlifters
                    ++ """
          }
        ]
        , Select.choices
            choosableToLabel
            [ """
                    ++ toOptionString TragicSingleton
                    ++ """ ]"""
                , Select.batch
                    [ Select.groupedChoices choosableToLabel
                        [ { label = texMexLabel, choices = List.map toOption allTexMex }
                        , { label = weightLifterLabel, choices = List.map toOption all81kg2020OlympicWeightlifters }
                        ]
                    , Select.choices choosableToLabel [ toOption TragicSingleton ]
                    ]
                )
          )
        , ( "Unselectable list with only one item", toValue [ TragicSingleton ] )
        ]
        |> Control.choice


{-| -}
type Msg
    = UpdateSettings (Control Settings)
    | ChangedTheSelectorValue Choosable


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ChangedTheSelectorValue newValue ->
            ( { state | selectedValue = Just newValue }, Cmd.none )

        UpdateSettings settings ->
            ( { state | control = settings }, Cmd.none )
