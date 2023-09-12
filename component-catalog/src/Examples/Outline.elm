module Examples.Outline exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (Color)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Outline.V1 as Outline exposing (KeyedOutline, Outline, RowTheme)
import Nri.Ui.Spacing.V1 as Spacing
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttrs


moduleName : String
moduleName =
    "Outline"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = [ preview ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.control
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Customizable Example"
                          , code =
                                case settings.type_ of
                                    Plain ->
                                        Code.fromModule moduleName "view"
                                            ++ Code.listMultiline
                                                [ Code.fromModule moduleName "row "
                                                    ++ Code.recordMultiline
                                                        [ ( "title", Code.maybe (Maybe.map Code.string settings.title) )
                                                        , ( "content", "text " ++ Code.string settings.content )
                                                        , ( "palette", Tuple.first settings.palette )
                                                        , ( "rows", Code.listMultiline [ "-- …" ] 3 )
                                                        ]
                                                        2
                                                ]
                                                1

                                    Custom ->
                                        "TODO"

                                    Keyed ->
                                        Code.fromModule moduleName "viewKeyed"
                                            ++ Code.listMultiline
                                                [ Code.fromModule moduleName "keyedRow "
                                                    ++ Code.string "base-node"
                                                    ++ Code.recordMultiline
                                                        [ ( "title", Code.maybe (Maybe.map Code.string settings.title) )
                                                        , ( "content", "text " ++ Code.string settings.content )
                                                        , ( "palette", Tuple.first settings.palette )
                                                        , ( "rows", Code.listMultiline [ "-- …" ] 3 )
                                                        ]
                                                        2
                                                ]
                                                1

                                    KeyedWithExtraContent ->
                                        "TODO"
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , case settings.type_ of
                Plain ->
                    Outline.view
                        [ Outline.row
                            { title = settings.title
                            , content = text settings.content
                            , palette = Tuple.second settings.palette
                            , rows = plainRows
                            }
                        ]

                Custom ->
                    Outline.view
                        [ Outline.customRow
                            { title = settings.title
                            , content = text settings.content
                            , palette = Tuple.second settings.palette
                            , rows = plainRows
                            }
                        ]

                Keyed ->
                    Outline.viewKeyed
                        [ Outline.keyedRow "base-node"
                            { title = settings.title
                            , content = text settings.content
                            , palette = Tuple.second settings.palette
                            , rows = keyedRows
                            }
                        ]

                KeyedWithExtraContent ->
                    text "TODO"
            , Heading.h2
                [ Heading.plaintext "Row Themes"
                , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
                ]
            , div
                [ css
                    [ Css.displayFlex
                    , Css.property "gap" "20px"
                    , withMedia [ mobile ] [ Css.flexWrap Css.wrap ]
                    ]
                ]
                [ Outline.view
                    [ Outline.row
                        { title = Just "Outline.view"
                        , content = text "Regular outlines support custom row themes (like this node's theme) as well as predefined themes."
                        , palette =
                            { border = Colors.azure
                            , borderStyle = Css.batch []
                            , background = Colors.gray96
                            }
                        , rows =
                            List.map
                                (\( themeName, theme ) ->
                                    Outline.row
                                        { title = Just themeName
                                        , content = text ""
                                        , palette = theme
                                        , rows = []
                                        }
                                )
                                allRowThemes
                        }
                    ]
                , Outline.viewKeyed
                    [ Outline.keyedRow "base"
                        { title = Just "Outline.viewKeyed"
                        , content = text "Keyed outlines support custom row themes (like this node's theme) as well as predefined themes."
                        , palette =
                            { border = Colors.azure
                            , borderStyle = Css.batch []
                            , background = Colors.gray96
                            }
                        , rows =
                            List.map
                                (\( themeName, theme ) ->
                                    Outline.keyedRow ("row-" ++ themeName)
                                        { title = Just themeName
                                        , content = text ""
                                        , palette = theme
                                        , rows = []
                                        }
                                )
                                allRowThemes
                        }
                    ]
                ]
            ]
    }


plainRows : List (Outline msg)
plainRows =
    [ Outline.row
        { title = Just "Node 2"
        , content = text ""
        , palette = Outline.cornflower
        , rows = []
        }
    , Outline.row
        { title = Just "Node 3"
        , content = text ""
        , palette = Outline.cornflower
        , rows = []
        }
    ]


keyedRows : List (KeyedOutline msg)
keyedRows =
    [ Outline.keyedRow "node-2"
        { title = Just "Node 2"
        , content = text ""
        , palette = Outline.cornflower
        , rows = []
        }
    , Outline.keyedRow "node-3"
        { title = Just "Node 3"
        , content = text ""
        , palette = Outline.cornflower
        , rows = []
        }
    ]


preview : Html msg
preview =
    Svg.svg
        [ SvgAttrs.viewBox "5 0 90 90"
        , SvgAttrs.width "100%"
        , SvgAttrs.height "100%"
        ]
        [ -- Connecting lines
          Svg.path
            [ SvgAttrs.d "M14 41, 14 58 Q14 60 16 60 L 20 60"
            , SvgAttrs.stroke Colors.purple.value
            , SvgAttrs.fill "none"
            ]
            []
        , Svg.path
            [ SvgAttrs.d "M14 29, 14 40 Q14 42 16 42 L 20 42"
            , SvgAttrs.stroke Colors.greenDarkest.value
            , SvgAttrs.fill "none"
            ]
            []

        -- Azure node
        , -- white box
          Svg.rect
            [ SvgAttrs.x "10"
            , SvgAttrs.y "13"
            , SvgAttrs.width "80"
            , SvgAttrs.height "16"
            , SvgAttrs.rx "2"
            , SvgAttrs.fill Colors.white.value
            , SvgAttrs.stroke Colors.azure.value
            , SvgAttrs.strokeWidth "0.5"
            ]
            []
        , -- azure label
          Svg.rect
            [ SvgAttrs.x "5"
            , SvgAttrs.y "10"
            , SvgAttrs.width "16"
            , SvgAttrs.height "6"
            , SvgAttrs.rx "4"
            , SvgAttrs.fill Colors.azure.value
            ]
            []

        -- Green node
        , -- white box
          Svg.rect
            [ SvgAttrs.x "20"
            , SvgAttrs.y "35"
            , SvgAttrs.width "70"
            , SvgAttrs.height "12"
            , SvgAttrs.rx "2"
            , SvgAttrs.fill Colors.white.value
            , SvgAttrs.stroke Colors.greenDarkest.value
            , SvgAttrs.strokeWidth "0.5"
            ]
            []
        , -- green label
          Svg.rect
            [ SvgAttrs.x "16"
            , SvgAttrs.y "32"
            , SvgAttrs.width "14"
            , SvgAttrs.height "6"
            , SvgAttrs.rx "4"
            , SvgAttrs.fill Colors.greenDarkest.value
            ]
            []

        -- Purple node
        , -- white box
          Svg.rect
            [ SvgAttrs.x "20"
            , SvgAttrs.y "54"
            , SvgAttrs.width "70"
            , SvgAttrs.height "12"
            , SvgAttrs.rx "2"
            , SvgAttrs.fill Colors.white.value
            , SvgAttrs.stroke Colors.purple.value
            , SvgAttrs.strokeWidth "0.5"
            ]
            []
        , -- purple label
          Svg.rect
            [ SvgAttrs.x "16"
            , SvgAttrs.y "50"
            , SvgAttrs.width "17"
            , SvgAttrs.height "6"
            , SvgAttrs.rx "4"
            , SvgAttrs.fill Colors.purple.value
            ]
            []
        ]


allRowThemes : List ( String, RowTheme )
allRowThemes =
    [ ( "purpleBordered", Outline.purpleBordered )
    , ( "greenBordered", Outline.greenBordered )
    , ( "blueDashBordered", Outline.blueDashBordered )
    , ( "red", Outline.red )
    , ( "green", Outline.green )
    , ( "aqua", Outline.aqua )
    , ( "turquoise", Outline.turquoise )
    , ( "cornflower", Outline.cornflower )
    , ( "blue", Outline.blue )
    , ( "darkBlue", Outline.darkBlue )
    , ( "purple", Outline.purple )
    , ( "darkGray", Outline.darkGray )
    , ( "gray", Outline.gray )
    , ( "white", Outline.white )
    ]


borderColorList : List ( String, Color )
borderColorList =
    [ ( "azure", Colors.azure )
    , ( "cornflower", Colors.cornflower )
    , ( "gray45", Colors.gray45 )
    , ( "gray75", Colors.gray75 )
    , ( "green", Colors.green )
    , ( "navy", Colors.navy )
    , ( "purple", Colors.purple )
    , ( "red", Colors.red )
    , ( "turquoise", Colors.turquoise )
    ]


backgroundColorList : List ( String, Color )
backgroundColorList =
    [ ( "gray96", Colors.gray96 )
    , ( "aquaLight", Colors.aquaLight )
    , ( "cornflowerLight", Colors.cornflowerLight )
    , ( "frost", Colors.frost )
    , ( "greenLightest", Colors.greenLightest )
    , ( "purpleLight", Colors.purpleLight )
    , ( "redLight", Colors.redLight )
    , ( "turquoiseLight", Colors.turquoiseLight )
    , ( "white", Colors.white )
    ]


{-| -}
type alias State =
    { control : Control Settings
    }


type alias Settings =
    { title : Maybe String
    , content : String
    , palette : ( String, RowTheme )
    , type_ : RowType
    }


init : State
init =
    { control =
        Control.record Settings
            |> Control.field "title" (Control.maybe True (Control.string "Title"))
            |> Control.field "content" (Control.string "")
            |> Control.field "palette"
                (Control.choice
                    (List.map
                        (\( name, value ) ->
                            ( name, Control.value ( Code.fromModule moduleName "." ++ name, value ) )
                        )
                        allRowThemes
                        ++ [ ( "custom", customRowTheme ) ]
                    )
                )
            |> Control.field "type"
                (Control.choice
                    [ ( "plain", Control.value Plain )
                    , ( "custom", Control.value Custom )
                    , ( "keyed", Control.value Keyed )
                    , ( "keyed with extra content", Control.value KeyedWithExtraContent )
                    ]
                )
    }


type RowType
    = Plain
    | Custom
    | Keyed
    | KeyedWithExtraContent


customRowTheme : Control ( String, RowTheme )
customRowTheme =
    Control.record
        (\( a1, a2 ) ( b1, b2 ) ( c1, c2 ) ->
            ( Code.recordMultiline
                [ ( "border", a1 )
                , ( "borderStyle", b1 )
                , ( "background", c1 )
                ]
                3
            , RowTheme a2 b2 c2
            )
        )
        |> Control.field "border" (CommonControls.choice "Colors" borderColorList)
        |> Control.field "borderStyle"
            (Control.choice
                [ ( "none", Control.value ( "Css.batch []", Css.batch [] ) )
                , ( "1px solid"
                  , Control.value
                        ( "Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.solid ]"
                        , Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.solid ]
                        )
                  )
                , ( "1px dashed"
                  , Control.value
                        ( "Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.dashed ]"
                        , Css.batch [ Css.borderWidth (Css.px 1), Css.borderStyle Css.dashed ]
                        )
                  )
                ]
            )
        |> Control.field "background" (CommonControls.choice "Colors" backgroundColorList)


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
