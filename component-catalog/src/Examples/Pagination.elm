module Examples.Pagination exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Pagination.V1 as Pagination
import Nri.Ui.Spacing.V1 as Spacing


moduleName : String
moduleName =
    "Pagination"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Navigation ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Fonts.baseFont
                , Css.fontSize (Css.px 15)
                ]
            ]
            [ previewFakeLink "Previous" []
            , previewFakeLink "1" []
            , previewFakeLink "2" [ Css.backgroundColor Colors.glacier, Css.color Colors.azureDark ]
            , previewFakeLink "3" []
            , previewFakeLink "Next" []
            ]
        ]
    , about = []
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings

                pages =
                    List.range 1 settings.pages
                        |> List.map
                            (\i ->
                                ( Code.record
                                    [ ( "onClick", "SelectPage " ++ String.fromInt i )
                                    , ( "href", Code.string ("#page" ++ String.fromInt i) )
                                    ]
                                , { onClick = SelectPage i
                                  , href = "#page" ++ String.fromInt i
                                  }
                                )
                            )
                        |> List.unzip
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = model.settings
                , mainType = Just "RootHtml.Html Msg"
                , extraCode = [ Code.unionType "Msg" [ "SelectPage Int" ] ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.listMultiline (Tuple.first pages)
                                        1
                                    ++ Code.newlineWithIndent 1
                                    ++ String.fromInt model.currentPage
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Pagination.view (Tuple.second pages) model.currentPage
            ]
    }


previewFakeLink : String -> List Css.Style -> Html msg
previewFakeLink word extraStyles =
    span
        [ css
            [ Css.color Colors.azure
            , Css.padding (Css.px 2)
            , Css.minWidth (Css.px 20)
            , Css.borderRadius (Css.px 4)
            , Css.textAlign Css.center
            , Css.batch extraStyles
            ]
        ]
        [ text word ]


{-| -}
type alias State =
    { currentPage : Int
    , settings : Control Settings
    }


init : State
init =
    { currentPage = 0
    , settings = controlSettings
    }


type alias Settings =
    { pages : Int
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Page count" (Control.int 6)


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | SelectPage Int


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        UpdateControls settings ->
            ( { model | settings = settings }, Cmd.none )

        SelectPage i ->
            ( { model | currentPage = i }, Cmd.none )
