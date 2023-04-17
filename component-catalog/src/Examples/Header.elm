module Examples.Header exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled.Role as Role
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Header.V1 as Header
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Select.V9 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "Header"


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
    , state = init Nothing
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = [ viewPreview ]
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
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
                    \settings ->
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view "
                                    ++ Code.list (List.map Tuple.first settings)
                                    ++ Code.newlineWithIndent 1
                                    ++ Code.commentInline "See the BreadCrumbs example to more fully customize the main data in the Header"
                                    ++ Code.recordMultiline
                                        [ ( "breadCrumbs"
                                          , Code.newlineWithIndent 2
                                                ++ Code.fromModule "BreadCrumbs" "init "
                                                ++ Code.record
                                                    [ ( "id", Code.string "page-header" )
                                                    , ( "text", Code.string "Page" )
                                                    , ( "route", "()" )
                                                    ]
                                                ++ Code.list []
                                          )
                                        , ( "isCurrentRoute", Code.always "True" )
                                        ]
                                        1
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Header.view
                (Header.breadCrumbsLabel "header example breadcrumbs" :: attributes)
                { breadCrumbs =
                    BreadCrumbs.init
                        { id = "page-header"
                        , text = "Page"
                        , route = ()
                        }
                        []
                , isCurrentRoute = \_ -> True
                }
            ]
    }


viewPreview : Html msg
viewPreview =
    div
        [ css
            [ Css.height (Css.px 80)
            , Css.backgroundColor Colors.white
            , Css.padding (Css.px 8)
            ]
        , Role.presentation
        ]
        [ div
            [ css
                [ Css.backgroundColor Colors.gray96
                ]
            ]
            [ div
                [ css
                    [ Css.color Colors.navy
                    , Css.fontSize (Css.px 10)
                    , Css.fontWeight Css.bold
                    , Fonts.baseFont
                    , Css.padding (Css.px 3)
                    ]
                ]
                [ text "All"
                , UiIcon.arrowRight
                    |> Svg.withWidth (Css.px 8)
                    |> Svg.withHeight (Css.px 8)
                    |> Svg.withColor Colors.gray75
                    |> Svg.withCss [ Css.margin2 Css.zero (Css.px 3) ]
                    |> Svg.toHtml
                , text "Category 1"
                ]
            ]
        ]


{-| -}
type alias State =
    { control : Control Settings
    , selection : Maybe String
    }


init : Maybe String -> State
init selection =
    { control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "extraContent"
                (Control.value
                    ( "Header.extraContent [ Html.text \"…\" ]"
                    , Header.extraContent
                        [ Select.view "Tortilla Selector"
                            [ Select.choices identity
                                [ { label = "Tacos", value = "tacos" }
                                , { label = "Burritos", value = "burritos" }
                                , { label = "Enchiladas", value = "enchiladas" }
                                ]
                            , Select.value selection
                            ]
                            |> map Select
                        ]
                    )
                )
            |> ControlExtra.optionalListItem "extraNav"
                (Control.value
                    ( Code.fromModule "Header" "extraNav "
                        ++ Code.string "Resources"
                        ++ Code.listMultiline
                            [ Code.fromModule "ClickableText" "link " ++ Code.string "Zendesk" ++ " []"
                            , Code.fromModule "ClickableText" "link " ++ Code.string "FAQ" ++ " []"
                            , Code.fromModule "ClickableText" "link " ++ Code.string "About" ++ " []"
                            ]
                            2
                    , Header.extraNav "Resources"
                        [ ClickableText.link "Zendesk" []
                        , ClickableText.link "FAQ" []
                        , ClickableText.link "About" []
                        ]
                    )
                )
            |> ControlExtra.optionalListItem "description"
                (Control.map
                    (\value ->
                        ( "Header.description " ++ Code.string value
                        , Header.description value
                        )
                    )
                    (Control.string "This page has some good content.")
                )
            |> ControlExtra.optionalListItem "customPageWidth"
                (Control.map
                    (\width ->
                        ( "Header.customPageWidth (Css.px" ++ String.fromFloat width ++ ")"
                        , Header.customPageWidth (Css.px width)
                        )
                    )
                    (ControlExtra.float 750)
                )
    , selection = Nothing
    }


type alias Settings =
    List ( String, Header.Attribute () Msg )


{-| -}
type Msg
    = UpdateControl (Control Settings)
    | Select String


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )

        Select value ->
            ( { state | selection = Just value }, Cmd.none )
