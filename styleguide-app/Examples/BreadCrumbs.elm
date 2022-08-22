module Examples.BreadCrumbs exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes exposing (css, href)
import Nri.Ui.BreadCrumbs.V1 as BreadCrumbs exposing (BreadCrumbs)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Table.V6 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type alias State =
    Control Settings


moduleName : String
moduleName =
    "BreadCrumbs"


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
    , preview =
        [ previewContainer [ previewText "🏠 Home" ]
        , previewContainer [ previewText "🏠 Home", previewArrowRight, previewText "🟠 Category " ]
        , previewContainer [ previewText "🏠", previewArrowRight, previewText "🟠", previewArrowRight, previewText "🟣 Sub-Category " ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                breadCrumbs : BreadCrumbs String
                breadCrumbs =
                    Tuple.second (Control.currentValue state).breadCrumbs
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = "RootHtml.Html msg"
                , extraCode = [ "import Html.Styled.Attributes exposing (href)" ]
                , toExampleCode = \settings -> [ { sectionName = moduleName ++ ".view", code = viewExampleCode settings } ]
                }
            , section [ css [ Css.margin2 (Css.px 20) Css.zero ] ]
                [ Heading.h2 [ Heading.plaintext "Example" ]
                , viewExample breadCrumbs
                ]
            , Table.view
                [ Table.string
                    { header = "Name"
                    , value = .name
                    , width = Css.pct 15
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "About"
                    , value = .about
                    , width = Css.px 200
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Result"
                    , value = \{ result } -> result breadCrumbs
                    , width = Css.px 50
                    , cellStyles = always []
                    , sort = Nothing
                    }
                ]
                [ { name = "headerId"
                  , about = "When changing routes in a SPA, moving focus to the heading of the new page orients screenreader users to the new location."
                  , result = BreadCrumbs.headerId
                  }
                , { name = "toPageTitle"
                  , about = "When changing routes in a SPA, the HTML title of the page should be updated to match the new route."
                  , result = BreadCrumbs.toPageTitle
                  }
                , { name = "toPageTitleWithSecondaryBreadCrumbs"
                  , about = "(Tessa doesn't know why this helper exists/why it includes less context than `toPageTitle` does)"
                  , result = BreadCrumbs.toPageTitleWithSecondaryBreadCrumbs
                  }
                ]
            ]
    }


previewContainer : List (Html msg) -> Html msg
previewContainer =
    span
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Fonts.baseFont
            , Css.fontSize (Css.px 10)
            , Css.fontWeight (Css.int 600)
            , Css.color Colors.navy
            ]
        ]


previewText : String -> Html msg
previewText name =
    span [ css [ Css.margin (Css.px 2) ] ] [ text name ]


previewArrowRight : Html msg
previewArrowRight =
    UiIcon.arrowRight
        |> Svg.withColor Colors.gray75
        |> Svg.withHeight (Css.px 10)
        |> Svg.withWidth (Css.px 8)
        |> Svg.withCss [ Css.flexShrink Css.zero ]
        |> Svg.toHtml


viewExampleCode : Settings -> String
viewExampleCode settings =
    "BreadCrumbs.view"
        ++ Code.record
            [ ( "aTagAttributes", "\\route -> [ href route ]" )
            , ( "isCurrentRoute", "\\route -> route == " ++ Code.string "/current/route" )
            , ( "label", Code.string "breadcrumbs" )
            ]
        ++ Code.newlineWithIndent 1
        ++ Tuple.first settings.breadCrumbs


viewExample : BreadCrumbs String -> Html msg
viewExample breadCrumbs =
    BreadCrumbs.view
        { aTagAttributes = \route -> [ href route ]
        , isCurrentRoute = \route -> route == "/current/route"
        , label = "breadcrumbs example"
        }
        breadCrumbs


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl control ->
            ( control, Cmd.none )


type alias Settings =
    { breadCrumbs : ( String, BreadCrumbs String )
    }


init : Control Settings
init =
    Control.map Settings controlBreadCrumbs


controlBreadCrumbs : Control ( String, BreadCrumbs String )
controlBreadCrumbs =
    Control.map (\f -> f Nothing) (controlBreadCrumbs_ 1)


controlBreadCrumbs_ : Int -> Control (Maybe ( String, BreadCrumbs String ) -> ( String, BreadCrumbs String ))
controlBreadCrumbs_ index =
    Control.record (composeBreadCrumbs index)
        |> Control.field "icon" (Control.maybe False CommonControls.uiIcon)
        |> Control.field "iconStyle"
            (CommonControls.choice moduleName
                [ ( "Default", BreadCrumbs.Default )
                , ( "Circled", BreadCrumbs.Circled )
                ]
            )
        |> Control.field "text" (ControlExtra.string ("Category " ++ String.fromInt index))
        |> Control.field ("category " ++ String.fromInt (index + 1))
            (Control.maybe False
                (Control.lazy
                    (\() -> controlBreadCrumbs_ (index + 1))
                )
            )


composeBreadCrumbs :
    Int
    -> Maybe ( String, Svg )
    -> ( String, BreadCrumbs.IconStyle )
    -> ( String, String )
    -> Maybe (Maybe ( String, BreadCrumbs String ) -> ( String, BreadCrumbs String ))
    -> (Maybe ( String, BreadCrumbs String ) -> ( String, BreadCrumbs String ))
composeBreadCrumbs index icon ( iconStyleStr, iconStyle ) ( textStr, text ) after maybeBase =
    let
        breadCrumb =
            { icon = Maybe.map Tuple.second icon
            , iconStyle = iconStyle
            , text = text
            , id = "breadcrumb-id-" ++ String.fromInt index
            , route = "/breadcrumb" ++ String.fromInt index
            }

        breadCrumbStr =
            Code.recordMultiline
                [ ( "icon", Code.maybeString (Maybe.map Tuple.first icon) )
                , ( "iconStyle", iconStyleStr )
                , ( "text", textStr )
                , ( "id", Code.string ("breadcrumb-id-" ++ String.fromInt index) )
                , ( "route", Code.string ("/breadcrumb" ++ String.fromInt index) )
                ]
                2

        newBase =
            case maybeBase of
                Just ( baseStr, base ) ->
                    ( "(BreadCrumbs.after "
                        ++ baseStr
                        ++ ControlView.withIndentLevel 2
                        ++ breadCrumbStr
                        ++ (Code.newlineWithIndent 1 ++ ")")
                    , BreadCrumbs.after base breadCrumb
                    )

                Nothing ->
                    ( "(BreadCrumbs.init "
                        ++ ControlView.withIndentLevel 2
                        ++ breadCrumbStr
                        ++ (Code.newlineWithIndent 1 ++ ")")
                    , BreadCrumbs.init breadCrumb
                    )
    in
    Maybe.map (\f -> f (Just newBase)) after |> Maybe.withDefault newBase
