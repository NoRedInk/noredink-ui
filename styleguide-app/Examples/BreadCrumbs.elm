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
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
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
    2


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
                    case (Control.currentValue state).breadCrumbs of
                        ( _, _, val ) ->
                            val
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = Just "RootHtml.Html msg"
                , extraCode = [ "import Html.Styled.Attributes exposing (href)" ]
                , renderExample =
                    \body ->
                        Code.newlineWithIndent 1
                            ++ "toUnstyled view"
                            ++ Code.newlines
                            ++ body
                , toExampleCode =
                    \settings ->
                        [ { sectionName = moduleName ++ ".view"
                          , code = viewExampleCode settings
                          }
                        ]
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
    let
        ( currentCrumb, crumbDefinitions, _ ) =
            settings.breadCrumbs
    in
    crumbDefinitions
        ++ Code.newlines
        ++ (Code.var "view" 1 <|
                "BreadCrumbs.view"
                    ++ Code.recordMultiline
                        [ ( "aTagAttributes", "\\route -> [ href route ]" )
                        , ( "isCurrentRoute", "\\route -> route == " ++ Code.string "/current/route" )
                        , ( "label", Code.string "breadcrumbs" )
                        ]
                        2
                    ++ Code.newlineWithIndent 2
                    ++ currentCrumb
           )


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
    { breadCrumbs : ConfigurableBreadCrumbs
    }


init : Control Settings
init =
    Control.map Settings controlBreadCrumbs


type alias ConfigurableBreadCrumbs =
    ( String, String, BreadCrumbs String )


controlBreadCrumbs : Control ConfigurableBreadCrumbs
controlBreadCrumbs =
    Control.map (\f -> f Nothing) (controlBreadCrumbs_ 1)


controlBreadCrumbs_ : Int -> Control (Maybe ConfigurableBreadCrumbs -> ConfigurableBreadCrumbs)
controlBreadCrumbs_ index =
    Control.record (composeBreadCrumbs index)
        |> Control.field "text" (Control.string ("Category " ++ String.fromInt index))
        |> Control.field "optional attributes"
            (Control.maybe False
                (ControlExtra.list
                    |> CommonControls.iconNotCheckedByDefault moduleName BreadCrumbs.icon
                    |> ControlExtra.optionalBoolListItem "iconCircledStyle" ( "BreadCrumbs.iconCircledStyle True", BreadCrumbs.iconCircledStyle True )
                )
            )
        |> Control.field ("category " ++ String.fromInt (index + 1))
            (Control.maybe False
                (Control.lazy
                    (\() -> controlBreadCrumbs_ (index + 1))
                )
            )


composeBreadCrumbs :
    Int
    -> String
    -> Maybe (List ( String, BreadCrumbs.BreadCrumbAttribute String ))
    -> Maybe (Maybe ConfigurableBreadCrumbs -> ConfigurableBreadCrumbs)
    -> (Maybe ConfigurableBreadCrumbs -> ConfigurableBreadCrumbs)
composeBreadCrumbs index text attributes after maybeBase =
    let
        ( configStr, config ) =
            ( Code.recordMultiline
                [ ( "text", Code.string text )
                , ( "id", Code.string ("breadcrumb-id-" ++ String.fromInt index) )
                , ( "route", Code.string ("/breadcrumb" ++ String.fromInt index) )
                ]
                2
            , { text = text
              , id = "breadcrumb-id-" ++ String.fromInt index
              , route = "/breadcrumb" ++ String.fromInt index
              }
            )

        ( optionalAttributesStr, optionalAttributes ) =
            List.unzip (Maybe.withDefault [] attributes)

        varName =
            "breadcrumb" ++ String.fromInt index

        newBase =
            case maybeBase of
                Just ( baseVar, baseStr, base ) ->
                    ( varName
                    , baseStr
                        ++ Code.newlines
                        ++ (Code.var varName 1 <|
                                "BreadCrumbs.after "
                                    ++ baseVar
                                    ++ configStr
                                    ++ Code.listMultiline optionalAttributesStr 2
                           )
                    , BreadCrumbs.after base config optionalAttributes
                    )

                Nothing ->
                    ( varName
                    , Code.var varName 1 <|
                        "BreadCrumbs.init "
                            ++ configStr
                            ++ Code.listMultiline optionalAttributesStr 2
                    , BreadCrumbs.init config optionalAttributes
                    )
    in
    Maybe.map (\f -> f (Just newBase)) after |> Maybe.withDefault newBase
