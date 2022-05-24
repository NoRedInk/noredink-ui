module Examples.BreadCrumbs exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.BreadCrumbs.V1 as BreadCrumbs exposing (BreadCrumb, BreadCrumbs)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import CommonControls


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
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = "RootHtml.Html msg"
                , extraImports = []
                , toExampleCode =
                    \a ->
                        -- TODO: implement
                        -- List { sectionName : String, code : String }
                        []
                }
            , BreadCrumbs.view
                { aTagAttributes = \_ -> []
                , isCurrentRoute = \_ -> False
                }
                (Control.currentValue state).breadCrumbs
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


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl control ->
            ( control, Cmd.none )


type alias Settings =
    { breadCrumbs : BreadCrumbs ()
    }


init : Control Settings
init =
    Control.record Settings
        |> Control.field "BreadCrumbs" controlBreadCrumbs


controlBreadCrumbs : Control (BreadCrumbs ())
controlBreadCrumbs =
    Control.map BreadCrumbs.init controlBreadCrumb


controlBreadCrumb : Control (BreadCrumb ())
controlBreadCrumb =
    Control.record (\icon iconStyle text -> {
            icon = icon,
            iconStyle = iconStyle,
            text =text,
            id = "unique-id",
            route = ()
        })
        |> Control.field "icon" (Control.maybe False (Control.map Tuple.second CommonControls.uiIcon))
        |> Control.field "iconStyle" (Control.choice
            [ ("Default", Control.value BreadCrumbs.Default)
            , ("Circled", Control.value BreadCrumbs.Circled)
        ])
        |> Control.field "text" (Control.string "Home")
