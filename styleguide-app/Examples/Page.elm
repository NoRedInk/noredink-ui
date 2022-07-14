module Examples.Page exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Http
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Page.V3 as Page exposing (DefaultPage, RecoveryText(..))


{-| -}
type alias State =
    Control Settings


{-| -}
type Msg
    = ShowItWorked String
    | UpdateSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowItWorked message ->
            ( Debug.log "Clicked: " message |> always model, Cmd.none )

        UpdateSettings settings ->
            ( settings, Cmd.none )


moduleName : String
moduleName =
    "Page"


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = controlSettings
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Page component to give styleguide users a sense of what the
          -- component might look like
          Html.div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.flexDirection Css.column
                , Css.backgroundColor Colors.white
                , Css.borderRadius (Css.px 4)
                , Css.padding (Css.px 20)
                ]
            ]
            [ Html.div [ css [ Css.fontSize (Css.px 40) ] ] [ Html.text "😵" ]
            , Html.p
                [ css
                    [ Css.color Colors.navy
                    , Fonts.baseFont
                    , Css.fontWeight Css.bold
                    , Css.textAlign Css.center
                    , Css.margin Css.zero
                    ]
                ]
                [ Html.text "There was a problem!" ]
            ]
        ]
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = model
                , mainType = "RootHtml.Html msg"
                , extraImports = [ "import Http" ]
                , toExampleCode = \_ -> []
                }
            , viewExample settings.page settings.recoveryText
            ]
    }


viewExample :
    ( String, Page.DefaultPage Msg -> Html Msg )
    -> RecoveryText
    -> Html Msg
viewExample ( viewName, view ) recoveryText =
    Html.div
        [ css
            [ Css.marginTop (Css.px 20)
            , Css.borderTop3 (Css.px 2) Css.solid Colors.gray96
            , Css.paddingTop (Css.px 20)
            , Css.marginBottom (Css.px 20)
            ]
        ]
        [ Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text viewName ]
        , Html.code []
            [ Html.text <|
                viewName
                    ++ " {  link = msg, recoveryText = "
                    ++ Debug.toString recoveryText
                    ++ " }"
            ]
        , view { link = ShowItWorked viewName, recoveryText = recoveryText }
        ]


type alias Settings =
    { page : ( String, DefaultPage Msg -> Html Msg )
    , recoveryText : RecoveryText
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "page" controlPageType
        |> Control.field "recoveryText" initRecoveryText


controlPageType : Control ( String, DefaultPage Msg -> Html Msg )
controlPageType =
    let
        choiceWithModuleName name value =
            ( name, Control.value ( moduleName ++ "." ++ name, value ) )
    in
    [ ( "httpError"
      , Control.map
            (\err ->
                ( moduleName ++ ".httpError httpError -- Use Http.error"
                , Page.httpError err
                )
            )
            CommonControls.httpError
      )
    , choiceWithModuleName "broken" Page.broken
    , ( "blockedV4"
      , Control.value
            ( "Page.blockedV4 " ++ Code.string "Error message details"
            , Page.blockedV4 "Error message details"
            )
      )
    , choiceWithModuleName "notFound" Page.notFound
    , choiceWithModuleName "noPermission" Page.noPermission
    , choiceWithModuleName "loggedOut" Page.loggedOut
    , choiceWithModuleName "timeOut" Page.timeOut
    , choiceWithModuleName "networkError" Page.networkError
    ]
        |> Control.choice


initRecoveryText : Control RecoveryText
initRecoveryText =
    Control.choice
        [ ( "Page.Reload", Control.value Page.Reload )
        , ( "Page.ReturnTo", Control.map Page.ReturnTo (Control.string "Home") )
        , ( "Page.Custom", Control.map Custom (Control.string "Hit the road, Jack") )
        ]
