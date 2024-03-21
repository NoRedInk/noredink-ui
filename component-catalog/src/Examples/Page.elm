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
import Guidance
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V5 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Page.V3 as Page exposing (DefaultPage, RecoveryText(..))
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


{-| -}
type alias State =
    Control Settings


{-| -}
type Msg
    = ShowItWorked
    | UpdateSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowItWorked ->
            ( Debug.log "Clicked!" |> always model, Cmd.none )

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
    , init = controlSettings
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Page component to give Component Catalog users a sense of what the
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
    , about =
        [ Guidance.useATACGuide moduleName
        , Text.smallBody
            [ Text.html
                [ Html.text "Learn more about this component from "
                , ClickableText.link "Tessa's demo"
                    [ ClickableText.linkExternal "https://noredink.zoom.us/rec/play/kcM9T-aBbiqQXM2dTKMHES0mq0_mvWqjhzR4LGxpqYp4VNTThZgieu1n2GGzpPtDnVXVBh2KWMQXCfkC.zoJcGlH62xvazLoE?canPlayFromShare=true&from=share_recording_detail&continueMode=true&componentName=rec-play&originRequestUrl=https%3A%2F%2Fnoredink.zoom.us%2Frec%2Fshare%2FKmNvId1mTxpKy42H4lwh5YF5AbSeFNAkpxHW4mVWT8QDKtWOuZoJWTCgHj5MIF2R.Tk4mPcps3J5apO3t%3F_x_zm_rtaid%3DXNTISNTGTUOGUBnkd9Mz4g.1689865208330.e2d5810537a2878e50e4a85b887b8e00%26_x_zm_rhtaid%3D39"
                    , ClickableText.appearsInline
                    ]
                , Html.text "."
                ]
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
                , mainType = Just "RootHtml.Html ()"
                , extraCode = [ "import Http" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \{ page, recoveryText } ->
                        [ { sectionName = "Example"
                          , code =
                                Tuple.first page
                                    ++ Code.recordMultiline
                                        [ ( "link", "()" )
                                        , ( "recoveryText", Debug.toString recoveryText )
                                        ]
                                        1
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Tuple.second settings.page
                { link = ShowItWorked
                , recoveryText = settings.recoveryText
                }
            ]
    }


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
                ( moduleName ++ ".httpError httpError"
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
