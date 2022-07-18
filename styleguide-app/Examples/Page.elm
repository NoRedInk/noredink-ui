module Examples.Page exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Http
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Page.V3 as Page exposing (RecoveryText(..))


{-| -}
type alias State =
    { httpError : Control Http.Error
    , recoveryText : Control RecoveryText
    }


{-| -}
type Msg
    = ShowItWorked String
    | SetHttpError (Control Http.Error)
    | SetRecoveryText (Control RecoveryText)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowItWorked message ->
            ( Debug.log "Clicked: " message |> always model, Cmd.none )

        SetHttpError controls ->
            ( { model | httpError = controls }, Cmd.none )

        SetRecoveryText controls ->
            ( { model | recoveryText = controls }, Cmd.none )


{-| -}
example : Example State Msg
example =
    { name = "Page"
    , version = 3
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state =
        { httpError =
            Control.record identity
                |> Control.field "httpError" CommonControls.httpError
        , recoveryText =
            Control.record identity
                |> Control.field "recoveryText" initRecoveryText
        }
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
                recoveryText =
                    Control.currentValue model.recoveryText
            in
            [ Html.fromUnstyled (Control.view SetRecoveryText model.recoveryText)
            , viewExample "Page.httpError error" (Page.httpError (Control.currentValue model.httpError)) recoveryText [ Html.fromUnstyled (Control.view SetHttpError model.httpError) ]
            , viewExample "Page.broken" Page.broken recoveryText []
            , viewExample "Page.blockedV4" (Page.blockedV4 "Error message details") recoveryText []
            , viewExample "Page.notFound" Page.notFound recoveryText []
            , viewExample "Page.noPermission" Page.noPermission recoveryText []
            , viewExample "Page.loggedOut" Page.loggedOut recoveryText []
            , viewExample "Page.timeOut" Page.timeOut recoveryText []
            , viewExample "Page.networkError" Page.networkError recoveryText []
            ]
    }


viewExample :
    String
    -> (Page.DefaultPage Msg -> Html Msg)
    -> RecoveryText
    -> List (Html Msg)
    -> Html Msg
viewExample viewName view recoveryText extras =
    Html.div
        [ css
            [ Css.marginTop (Css.px 20)
            , Css.borderTop3 (Css.px 2) Css.solid Colors.gray96
            , Css.paddingTop (Css.px 20)
            , Css.marginBottom (Css.px 20)
            ]
        ]
        [ Heading.h2 [] [ Html.text viewName ]
        , Html.div [] extras
        , Html.code []
            [ Html.text <|
                viewName
                    ++ " {  link = msg, recoveryText = "
                    ++ Debug.toString recoveryText
                    ++ " }"
            ]
        , view { link = ShowItWorked viewName, recoveryText = recoveryText }
        ]


initRecoveryText : Control RecoveryText
initRecoveryText =
    Control.choice
        [ ( "Page.Reload", Control.value Page.Reload )
        , ( "Page.ReturnTo", Control.map Page.ReturnTo (Control.string "Home") )
        , ( "Page.Custom", Control.map Custom (Control.string "Hit the road, Jack") )
        ]
