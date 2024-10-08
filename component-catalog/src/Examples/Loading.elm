module Examples.Loading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Guidance
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Loading.V1 as Loading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg


{-| -}
type alias State =
    { showLoadingFadeIn : Bool
    , showLoading : Bool
    , showSpinners : Bool
    }


init : State
init =
    { showLoadingFadeIn = False
    , showLoading = False
    , showSpinners = False
    }


{-| -}
type Msg
    = ShowLoadingFadeIn
    | ShowLoading
    | ShowSpinners
    | Close


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowLoadingFadeIn ->
            ( { model | showLoadingFadeIn = True }
            , Cmd.none
            )

        ShowLoading ->
            ( { model | showLoading = True }
            , Cmd.none
            )

        ShowSpinners ->
            ( { model | showSpinners = True }
            , Cmd.none
            )

        Close ->
            ( { model
                | showLoadingFadeIn = False
                , showLoading = False
                , showSpinners = False
              }
            , Cmd.none
            )


subscriptions : State -> Sub Msg
subscriptions { showLoadingFadeIn, showLoading, showSpinners } =
    if showLoadingFadeIn || showLoading || showSpinners then
        Browser.Events.onClick (Json.Decode.succeed Close)

    else
        Sub.none


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = 1
    , categories = [ Animations ]
    , keyboardSupport = []
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = subscriptions
    , preview =
        [ Loading.spinningPencil
            |> Svg.withCss
                [ Css.property "animation-name" "none" |> Css.important
                , Css.alignSelf Css.center
                ]
            |> Svg.toHtml
        ]
    , about = [ Guidance.useATACGuide moduleName ]
    , view =
        \ellieLinkConfig { showLoadingFadeIn, showLoading, showSpinners } ->
            [ Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css
                    [ Css.marginTop Spacing.verticalSpacerPx
                    , Css.marginBottom (Css.px 10)
                    ]
                ]
            , if showLoading then
                Loading.page

              else
                Html.text ""
            , button "Loading.page" ShowLoading showLoadingFadeIn
            , if showLoadingFadeIn then
                Loading.fadeInPage

              else
                Html.text ""
            , button "Loading.fadeInPage" ShowLoadingFadeIn showLoadingFadeIn
            , if showSpinners then
                Loading.spinning (Css.px 140) Colors.navy

              else
                button "Loading.spinning Colors.navy" ShowSpinners showLoadingFadeIn
            ]
    }


moduleName : String
moduleName =
    "Loading"


button : String -> Msg -> Bool -> Html Msg
button name do disabled =
    Button.button name
        [ Button.custom
            [ Events.stopPropagationOn "click"
                (Json.Decode.map (\m -> ( m, True ))
                    (Json.Decode.succeed do)
                )
            ]
        , if disabled then
            Button.disabled

          else
            Button.secondary
        , Button.css [ Css.marginRight (Css.px 20) ]
        ]
