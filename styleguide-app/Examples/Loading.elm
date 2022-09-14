module Examples.Loading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Loading.V1 as Loading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text


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
    { name = "Loading"
    , version = 2
    , categories = [ Animations ]
    , keyboardSupport = []
    , state = init
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
    , view =
        \ellieLinkConfig { showLoadingFadeIn, showLoading, showSpinners } ->
            [ if showLoading then
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
                Html.div []
                    [ Loading.spinningPencil
                        |> Svg.withColor Colors.azure
                        |> Svg.toHtml
                    , Text.caption [ Text.plaintext "By default, the spinningPencil is white. Showing as blue for visibility." ]
                    ]

              else
                button "Loading.spinningPencil" ShowSpinners showLoadingFadeIn
            ]
    }


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
