module Examples.Loading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Css
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Loading.V1 as Loading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V4 as Text


{-| -}
type alias State =
    { showLoadingFadeIn : Bool
    , showLoading : Bool
    , showSpinner : Bool
    }


init : State
init =
    { showLoadingFadeIn = False
    , showLoading = False
    , showSpinner = False
    }


{-| -}
type Msg
    = ShowLoadingFadeIn
    | ShowLoading
    | ShowSpinner
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

        ShowSpinner ->
            ( { model | showSpinner = True }
            , Cmd.none
            )

        Close ->
            ( { model
                | showLoadingFadeIn = False
                , showLoading = False
                , showSpinner = False
              }
            , Cmd.none
            )


subscriptions : State -> Sub Msg
subscriptions { showLoadingFadeIn, showLoading, showSpinner } =
    if showLoadingFadeIn || showLoading || showSpinner then
        Browser.Events.onClick (Json.Decode.succeed Close)

    else
        Sub.none


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Loading.V1"
    , categories = [ Pages ]
    , state = init
    , update = update
    , subscriptions = subscriptions
    , view =
        \{ showLoadingFadeIn, showLoading, showSpinner } ->
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
            , if showSpinner then
                Html.div []
                    [ Loading.spinner
                        |> Svg.withColor Colors.blue
                        |> Svg.toHtml
                    , Text.caption [ Html.text "By default, the spinner is white. Showing as blue for visibility." ]
                    ]

              else
                button "Loading.spinner" ShowSpinner showLoadingFadeIn
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
