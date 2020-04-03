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
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Loading.V1 as Loading


{-| -}
type alias State =
    { showLoadingFadeIn : Bool
    , showLoading : Bool
    }


init : State
init =
    { showLoadingFadeIn = False
    , showLoading = False
    }


{-| -}
type Msg
    = ShowLoadingFadeIn
    | ShowLoading
    | CloseFullScreenPage


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

        CloseFullScreenPage ->
            ( { model
                | showLoadingFadeIn = False
                , showLoading = False
              }
            , Cmd.none
            )


subscriptions : State -> Sub Msg
subscriptions { showLoadingFadeIn, showLoading } =
    if showLoadingFadeIn || showLoading then
        Browser.Events.onClick (Json.Decode.succeed CloseFullScreenPage)

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
        \{ showLoadingFadeIn, showLoading } ->
            [ if showLoading then
                Loading.page

              else
                Html.text ""
            , Button.button "Loading.page"
                [ Button.custom
                    [ Events.stopPropagationOn "click"
                        (Json.Decode.map (\m -> ( m, True ))
                            (Json.Decode.succeed ShowLoading)
                        )
                    ]
                , if showLoadingFadeIn then
                    Button.disabled

                  else
                    Button.secondary
                , Button.css [ Css.marginRight (Css.px 20) ]
                ]
            , if showLoadingFadeIn then
                Loading.fadeInPage

              else
                Html.text ""
            , Button.button "Loading.fadeInPage"
                [ Button.custom
                    [ Events.stopPropagationOn "click"
                        (Json.Decode.map (\m -> ( m, True ))
                            (Json.Decode.succeed ShowLoadingFadeIn)
                        )
                    ]
                , if showLoadingFadeIn then
                    Button.loading

                  else
                    Button.secondary
                ]
            ]
    }
