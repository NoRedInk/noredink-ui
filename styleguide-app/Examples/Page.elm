module Examples.Page exposing (example, State, Msg)

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
import Nri.Ui.Page.V3 as Page


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
    | LinkClick String


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

        LinkClick message ->
            let
                _ =
                    Debug.log "Clicked: " message
            in
            ( model, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions { showLoadingFadeIn, showLoading } =
    if showLoadingFadeIn || showLoading then
        Browser.Events.onClick (Json.Decode.succeed CloseFullScreenPage)

    else
        Sub.none


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Page.V3"
    , categories = List.singleton Pages
    , state = init
    , update = update
    , subscriptions = subscriptions
    , view =
        \{ showLoadingFadeIn, showLoading } ->
            [ Css.Global.global
                [ Css.Global.selector "[data-page-container]"
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    ]
                ]
            , Heading.h3 [] [ Html.text "Page: Not Found, recovery text: ReturnTo" ]
            , Page.notFound
                { link = LinkClick "ReturnTo"
                , recoveryText = Page.ReturnTo "the main page"
                }
            , Heading.h3 [] [ Html.text "Page: Broken, recovery text: Reload" ]
            , Page.broken
                { link = LinkClick "Reload"
                , recoveryText = Page.Reload
                }
            , Heading.h3 [] [ Html.text "Page: No Permission, recovery text: Custom" ]
            , Page.noPermission
                { link = LinkClick "Custom"
                , recoveryText = Page.Custom "Hit the road, Jack"
                }
            , Heading.h3 [] [ Html.text "Page.loadingFadeIn" ]
            , if showLoadingFadeIn then
                Page.loadingFadeIn

              else
                Html.text ""
            , Button.button "Open loadingFadeIn"
                [ Button.custom
                    [ Events.stopPropagationOn "click"
                        (Json.Decode.map (\m -> ( m, True ))
                            (Json.Decode.succeed ShowLoadingFadeIn)
                        )
                    ]
                , if showLoadingFadeIn then
                    Button.loading

                  else
                    Button.primary
                ]
            , Heading.h3 [] [ Html.text "Page.loading" ]
            , if showLoading then
                Page.loading

              else
                Html.text ""
            , Button.button "Open loading"
                [ Button.custom
                    [ Events.stopPropagationOn "click"
                        (Json.Decode.map (\m -> ( m, True ))
                            (Json.Decode.succeed ShowLoading)
                        )
                    ]
                , if showLoadingFadeIn then
                    Button.disabled

                  else
                    Button.primary
                ]
            ]
    }
