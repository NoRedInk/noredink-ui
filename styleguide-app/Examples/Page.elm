module Examples.Page exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Http
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Page.V3 as Page


{-| -}
type alias State =
    Control Http.Error


{-| -}
type Msg
    = ShowItWorked String
    | SetControls (Control Http.Error)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowItWorked message ->
            let
                _ =
                    Debug.log "Clicked: " message
            in
            ( model, Cmd.none )

        SetControls controls ->
            ( controls, Cmd.none )


{-| -}
example : Example State Msg
example =
    { name = "Page"
    , version = 3
    , categories = [ Pages ]
    , atomicDesignType = Page
    , keyboardSupport = []
    , state =
        Control.choice
            [ ( "Bad Url", Control.value (Http.BadUrl "/request-url") )
            , ( "Timeout", Control.value Http.Timeout )
            , ( "Network Error", Control.value Http.NetworkError )
            , ( "Bad Status: 401", Control.value (Http.BadStatus 401) )
            , ( "Bad Status: 404", Control.value (Http.BadStatus 404) )
            , ( "Bad Status: ???", Control.value (Http.BadStatus 301) )
            , ( "Bad Body (often, a JSON decoding problem)"
              , Control.value
                    (Http.BadBody
                        """
                        The Json.Decode.oneOf at json.draft failed in the following 2 ways:



                        (1) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `content`



                        (2) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `code`
                        """
                    )
              )
            ]
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \model ->
            [ Css.Global.global
                [ selector "[data-page-container]"
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    ]
                ]
            , Heading.h3 [] [ Html.text "Page: Not Found, recovery text: ReturnTo" ]
            , Page.notFound
                { link = ShowItWorked "Page.notFound ReturnTo"
                , recoveryText = Page.ReturnTo "the main page"
                }
            , Heading.h3 [] [ Html.text "Page: Broken, recovery text: Reload" ]
            , Page.broken
                { link = ShowItWorked "Page.broken Reload"
                , recoveryText = Page.Reload
                }
            , Heading.h3 [] [ Html.text "Page: No Permission, recovery text: Custom" ]
            , Page.noPermission
                { link = ShowItWorked "Page.noPermission Custom"
                , recoveryText = Page.Custom "Hit the road, Jack"
                }
            , Heading.h3 [] [ Html.text "Page: Logged Out, recovery text: Custom" ]
            , Page.loggedOut
                { link = ShowItWorked "Page.loggedOut Custom"
                , recoveryText = Page.Custom "And don't you come back no mo no mo no mo"
                }
            , Heading.h3 [] [ Html.text "Page: Http Error" ]
            , Html.fromUnstyled (Control.view SetControls model)
            , Page.httpError
                { link = ShowItWorked "Page.httpError"
                , recoveryText = Page.Reload
                }
                (Control.currentValue model)
            ]
    }
