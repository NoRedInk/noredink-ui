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
            let
                _ =
                    Debug.log "Clicked: " message
            in
            ( model, Cmd.none )

        SetHttpError controls ->
            ( { model | httpError = controls }, Cmd.none )

        SetRecoveryText controls ->
            ( { model | recoveryText = controls }, Cmd.none )


{-| -}
example : Example State Msg
example =
    { name = "Page"
    , version = 3
    , categories = [ Pages ]
    , atomicDesignType = Page
    , keyboardSupport = []
    , state = { httpError = initHttpError, recoveryText = initRecoveryText }
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \model ->
            let
                recoveryText =
                    Control.currentValue model.recoveryText
            in
            [ Html.fromUnstyled (Control.view SetRecoveryText model.recoveryText)
            , Heading.h3 [] [ Html.text "Page.httpError" ]
            , Html.fromUnstyled (Control.view SetHttpError model.httpError)
            , Page.httpError
                { link = ShowItWorked "Page.httpError"
                , recoveryText = recoveryText
                }
                (Control.currentValue model.httpError)
            , Heading.h3 [] [ Html.text "Page.notFound" ]
            , Page.notFound
                { link = ShowItWorked "Page.notFound"
                , recoveryText = recoveryText
                }
            , Heading.h3 [] [ Html.text "Page.broken" ]
            , Page.broken
                { link = ShowItWorked "Page.broken"
                , recoveryText = recoveryText
                }
            , Heading.h3 [] [ Html.text "Page.noPermission" ]
            , Page.noPermission
                { link = ShowItWorked "Page.noPermission"
                , recoveryText = recoveryText
                }
            , Heading.h3 [] [ Html.text "Page.loggedOut" ]
            , Page.loggedOut
                { link = ShowItWorked "Page.loggedOut Custom"
                , recoveryText = recoveryText
                }
            ]
    }


initHttpError : Control Http.Error
initHttpError =
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


initRecoveryText : Control RecoveryText
initRecoveryText =
    Control.choice
        [ ( "Page.ReturnTo", Control.map Page.ReturnTo (Control.string "Home") )
        , ( "Page.Reload", Control.value Page.Reload )
        , ( "Page.Custom", Control.map Custom (Control.string "Hit the road, Jack") )
        ]
