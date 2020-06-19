module Examples.Page exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Page.V3 as Page


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    String


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Page.V3"
    , categories = [ Pages ]
    , atomicDesignType = Page
    , keyboardSupport = []
    , state = ()
    , update =
        \msg model ->
            let
                _ =
                    Debug.log "Clicked: " msg
            in
            ( model, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ Css.Global.global
                [ selector "[data-page-container]"
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    ]
                ]
            , Heading.h3 [] [ Html.text "Page: Not Found, recovery text: ReturnTo" ]
            , Page.notFound
                { link = "Page.notFound ReturnTo"
                , recoveryText = Page.ReturnTo "the main page"
                }
            , Heading.h3 [] [ Html.text "Page: Broken, recovery text: Reload" ]
            , Page.broken
                { link = "Page.broken Reload"
                , recoveryText = Page.Reload
                }
            , Heading.h3 [] [ Html.text "Page: No Permission, recovery text: Custom" ]
            , Page.noPermission
                { link = "Page.noPermission Custom"
                , recoveryText = Page.Custom "Hit the road, Jack"
                }
            ]
    }
