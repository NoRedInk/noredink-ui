module Nri.Alert
    exposing
        ( Model
        , error
        , success
        , tip
        , viewResult
        , warning
        )

{-| UI components that highlight information to the user.

@docs Model
@docs error
@docs success
@docs tip
@docs viewResult
@docs warning

-}

import Accessibility exposing (..)
import Html.Attributes exposing (..)
import Markdown


{-| -}
type alias Model =
    { content : String }


alert : String -> Model -> Html msg
alert className { content } =
    div
        [ class className ]
        (Markdown.toHtml Nothing content)


{-| Show either an error or success alert depending on the given Result
-}
viewResult : Result String String -> Html msg
viewResult result =
    case result of
        Ok msg ->
            success { content = msg }

        Err msg ->
            error { content = msg }


{-| -}
error : Model -> Html msg
error =
    alert "alert-error-container"


{-| -}
success : Model -> Html msg
success =
    alert "alert-success-container"


{-| -}
tip : Model -> Html msg
tip =
    alert "alert-tip-container"


{-| -}
warning : Model -> Html msg
warning =
    alert "alert-warning-container"
