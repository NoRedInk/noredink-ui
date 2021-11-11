module InputErrorAndGuidanceInternal exposing
    ( ErrorState, noError, setErrorIf, setErrorMessage
    , Guidance, noGuidance, setGuidance
    , getIsInError, getErrorMessage
    , describedBy, view
    )

{-|

@docs ErrorState, noError, setErrorIf, setErrorMessage
@docs Guidance, noGuidance, setGuidance
@docs getIsInError, getErrorMessage
@docs describedBy, view

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (Style)
import Html.Styled as Html exposing (Html)
import Nri.Ui.Html.Attributes.V2
import Nri.Ui.Message.V3 as Message
import Nri.Ui.Text.V6 as Text


{-| -}
type ErrorState
    = NoError
    | Error
    | ErrorWithMessage String


{-| -}
noError : ErrorState
noError =
    NoError


{-| -}
setErrorIf : Bool -> { config | error : ErrorState } -> { config | error : ErrorState }
setErrorIf isInError_ config =
    { config
        | error =
            if isInError_ then
                Error

            else
                NoError
    }


{-| -}
setErrorMessage : Maybe String -> { config | error : ErrorState } -> { config | error : ErrorState }
setErrorMessage maybeMessage config =
    { config
        | error =
            case maybeMessage of
                Nothing ->
                    NoError

                Just message ->
                    ErrorWithMessage message
    }


{-| -}
type alias Guidance =
    Maybe String


{-| -}
noGuidance : Guidance
noGuidance =
    Nothing


{-| -}
setGuidance : String -> { config | guidance : Guidance } -> { config | guidance : Guidance }
setGuidance guidance config =
    { config | guidance = Just guidance }


{-| -}
getIsInError : ErrorState -> Bool
getIsInError error =
    case error of
        NoError ->
            False

        Error ->
            True

        ErrorWithMessage _ ->
            True


{-| -}
getErrorMessage : ErrorState -> Maybe String
getErrorMessage error =
    case error of
        NoError ->
            Nothing

        Error ->
            Nothing

        ErrorWithMessage message ->
            Just message


describedBy : String -> { config | guidance : Guidance, error : ErrorState } -> Html.Attribute msg
describedBy idValue config =
    case ( getErrorMessage config.error, config.guidance ) of
        ( Nothing, Just _ ) ->
            Aria.describedBy [ idValue ++ "_guidance" ]

        _ ->
            Nri.Ui.Html.Attributes.V2.none


view : String -> { config | guidance : Guidance, error : ErrorState } -> Html msg
view idValue config =
    case ( getErrorMessage config.error, config.guidance ) of
        ( Just m, _ ) ->
            Message.view
                [ Message.tiny
                , Message.error
                , Message.plaintext m
                , Message.alertRole
                , Message.css spacing
                ]

        ( _, Just guidanceMessage ) ->
            Text.caption
                [ Text.id (idValue ++ "_guidance")
                , Text.plaintext guidanceMessage
                , Text.css spacing
                ]

        _ ->
            Html.text ""


spacing : List Style
spacing =
    [ Css.important (Css.paddingTop Css.zero)
    , Css.important (Css.paddingBottom Css.zero)
    , Css.marginTop (Css.px 5)
    ]
