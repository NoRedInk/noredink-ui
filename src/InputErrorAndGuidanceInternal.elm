module InputErrorAndGuidanceInternal exposing
    ( ErrorState, noError, setErrorIf, setErrorMessage
    , Guidance, noGuidance, setGuidance
    , getIsInError, getErrorMessage
    , describedBy, view, smallMargin
    , guidanceId, errorId
    )

{-|

@docs ErrorState, noError, setErrorIf, setErrorMessage
@docs Guidance, noGuidance, setGuidance
@docs getIsInError, getErrorMessage
@docs describedBy, view, smallMargin
@docs guidanceId, errorId

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Live as Live
import Css
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
        ( Just _, _ ) ->
            Aria.describedBy [ errorId idValue ]

        ( _, Just _ ) ->
            Aria.describedBy [ guidanceId idValue ]

        _ ->
            Nri.Ui.Html.Attributes.V2.none


view : String -> Css.Style -> { config | guidance : Guidance, error : ErrorState } -> Html msg
view idValue marginTop config =
    case ( getErrorMessage config.error, config.guidance ) of
        ( Just m, _ ) ->
            Message.view
                [ Message.tiny
                , Message.error
                , Message.plaintext m
                , Message.alertRole
                , Message.id (errorId idValue)
                , Message.custom [ Live.polite ]
                , Message.css
                    [ Css.important (Css.paddingTop Css.zero)
                    , Css.important (Css.paddingBottom Css.zero)
                    , marginTop
                    ]
                ]

        ( _, Just guidanceMessage ) ->
            Text.caption
                [ Text.id (guidanceId idValue)
                , Text.plaintext guidanceMessage
                , Text.css
                    [ Css.important (Css.paddingTop Css.zero)
                    , Css.important (Css.paddingBottom Css.zero)
                    , Css.important marginTop
                    , Css.lineHeight (Css.num 1)
                    ]
                ]

        _ ->
            Html.text ""


smallMargin : Css.Style
smallMargin =
    Css.marginTop (Css.px 5)


errorId : String -> String
errorId idValue =
    idValue ++ "_error-message"


guidanceId : String -> String
guidanceId idValue =
    idValue ++ "_guidance"
