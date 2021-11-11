module InputErrorAndGuidanceInternal exposing
    ( ErrorState, noError, setErrorIf, setErrorMessage
    , Guidance, noGuidance, setGuidance
    , getIsInError, getErrorMessage
    )

{-|

@docs ErrorState, noError, setErrorIf, setErrorMessage
@docs Guidance, noGuidance, setGuidance
@docs getIsInError, getErrorMessage

-}


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
