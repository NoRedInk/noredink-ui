module InputErrorAndGuidanceInternal exposing
    ( ErrorState, noError
    , setErrorIf, setErrorMessage
    , getIsInError, getErrorMessage
    )

{-|

@docs ErrorState, noError
@docs setErrorIf, setErrorMessage
@docs getIsInError, getErrorMessage

-}


{-| -}
type ErrorState
    = NoError
    | Error { message : Maybe String }


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
                Error { message = Nothing }

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
                    Error { message = Just message }
    }


{-| -}
getIsInError : ErrorState -> Bool
getIsInError error =
    case error of
        NoError ->
            False

        Error _ ->
            True


{-| -}
getErrorMessage : ErrorState -> Maybe String
getErrorMessage error =
    case error of
        NoError ->
            Nothing

        Error { message } ->
            message
