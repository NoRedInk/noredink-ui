module InputErrorAndGuidanceInternal exposing
    ( ErrorState, noError, setErrorIf, setErrorMessage
    , Guidance, noGuidance, setGuidance, setGuidanceHtml
    , getIsInError, getErrorMessage
    , describedBy, view, smallMargin
    , guidanceId, errorId
    )

{-|

@docs ErrorState, noError, setErrorIf, setErrorMessage
@docs Guidance, noGuidance, setGuidance, setGuidanceHtml
@docs getIsInError, getErrorMessage
@docs describedBy, view, smallMargin
@docs guidanceId, errorId

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Live as Live
import Css
import Html.Styled as Html exposing (Html)
import Nri.Ui.Html.Attributes.V2
import Nri.Ui.Message.V4 as Message
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
type alias Guidance msg =
    Maybe (GuidanceContent msg)


type GuidanceContent msg
    = TextGuidance String
    | HtmlGuidance (List (Html msg))


{-| -}
noGuidance : Guidance msg
noGuidance =
    Nothing


{-| -}
setGuidance : String -> { config | guidance : Guidance msg } -> { config | guidance : Guidance msg }
setGuidance guidance config =
    { config | guidance = Just (TextGuidance guidance) }


setGuidanceHtml : List (Html msg) -> { config | guidance : Guidance msg } -> { config | guidance : Guidance msg }
setGuidanceHtml html config =
    { config | guidance = Just (HtmlGuidance html) }


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


describedBy : String -> { config | guidance : Guidance msg, error : ErrorState } -> Html.Attribute msg
describedBy idValue config =
    let
        relevantIds =
            apply
                { ifError = \_ -> errorId idValue
                , ifGuidance = \_ -> guidanceId idValue
                }
                config
    in
    case relevantIds of
        [] ->
            Nri.Ui.Html.Attributes.V2.none

        _ ->
            Aria.describedBy relevantIds


view : String -> Css.Style -> { config | guidance : Guidance msg, error : ErrorState } -> List (Html msg)
view idValue marginTop =
    apply
        { ifError = renderErrorMessage idValue marginTop
        , ifGuidance = renderGuidance idValue marginTop
        }


apply :
    { ifError : String -> a, ifGuidance : GuidanceContent msg -> a }
    -> { config | guidance : Guidance msg, error : ErrorState }
    -> List a
apply { ifError, ifGuidance } config =
    let
        maybeError =
            getErrorMessage config.error
    in
    case ( maybeError, config.guidance ) of
        ( Just errorMessage, Just guidanceContent ) ->
            if guidanceContent /= TextGuidance errorMessage then
                [ ifError errorMessage
                , ifGuidance guidanceContent
                ]

            else
                [ ifError errorMessage ]

        ( Just errorMessage, Nothing ) ->
            [ ifError errorMessage ]

        ( Nothing, Just guidanceContent ) ->
            [ ifGuidance guidanceContent ]

        ( Nothing, Nothing ) ->
            []


renderErrorMessage : String -> Css.Style -> String -> Html msg
renderErrorMessage idValue marginTop m =
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


renderGuidance : String -> Css.Style -> GuidanceContent msg -> Html msg
renderGuidance idValue marginTop guidanceContent =
    Text.caption
        [ Text.id (guidanceId idValue)
        , case guidanceContent of
            TextGuidance guidanceMessage ->
                Text.plaintext guidanceMessage

            HtmlGuidance html ->
                Text.html html
        , Text.css
            [ Css.important (Css.paddingTop Css.zero)
            , Css.important (Css.paddingBottom Css.zero)
            , Css.important marginTop
            ]
        ]


smallMargin : Css.Style
smallMargin =
    Css.marginTop (Css.px 5)


errorId : String -> String
errorId idValue =
    idValue ++ "_error-message"


guidanceId : String -> String
guidanceId idValue =
    idValue ++ "_guidance"
