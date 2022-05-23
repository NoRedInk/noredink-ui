module Nri.Ui.Page.V4 exposing
    ( httpError
    , DefaultPage, broken, blocked, notFound, noPermission, loggedOut, timeOut, networkError
    , RecoveryText(..)
    )

{-| A styled NRI page!

@docs httpError
@docs DefaultPage, broken, blocked, notFound, noPermission, loggedOut, timeOut, networkError
@docs RecoveryText

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)


{-| The default page information is for the button
which will direct the user back to the main page of
the SPA. Specify its name and the message which will
navigate to the page.
-}
type alias DefaultPage msg =
    { link : msg
    , recoveryText : RecoveryText
    }


{-| ReturnTo just needs the name of the page the user
will be returned to. Reload displays default text to reload
the current page. Custom is to add flexibility to the button.
-}
type RecoveryText
    = ReturnTo String
    | Reload
    | Custom String


{-| For the not found page.
-}
notFound : DefaultPage msg -> Html msg
notFound defaultPage =
    view
        { emoji = "🤔"
        , title = "We couldn’t find that!"
        , subtitle = "Feel free to browse around, or check out our help center."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = True
        }


{-| For HTTP errors and other broken states.
-}
broken : DefaultPage msg -> Html msg
broken defaultPage =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = True
        }


{-| Error page with details for engineers.
-}
blocked : String -> DefaultPage msg -> Html msg
blocked details defaultPage =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = defaultPage
        , details = Just details
        , showHelpButton = True
        }


{-| For pages the user does not have access to.
-}
noPermission : DefaultPage msg -> Html msg
noPermission defaultPage =
    view
        { emoji = "🤐"
        , title = "You do not have access to this page!"
        , subtitle = "Talk to a site administrator if you believe you should have access to this page."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = True
        }


{-| When a request fails due to a connectivity failure.
-}
networkError : DefaultPage msg -> Html msg
networkError defaultPage =
    view
        { emoji = "🤝"
        , title = "Are you connected to the Internet?"
        , subtitle = "Something went wrong, and we think the problem is probably with your internet connection."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = False
        }


{-| When a request takes too long to complete.
-}
timeOut : DefaultPage msg -> Html msg
timeOut defaultPage =
    view
        { emoji = "⏱"
        , title = "There was a problem!"
        , subtitle = "This request took too long to complete."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = False
        }


{-| When the user has been logged out.
-}
loggedOut : DefaultPage msg -> Html msg
loggedOut defaultPage =
    view
        { emoji = "🙃"
        , title = "You were logged out."
        , subtitle = "Please log in again to continue working."
        , defaultPage = defaultPage
        , details = Nothing
        , showHelpButton = False
        }


{-| -}
httpError : Http.Error -> DefaultPage msg -> Html msg
httpError error defaultPage =
    case error of
        Http.BadUrl _ ->
            broken defaultPage

        Http.Timeout ->
            timeOut defaultPage

        Http.NetworkError ->
            networkError defaultPage

        Http.BadStatus 401 ->
            loggedOut defaultPage

        Http.BadStatus 404 ->
            notFound defaultPage

        Http.BadStatus status ->
            blocked ("HTTP error status: " ++ String.fromInt status) defaultPage

        Http.BadBody body ->
            blocked body defaultPage



-- INTERNAL


type alias Config msg =
    { emoji : String
    , title : String
    , subtitle : String
    , defaultPage : DefaultPage msg
    , details : Maybe String
    , showHelpButton : Bool
    }


view : Config msg -> Html msg
view config =
    viewContainer
        [ viewEmoji [ Html.text config.emoji ]
        , Heading.h1 [] [ Html.text config.title ]
        , Heading.h2 [] [ Html.text config.subtitle ]
        , viewButton [ viewExit config ]
        , viewIf
            (\_ ->
                viewButton
                    [ Button.link "Get help!"
                        [ Button.linkExternal "https://noredink.zendesk.com/hc/en-us"
                        , Button.large
                        , Button.exactWidth 260
                        , Button.secondary
                        ]
                    ]
            )
            config.showHelpButton
        , case config.details of
            Just details ->
                viewButton [ viewDetails details ]

            Nothing ->
                Html.text ""
        ]


viewExit : Config msg -> Html msg
viewExit config =
    Button.button
        (case config.defaultPage.recoveryText of
            ReturnTo name ->
                "Return to " ++ name

            Reload ->
                "Try again"

            Custom text ->
                text
        )
        [ Button.onClick config.defaultPage.link
        , Button.large
        , Button.exactWidth 260
        ]


viewDetails : String -> Html msg
viewDetails detailsForEngineers =
    Html.div []
        [ Html.styled Html.details
            [ margin (px 10)
            , maxWidth (px 700)
            ]
            []
            [ Html.styled Html.summary
                [ color Colors.gray45 ]
                []
                [ Html.text "Details for NoRedInk engineers" ]
            , Html.styled Html.code
                [ display block
                , whiteSpace normal
                , overflowWrap breakWord
                , textAlign left
                , marginTop (px 10)
                ]
                []
                [ Html.text detailsForEngineers ]
            ]
        ]


viewContainer : List (Html msg) -> Html msg
viewContainer =
    Html.styled Html.div
        [ marginTop (px 80)
        , displayFlex
        , flexDirection column
        , alignItems center
        ]
        [ Attributes.attribute "data-page-container" "" ]


viewButton : List (Html msg) -> Html msg
viewButton children =
    Html.styled Html.div
        [ marginTop (px 15)
        ]
        []
        [ Html.styled Html.div
            [ textAlign center ]
            []
            children
        ]


viewEmoji : List (Html msg) -> Html msg
viewEmoji =
    Html.styled Html.div
        [ fontSize (px 75)
        , height (px 98)
        , lineHeight (px 98)
        ]
        []
