module Nri.Ui.Page.V3 exposing
    ( httpError
    , DefaultPage, broken, blockedV4, blocked, notFound, noPermission, loggedOut, timeOut, networkError
    , RecoveryText(..)
    , headingId
    )

{-| Patch changes:

  - added `headingId` as the `id` of the `h1` produced by `Page`
  - made the `h1` produced by `Page` programmatically focusable

A styled NRI error page.

@docs httpError
@docs DefaultPage, broken, blockedV4, blocked, notFound, noPermission, loggedOut, timeOut, networkError
@docs RecoveryText
@docs headingId

-}

import Accessibility.Styled.Key as Key
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http
import Nri.Ui.Button.V11 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.Text.V6 as Text


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
        , defaultPage = Just defaultPage
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
        , defaultPage = Just defaultPage
        , details = Nothing
        , showHelpButton = True
        }


{-| DEPRECATED: please use blockedV4.

For HTTP errors and other broken states, where link goes to "/".

-}
blocked : String -> Html msg
blocked details =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = Nothing
        , details = Just details
        , showHelpButton = True
        }


{-| Error page with details for engineers.
-}
blockedV4 : String -> DefaultPage msg -> Html msg
blockedV4 details defaultPage =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = Just defaultPage
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
        , defaultPage = Just defaultPage
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
        , defaultPage = Just defaultPage
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
        , defaultPage = Just defaultPage
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
        , defaultPage = Just defaultPage
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
            blockedV4 ("HTTP error status: " ++ String.fromInt status) defaultPage

        Http.BadBody body ->
            blockedV4 body defaultPage


{-| This ID is attached to the `h1` produced by Page.
This can be useful when you need to move focus to the heading to communicate to AT users that there's been an error.
-}
headingId : String
headingId =
    "nri-ui-page-heading-h1"



-- INTERNAL


type alias Config msg =
    { emoji : String
    , title : String
    , subtitle : String
    , defaultPage : Maybe (DefaultPage msg)
    , details : Maybe String
    , showHelpButton : Bool
    }


view : Config msg -> Html msg
view config =
    viewContainer
        [ viewEmoji [ Html.text config.emoji ]
        , Heading.h1
            [ Heading.plaintext config.title
            , Heading.css [ Css.textAlign Css.center ]
            , Heading.id headingId
            , Heading.custom [ Key.tabbable False ]
            ]
        , Text.mediumBody
            [ Text.plaintext config.subtitle
            , Text.css
                [ Css.fontSize (Css.px 20)
                , Css.color Colors.gray45
                , Css.marginBottom Css.zero
                , Css.textAlign Css.center
                ]
            ]
        , viewButton
            [ viewExit config ]
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
    case config.defaultPage of
        Just defaultPage ->
            Button.button
                (case defaultPage.recoveryText of
                    ReturnTo name ->
                        "Return to " ++ name

                    Reload ->
                        "Try again"

                    Custom text ->
                        text
                )
                [ Button.onClick defaultPage.link
                , Button.large
                , Button.exactWidth 260
                ]

        Nothing ->
            Button.link "Return to dashboard"
                [ Button.href "/"
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
                , whiteSpace preWrap
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
