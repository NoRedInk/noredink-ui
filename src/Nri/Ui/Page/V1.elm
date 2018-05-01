module Nri.Ui.Page.V1 exposing (Default, blocked, broken, notFound)

{-| A styled NRI issue page!

@docs Default, broken, blocked, notFound

-}

import Css exposing (..)
import Html
import Html.Styled
import Nri.Ui.Button.V2 as Button
import Nri.Ui.Text.V1


{-| The default page information is for the button
which will direct the user back to the main page of
the SPA. Specify it's name and the message which will
navigate to the page.
-}
type alias Default msg =
    { link : msg
    , name : String
    }


{-| For the not found page.
-}
notFound : Default msg -> Html.Html msg
notFound defaultPage =
    view
        { emoji = "\x1F914"
        , title = "We couldn’t find that!"
        , subtitle = "Feel free to browse around, or check out our help center."
        , defaultPage = Just defaultPage
        , details = Nothing
        }


{-| For HTTP errors and other broken states.
-}
broken : Default msg -> Html.Html msg
broken defaultPage =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = Just defaultPage
        , details = Nothing
        }


{-| For HTTP errors and other broken states, where link goes to "/".
-}
blocked : String -> Html.Html msg
blocked details =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = Nothing
        , details = Just details
        }



-- INTERNAL


type alias Config msg =
    { emoji : String
    , title : String
    , subtitle : String
    , defaultPage : Maybe (Default msg)
    , details : Maybe String
    }


view : Config msg -> Html.Html msg
view config =
    Html.Styled.toUnstyled <|
        viewContainer
            [ viewEmoji [ Html.Styled.text config.emoji ]
            , Nri.Ui.Text.V1.heading [ Html.text config.title ]
                |> Html.Styled.fromUnstyled
            , Nri.Ui.Text.V1.tagline [ Html.text config.subtitle ]
                |> Html.Styled.fromUnstyled
            , viewCentered
                [ viewExit config ]
            , viewCentered
                [ Button.linkExternal
                    { label = "Get help!"
                    , icon = Nothing
                    , url = "https://noredink.zendesk.com/hc/en-us" -- TODO: What is the real support url?
                    , size = Button.Large
                    , style = Button.Secondary
                    , width = Just 260
                    }
                    |> Html.Styled.fromUnstyled
                ]
            , case config.details of
                Just details ->
                    viewCentered [ viewDetails details ]

                Nothing ->
                    Html.Styled.text ""
            ]


viewExit : Config msg -> Html.Styled.Html msg
viewExit config =
    case config.defaultPage of
        Just defaultPage ->
            Button.button
                { onClick = defaultPage.link
                , size = Button.Large
                , style = Button.Primary
                , width = Just 260
                }
                { label = "Return to " ++ defaultPage.name
                , state = Button.Enabled
                , icon = Nothing
                }
                |> Html.Styled.fromUnstyled

        Nothing ->
            Button.link
                { label = "Return to dashboard"
                , icon = Nothing
                , url = "/"
                , size = Button.Large
                , style = Button.Primary
                , width = Just 260
                }
                |> Html.Styled.fromUnstyled


viewDetails : String -> Html.Styled.Html msg
viewDetails detailsForEngineers =
    Html.Styled.div []
        [ Html.Styled.styled Html.Styled.details
            [ margin (px 10) ]
            []
            [ Html.Styled.styled Html.Styled.summary
                [ color (hex "8F8F8F") ]
                []
                [ Html.Styled.text "Details for NoRedInk engineers" ]
            , Html.Styled.styled Html.Styled.code
                [ display block
                , whiteSpace normal
                , overflowWrap breakWord
                , textAlign left
                , marginTop (px 10)
                ]
                []
                [ Html.Styled.text detailsForEngineers ]
            ]
        ]


viewContainer : List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewContainer =
    Html.Styled.styled Html.Styled.div
        [ marginTop (px 80)
        ]
        []


viewCentered : List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewCentered children =
    Html.Styled.styled Html.Styled.div
        [ margin2 zero auto
        , marginTop (px 15)
        ]
        []
        [ Html.Styled.styled Html.Styled.div
            [ textAlign center ]
            []
            children
        ]


viewEmoji : List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewEmoji =
    Html.Styled.styled Html.Styled.div
        [ fontSize (px 75)
        , height (px 98)
        , lineHeight (px 98)
        ]
        []
