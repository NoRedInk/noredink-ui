module Nri.Ui.Page.V3 exposing
    ( DefaultPage, broken, blocked, notFound, noPermission
    , RecoveryText(..)
    , loadingFadeIn, loading
    )

{-| A styled NRI page!

@docs DefaultPage, broken, blocked, notFound, noPermission
@docs RecoveryText
@docs loadingFadeIn, loading

-}

import Css exposing (..)
import Css.Animations
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Button.V5 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V2 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


{-| The default page information is for the button
which will direct the user back to the main page of
the SPA. Specify it's name and the message which will
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
        { emoji = "\u{1F914}"
        , title = "We couldn’t find that!"
        , subtitle = "Feel free to browse around, or check out our help center."
        , defaultPage = Just defaultPage
        , details = Nothing
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
        }


{-| For HTTP errors and other broken states, where link goes to "/".
-}
blocked : String -> Html msg
blocked details =
    view
        { emoji = "😵"
        , title = "There was a problem!"
        , subtitle = "You can try again, or check out our help center."
        , defaultPage = Nothing
        , details = Just details
        }


{-| For pages the user does not have access to.
-}
noPermission : DefaultPage msg -> Html msg
noPermission defaultPage =
    view
        { emoji = "\u{1F910}"
        , title = "You do not have access to this page!"
        , subtitle = "Talk to a site administrator if you believe you should have access to this page."
        , defaultPage = Just defaultPage
        , details = Nothing
        }


{-| View a full-screen loading page that fades into view.
-}
loadingFadeIn : Html msg
loadingFadeIn =
    loading_
        [ Css.property "animation-delay" "1s"
        , Css.property "animation-duration" "1.5s"
        , Css.property "animation-fill-mode" "forwards"
        , Css.animationName fadeInKeyframes
        , Css.property "animation-timing-function" "linear"
        , Css.opacity Css.zero
        ]


{-| View a full-screen loading page.
-}
loading : Html msg
loading =
    loading_ []


loading_ : List Css.Style -> Html msg
loading_ withCss =
    Html.div
        [ Attributes.css
            ([ Css.backgroundColor Colors.blueDeep
             , Css.position Css.fixed
             , Css.displayFlex
             , Css.alignItems Css.center
             , Css.justifyContent Css.center
             , Css.width (Css.vw 100)
             , Css.height (Css.vh 100)
             , Css.top Css.zero
             , Css.left Css.zero
             , Css.zIndex (Css.int 10000)
             ]
                ++ withCss
            )
        ]
        [ UiIcon.edit
            |> Svg.withLabel "Loading..."
            |> Svg.withColor Colors.white
            |> Svg.withWidth (Css.px 100)
            |> Svg.withHeight (Css.px 100)
            |> Svg.withCss
                [ Css.property "animation-duration" "1s"
                , Css.property "animation-iteration-count" "infinite"
                , Css.animationName rotateKeyframes
                , Css.property "animation-timing-function" "linear"
                ]
            |> Svg.toHtml
        ]


rotateKeyframes : Css.Animations.Keyframes {}
rotateKeyframes =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.transform [ Css.rotate (Css.deg -360) ] ] )
        ]


fadeInKeyframes : Css.Animations.Keyframes {}
fadeInKeyframes =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.opacity Css.zero ] )
        , ( 100, [ Css.Animations.opacity (Css.num 1) ] )
        ]



-- INTERNAL


type alias Config msg =
    { emoji : String
    , title : String
    , subtitle : String
    , defaultPage : Maybe (DefaultPage msg)
    , details : Maybe String
    }


view : Config msg -> Html msg
view config =
    viewContainer
        [ viewEmoji [ Html.text config.emoji ]
        , Text.heading [ Html.text config.title ]
        , Text.tagline [ Html.text config.subtitle ]
        , viewButton
            [ viewExit config ]
        , viewButton
            [ Button.linkExternal
                { label = "Get help!"
                , icon = Nothing
                , url = "https://noredink.zendesk.com/hc/en-us"
                , size = Button.Large
                , style = Button.Secondary
                , width = Button.WidthExact 260
                }
            ]
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
                { onClick = defaultPage.link
                , size = Button.Large
                , style = Button.Primary
                , width = Button.WidthExact 260
                }
                { label =
                    case defaultPage.recoveryText of
                        ReturnTo name ->
                            "Return to " ++ name

                        Reload ->
                            "Try again"

                        Custom text ->
                            text
                , state = Button.Enabled
                , icon = Nothing
                }

        Nothing ->
            Button.link
                { label = "Return to dashboard"
                , icon = Nothing
                , url = "/"
                , size = Button.Large
                , style = Button.Primary
                , width = Button.WidthExact 260
                }


viewDetails : String -> Html msg
viewDetails detailsForEngineers =
    Html.div []
        [ Html.styled Html.details
            [ margin (px 10)
            , maxWidth (px 700)
            ]
            []
            [ Html.styled Html.summary
                [ color (hex "8F8F8F") ]
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
