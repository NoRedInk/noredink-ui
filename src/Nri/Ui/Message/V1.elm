module Nri.Ui.Message.V1 exposing
    ( tiny
    , Theme(..), Content(..)
    , somethingWentWrong
    )

{-|

@docs tiny
@docs Theme, Content

@docs somethingWentWrong

-}

import Accessibility.Styled exposing (..)
import Css exposing (..)
import Css.Global
import Html.Styled exposing (fromUnstyled, styled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SpriteSheet as SpriteSheet
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


type Theme
    = Error
    | Warning
    | Tip
    | Success


type Content msg
    = Plain String
    | Markdown String
    | Html (Html msg)


tiny : Theme -> Content msg -> Html msg
tiny theme content =
    let
        children =
            case theme of
                Error ->
                    [ exclamation Colors.purple
                    , alertString Colors.purple content
                    ]

                Warning ->
                    [ exclamation Colors.red
                    , alertString Colors.redDark content
                    ]

                Tip ->
                    [ iconContainer [ color Colors.yellow ] (NriSvg.toHtml SpriteSheet.bulb)
                    , alertString Colors.navy content
                    ]

                Success ->
                    [ iconContainer
                        [ color Colors.white
                        , backgroundColor Colors.green
                        ]
                        (div
                            [ css [ width (px 12), marginTop (px 1) ] ]
                            [ NriSvg.toHtml SpriteSheet.checkmark ]
                        )
                    , alertString Colors.greenDarkest content
                    ]
    in
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--tiny"
        [ displayFlex
        , justifyContent start
        , alignItems flexStart
        , paddingTop (px 6)
        , paddingBottom (px 8)
        ]
        []
        children


{-| Shows an appropriate error message for when something unhandled happened.
-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    div []
        [ tiny Error (Plain "Sorry, something went wrong.  Please try again later.")
        , details []
            [ summary
                [ css
                    [ Fonts.baseFont
                    , fontSize (px 14)
                    , color Colors.gray45
                    ]
                ]
                [ text "Details for NoRedInk engineers" ]
            , code
                [ css
                    [ display block
                    , whiteSpace normal
                    , overflowWrap breakWord
                    , color Colors.gray45
                    , backgroundColor Colors.gray96
                    , border3 (px 1) solid Colors.gray92
                    , borderRadius (px 3)
                    , padding2 (px 2) (px 4)
                    , fontSize (px 12)
                    , fontFamily monospace
                    ]
                ]
                [ text errorMessageForEngineers ]
            ]
        ]



--
-- PRIVATE
--


exclamation : Color -> Html msg
exclamation iconColor =
    iconContainer
        [ color Colors.white
        , backgroundColor iconColor
        ]
        (styled div
            [ height (px 13)
            , marginTop (px 1)
            ]
            []
            [ NriSvg.toHtml SpriteSheet.exclamationMark ]
        )


iconContainer : List Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--iconContainer"
        (styles
            ++ [ -- Content positioning
                 displayFlex
               , alignItems center
               , justifyContent center
               , marginRight (px 5)
               , lineHeight (px 13)
               , flexShrink zero

               -- Size
               , borderRadius (px 13)
               , height (px 20)
               , width (px 20)
               ]
        )
        []
        [ icon ]


alertString : ColorValue compatible -> Content msg -> Html msg
alertString textColor content =
    let
        children =
            case content of
                Plain stringContent ->
                    [ text stringContent ]

                Markdown markdownContent ->
                    Markdown.toHtml Nothing markdownContent |> List.map fromUnstyled

                Html html ->
                    [ html ]
    in
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--alert"
        [ color textColor
        , Fonts.baseFont
        , fontSize (px 13)

        --, lineHeight (px 20)
        , listStyleType none

        -- This global selector and overrides are necessary due to
        -- old stylesheets used on the monolith that set the
        -- `.txt p { font-size: 18px; }` -- without these overrides,
        -- we may see giant ugly alerts.
        -- Remove these if you want to! but be emotionally prepped
        -- to deal with visual regressions. 🙏
        , Css.Global.descendants
            [ Css.Global.p
                [ margin zero

                --, lineHeight (px 20)
                , fontSize (px 13)
                , Fonts.baseFont
                ]
            ]
        ]
        []
        children
