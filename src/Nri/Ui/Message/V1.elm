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

import Accessibility.Styled as Html exposing (..)
import Css exposing (..)
import Css.Global
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SpriteSheet exposing (exclamationMark)
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


type Theme
    = Error


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
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V3__alert"
        [ Css.displayFlex
        , Css.justifyContent Css.start
        , Css.alignItems Css.flexStart
        , Css.paddingTop (Css.px 6)
        , Css.paddingBottom (Css.px 8)
        ]
        []
        children


{-| Shows an appropriate error message for when something unhandled happened.
-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    Html.div []
        [ tiny Error (Plain "Sorry, something went wrong.  Please try again later.")
        , Html.details []
            [ Html.summary
                [ css
                    [ Fonts.baseFont
                    , Css.fontSize (Css.px 14)
                    , Css.color Colors.gray45
                    ]
                ]
                [ Html.text "Details for NoRedInk engineers" ]
            , Html.code
                [ css
                    [ Css.display Css.block
                    , Css.whiteSpace Css.normal
                    , Css.overflowWrap Css.breakWord
                    , Css.color Colors.gray45
                    , Css.backgroundColor Colors.gray96
                    , Css.border3 (Css.px 1) Css.solid Colors.gray92
                    , Css.borderRadius (Css.px 3)
                    , Css.padding2 (Css.px 2) (Css.px 4)
                    , Css.fontSize (Css.px 12)
                    , Css.fontFamily Css.monospace
                    ]
                ]
                [ Html.text errorMessageForEngineers ]
            ]
        ]



--
-- PRIVATE
--


exclamation : Css.Color -> Html msg
exclamation backgroundColor =
    iconContainer
        [ Css.color Colors.white
        , Css.backgroundColor backgroundColor
        ]
        (Html.div
            [ css [ Css.height (Css.px 13), Css.marginTop (Css.px 1) ] ]
            [ NriSvg.toHtml exclamationMark ]
        )


iconContainer : List Css.Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V4__iconContainer"
        (styles
            ++ [ -- Content positioning
                 Css.displayFlex
               , Css.alignItems Css.center
               , Css.justifyContent Css.center
               , Css.marginRight (Css.px 5)
               , Css.lineHeight (Css.px 13)
               , Css.flexShrink Css.zero

               -- Size
               , Css.borderRadius (Css.px 13)
               , Css.height (Css.px 20)
               , Css.width (Css.px 20)
               ]
        )
        []
        [ icon ]


alertString : Css.ColorValue compatible -> Content msg -> Html msg
alertString color content =
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
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V4--alert"
        [ Css.color color
        , Fonts.baseFont
        , Css.fontSize (Css.px 13)

        --, Css.lineHeight (Css.px 20)
        , Css.listStyleType Css.none

        -- This global selector and overrides are necessary due to
        -- old stylesheets used on the monolith that set the
        -- `.txt p { font-size: 18px; }` -- without these overrides,
        -- we may see giant ugly alerts.
        -- Remove these if you want to! but be emotionally prepped
        -- to deal with visual regressions. 🙏
        , Css.Global.descendants
            [ Css.Global.p
                [ Css.margin Css.zero

                --, Css.lineHeight (Css.px 20)
                , Css.fontSize (Css.px 13)
                , Fonts.baseFont
                ]
            ]
        ]
        []
        children
