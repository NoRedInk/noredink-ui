module Nri.Ui.BorderlessButton.V1 exposing (Size(..), button)

import Css
import Css.Global
import Html.Styled as Html exposing (..)
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Icon.V4 as Icon exposing (IconType)


type Size
    = Small
    | Medium
    | Large


type alias Config =
    { label : String
    , size : Size
    , icon : Maybe IconType -- TODO: maybe do something else?
    }


button : Config -> Html msg
button config =
    let
        fontSize =
            case config.size of
                Small ->
                    15

                Medium ->
                    17

                Large ->
                    20

        icon =
            case config.icon of
                Just iconType ->
                    -- TODO: We should never use an image here, only SVG
                    Nri.Ui.styled Html.span
                        "icon-holder"
                        [ Css.height (Css.px fontSize)
                        , Css.width (Css.px fontSize)
                        , Css.display Css.inlineBlock
                        , Css.marginRight (Css.px 5)
                        ]
                        []
                        [ Icon.decorativeIcon iconType ]

                Nothing ->
                    text ""
    in
    Nri.Ui.styled Html.button
        "borderless-button-v1"
        [ Css.cursor Css.pointer
        , -- Specifying the font can and should go away after bootstrap is removed from application.css
          Nri.Ui.Fonts.V1.baseFont
        , Css.backgroundImage Css.none
        , Css.textShadow Css.none
        , Css.property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
        , Css.boxShadow Css.none
        , Css.border Css.zero
        , Css.disabled [ Css.cursor Css.notAllowed ]
        , Css.color Colors.azure
        , Css.backgroundColor Css.transparent
        , Css.fontWeight (Css.int 600)
        , Css.textAlign Css.left
        , Css.borderStyle Css.none
        , Css.hover [ Css.textDecoration Css.underline ]
        , Css.fontSize (Css.px fontSize)
        ]
        []
        [ icon
        , text config.label
        ]
