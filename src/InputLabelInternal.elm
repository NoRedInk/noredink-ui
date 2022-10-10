module InputLabelInternal exposing (view)

{-|

@docs view

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Style as Accessibility
import Css
import Html.Styled.Attributes as Attributes
import InputErrorAndGuidanceInternal exposing (ErrorState)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.InputStyles.V4 as InputStyles exposing (Theme)


{-| -}
view :
    { for : String, label : String, theme : Theme }
    ->
        { config
            | error : ErrorState
            , noMarginTop : Bool
            , hideLabel : Bool
            , disabled : Bool
        }
    -> Html msg
view { for, label, theme } config =
    let
        extraStyles =
            if config.hideLabel then
                Accessibility.invisible

            else
                []
    in
    Html.label
        ([ Attributes.for for
         , Attributes.css
            [ InputStyles.label theme (InputErrorAndGuidanceInternal.getIsInError config.error)
            , Css.batch <|
                if config.disabled then
                    [ Css.backgroundColor Colors.gray92
                    , Css.color Colors.gray20
                    ]

                else
                    []
            , if config.noMarginTop then
                Css.top (Css.px -InputStyles.defaultMarginTop)

              else
                Css.batch []
            ]
         ]
            ++ extraStyles
        )
        [ Html.text label ]
