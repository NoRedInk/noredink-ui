module InputLabelInternal exposing (view)

{-|

@docs view

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Style as Accessibility
import Css
import Html.Styled.Attributes as Attributes
import InputErrorInternal exposing (ErrorState)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.InputStyles.V3 as InputStyles exposing (Theme)


{-| -}
view :
    { for : String, label : String, theme : Theme }
    -> { config | error : ErrorState, noMarginTop : Bool, hideLabel : Bool }
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
            [ InputStyles.label theme (InputErrorInternal.getIsInError config.error)
            , if config.noMarginTop then
                Css.top (Css.px -InputStyles.defaultMarginTop)

              else
                Css.batch []
            ]
         ]
            ++ extraStyles
        )
        [ Html.text label ]
