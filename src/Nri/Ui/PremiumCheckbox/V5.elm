module Nri.Ui.PremiumCheckbox.V5 exposing (PremiumConfig, premium, Pennant(..))

{-|

@docs PremiumConfig, premium, Pennant

-}

import Accessibility.Styled as Html
import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox


{-|

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
type alias PremiumConfig msg =
    { label : String
    , id : String
    , selected : Checkbox.IsSelected
    , disabled : Bool
    , isLocked : Bool
    , pennant : Maybe Pennant
    , onChange : Bool -> msg
    , onLockedClick : msg
    }


{-| Premium is the yellow "P" pennant
PremiumWithWriting is the yellow "P+" pennant
-}
type Pennant
    = Premium
    | PremiumWithWriting


{-| A checkbox that should be used for premium content
-}
premium : PremiumConfig msg -> Html.Html msg
premium config =
    Html.div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ Checkbox.viewWithLabel
            { identifier = config.id
            , label = config.label
            , setterMsg =
                if config.isLocked then
                    \_ -> config.onLockedClick

                else
                    config.onChange
            , selected = config.selected
            , disabled = config.disabled
            , theme =
                if config.isLocked then
                    Checkbox.Locked

                else
                    Checkbox.Square
            }
        , case config.pennant of
            Just pennant ->
                Html.div
                    [ Attributes.class "premium-checkbox-V5__PremiumClass"
                    , css
                        [ property "content" "''"
                        , display inlineBlock
                        , width (px 26)
                        , height (px 24)
                        , marginLeft (px 8)

                        -- , backgroundImage
                        --     (case pennant of
                        --         Premium ->
                        --             assets.iconPremiumFlag_svg
                        --         PremiumWithWriting ->
                        --             assets.iconPremiumWithWritingFlag_svg
                        --     )
                        , backgroundRepeat noRepeat
                        , backgroundPosition center
                        ]
                    ]
                    []

            Nothing ->
                Html.text ""
        ]
