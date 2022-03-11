module Nri.Ui.PremiumCheckbox.V8 exposing
    ( view
    , selected, partiallySelected
    , premium, showPennant
    , Attribute
    , disabled, enabled
    , id
    )

{-| Changes from V7:

  - Use PremiumDisplay instead of PremiumLevel

@docs view

@docs selected, partiallySelected


### Content

@docs premium, showPennant


### Attributes

@docs Attribute
@docs disabled, enabled
@docs id

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.Pennant.V2 exposing (premiumFlag)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Util exposing (removePunctuation)
import String exposing (toLower)
import String.Extra exposing (dasherize)


{-| Set a custom ID for this checkbox and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one checkbox with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute msg
id id_ =
    Attribute (\config -> { config | id = Just id_ })


{-| This disables the input
-}
disabled : Attribute msg
disabled =
    Attribute <| \config -> { config | isDisabled = True }


{-| This enables the input, this is the default behavior
-}
enabled : Attribute msg
enabled =
    Attribute <| \config -> { config | isDisabled = False }


{-| Lock Premium content if the user does not have Premium.
-}
premium : PremiumDisplay -> Attribute msg
premium premiumDisplay =
    Attribute <| \config -> { config | premiumDisplay = premiumDisplay }


{-| Show Premium pennant on Premium content.

When a locked premium checkbox is clicked, the msg that's passed in will fire.

-}
showPennant : msg -> Attribute msg
showPennant premiumMsg =
    Attribute <| \config -> { config | premiumMsg = Just premiumMsg }


setSelectionStatus : Checkbox.IsSelected -> Attribute msg
setSelectionStatus status =
    Attribute (\config -> { config | selected = status })


{-| -}
selected : Bool -> Attribute msg
selected isSelected =
    setSelectionStatus <|
        if isSelected then
            Checkbox.Selected

        else
            Checkbox.NotSelected


{-| -}
partiallySelected : Attribute msg
partiallySelected =
    setSelectionStatus Checkbox.PartiallySelected


{-| Customizations for the RadioButton.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { id : Maybe String
    , premiumDisplay : PremiumDisplay
    , isDisabled : Bool
    , containerCss : List Css.Style
    , selected : Checkbox.IsSelected
    , premiumMsg : Maybe msg
    }


emptyConfig : Config msg
emptyConfig =
    { id = Nothing
    , premiumDisplay = PremiumDisplay.Free
    , isDisabled = False
    , containerCss =
        [ Css.displayFlex
        , Css.alignItems Css.center
        ]
    , selected = Checkbox.NotSelected
    , premiumMsg = Nothing
    }


applyConfig : List (Attribute msg) -> Config msg -> Config msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


{-| -}
view :
    { label : String
    , onChange : Bool -> msg
    }
    -> List (Attribute msg)
    -> Html msg
view { label, onChange } attributes =
    let
        config =
            applyConfig attributes emptyConfig

        idValue =
            case config.id of
                Just specificId ->
                    specificId

                Nothing ->
                    "checkbox-" ++ dasherize (removePunctuation (toLower label))

        isLocked =
            config.premiumDisplay == PremiumDisplay.PremiumLocked
    in
    Html.div [ css config.containerCss ]
        [ case config.premiumDisplay of
            PremiumDisplay.PremiumLocked ->
                viewPremiumFlag

            PremiumDisplay.PremiumUnlocked ->
                viewPremiumFlag

            PremiumDisplay.Free ->
                -- left-align the checkbox with checkboxes that _do_ have the premium pennant
                Html.div [ css [ Css.width (Css.px (iconWidth + iconRightMargin)) ] ] []
        , Checkbox.viewWithLabel
            { identifier = idValue
            , label = label
            , setterMsg =
                case ( isLocked, config.premiumMsg ) of
                    ( True, Just onLockedClick ) ->
                        \_ -> onLockedClick

                    _ ->
                        onChange
            , selected = config.selected
            , disabled = isLocked || config.isDisabled
            , theme =
                if isLocked then
                    Checkbox.Locked

                else
                    Checkbox.Square
            }
        ]


viewPremiumFlag : Html msg
viewPremiumFlag =
    premiumFlag
        |> Svg.withLabel "Premium"
        |> Svg.withWidth (Css.px iconWidth)
        |> Svg.withHeight (Css.px 30)
        |> Svg.withCss [ Css.marginRight (Css.px iconRightMargin) ]
        |> Svg.toHtml


iconWidth : Float
iconWidth =
    25


iconRightMargin : Float
iconRightMargin =
    8
