module Nri.Ui.PremiumCheckbox.V7 exposing
    ( view
    , selected, partiallySelected
    , premium, showPennant
    , disabled, enabled
    )

{-|


# Changes from V6

  - Move the Premium pennant to the left of the checkbox
  - list based API instead of record based

@docs view
@docs selected, partiallySelected


### Content

@docs premium, showPennant


### Attributes

@docs Attribute
@docs disabled, enabled

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Pennant.V2 exposing (premiumFlag)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Util exposing (removePunctuation)
import String exposing (toLower)
import String.Extra exposing (dasherize)


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
premium :
    { teacherPremiumLevel : PremiumLevel
    , contentPremiumLevel : PremiumLevel
    }
    -> Attribute msg
premium { teacherPremiumLevel, contentPremiumLevel } =
    Attribute <|
        \config ->
            { config
                | teacherPremiumLevel = Just teacherPremiumLevel
                , contentPremiumLevel = Just contentPremiumLevel
            }


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
    , teacherPremiumLevel : Maybe PremiumLevel
    , contentPremiumLevel : Maybe PremiumLevel
    , isDisabled : Bool
    , containerCss : List Css.Style
    , selected : Checkbox.IsSelected
    , premiumMsg : Maybe msg
    }


emptyConfig : Config msg
emptyConfig =
    { id = Nothing
    , teacherPremiumLevel = Nothing
    , contentPremiumLevel = Nothing
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
            Maybe.map2 PremiumLevel.allowedFor config.contentPremiumLevel config.teacherPremiumLevel
                |> Maybe.withDefault True
                |> not
    in
    Html.div [ css config.containerCss ]
        [ case config.contentPremiumLevel of
            Just PremiumLevel.Premium ->
                viewPremiumFlag

            Just PremiumLevel.PremiumWithWriting ->
                viewPremiumFlag

            _ ->
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
