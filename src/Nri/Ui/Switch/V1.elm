module Nri.Ui.Switch.V1 exposing (view, Attribute, onSwitch, id, label)

{-|

@docs view, Attribute, onSwitch, id, label

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Widget as Widget
import Html.Styled as WildWildHtml
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


type Attribute msg
    = OnSwitch (Bool -> msg)
    | Id String
    | Label (Html msg)


onSwitch : (Bool -> msg) -> Attribute msg
onSwitch =
    OnSwitch


id : String -> Attribute msg
id =
    Id


{-| Labeling text requirements: should be descriptive but not interactive
(that is, this API would be `Html Never` if it was ergonomic to do so)
and should be styled so that it can be displayed inline.
-}
label : Html msg -> Attribute msg
label =
    Label


type alias Config msg =
    { onSwitch : Maybe (Bool -> msg)
    , id : String
    , label : Maybe (Html msg)
    }


defaultConfig : Config msg
defaultConfig =
    { onSwitch = Nothing
    , id = "nri-ui-switch-with-default-id"
    , label = Nothing
    }


customize : Attribute msg -> Config msg -> Config msg
customize attr config =
    case attr of
        OnSwitch onSwitch_ ->
            { config | onSwitch = Just onSwitch_ }

        Id id_ ->
            { config | id = id_ }

        Label label_ ->
            { config | label = Just label_ }


view : List (Attribute msg) -> Bool -> Html msg
view attrs isOn =
    let
        config =
            List.foldl customize defaultConfig attrs
    in
    WildWildHtml.span
        [ Attributes.id (config.id ++ "-container") ]
        [ viewCheckbox
            { id = config.id
            , onCheck = config.onSwitch
            , checked = isOn
            }
        , WildWildHtml.label
            [ Attributes.for config.id ]
            [ Html.text "TODO: switch"
            , Maybe.withDefault (Html.text "") config.label
            ]
        ]


viewCheckbox :
    { id : String
    , onCheck : Maybe (Bool -> msg)
    , checked : Bool
    }
    -> Html msg
viewCheckbox config =
    Html.checkbox config.id
        (Just config.checked)
        (List.concat
            [ [ Attributes.id config.id ]
            , case config.onCheck of
                Just onCheck ->
                    [ Events.onCheck onCheck ]

                Nothing ->
                    [ Attributes.disabled True
                    , Widget.disabled True
                    ]
            ]
        )
