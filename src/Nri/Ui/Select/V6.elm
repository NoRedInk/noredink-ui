module Nri.Ui.Select.V6 exposing (Config, Choice, view)

{-| Build a select input.

@docs Config, Choice, view

-}

import Css
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode exposing (Decoder)
import Nri.Ui
import Nri.Ui.Colors.V1
import Nri.Ui.Util


{-| Configure a Select
-}
type alias Config a =
    { choices : List (Choice a)
    , current : Maybe a
    , id : Maybe String
    , valueToString : a -> String
    , defaultDisplayText : Maybe String
    }


{-| A single possible choice.
-}
type alias Choice a =
    { label : String, value : a }


niceId : String -> String
niceId x =
    "nri-select-" ++ Nri.Ui.Util.dashify (Nri.Ui.Util.removePunctuation x)


{-| A select dropdown
-}
view : Config a -> Html a
view config =
    let
        valueLookup =
            config.choices
                |> List.map (\x -> ( niceId (config.valueToString x.value), x.value ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault
                    -- At present, elm/virtual-dom throws this failure away.
                    (Json.Decode.fail
                        ("Nri.Select: could not decode the value: "
                            ++ string
                            ++ "\nexpected one of: "
                            ++ String.join ", " (Dict.keys valueLookup)
                        )
                    )

        onSelectHandler =
            Events.on "change" (Events.targetValue |> Json.Decode.andThen decodeValue)

        defaultOption =
            config.defaultDisplayText
                |> Maybe.map (viewDefaultChoice config.current >> List.singleton)
                |> Maybe.withDefault []

        extraAttrs =
            config.id
                |> Maybe.map (\id -> [ Attributes.id id ])
                |> Maybe.withDefault []

        currentVal =
            if config.current == Nothing && config.defaultDisplayText == Nothing then
                config.choices
                    |> List.head
                    |> Maybe.map .value

            else
                config.current
    in
    config.choices
        |> List.map (viewChoice currentVal config.valueToString)
        |> (++) defaultOption
        |> Nri.Ui.styled Html.select
            "nri-select-menu"
            [ Css.backgroundColor Nri.Ui.Colors.V1.white
            , Css.border3 (Css.px 1) Css.solid Nri.Ui.Colors.V1.gray75
            , Css.borderRadius (Css.px 8)
            , Css.color Nri.Ui.Colors.V1.gray20
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 15)
            , Css.height (Css.px 45)
            , Css.width (Css.pct 100)
            ]
            ([ onSelectHandler ] ++ extraAttrs)


viewDefaultChoice : Maybe a -> String -> Html a
viewDefaultChoice current displayText =
    Html.option
        [ Attributes.selected (current == Nothing)
        , Attributes.disabled True
        ]
        [ Html.text displayText ]


viewChoice : Maybe a -> (a -> String) -> Choice a -> Html a
viewChoice current toString choice =
    let
        isSelected =
            current
                |> Maybe.map ((==) choice.value)
                |> Maybe.withDefault False
    in
    Html.option
        [ Attributes.id (niceId (toString choice.value))
        , Attributes.value (niceId (toString choice.value))
        , Attributes.selected isSelected
        ]
        [ Html.text choice.label ]
