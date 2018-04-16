module Nri.Ui.Select.V1
    exposing
        ( Config
        , customView
        , view
        )

{-| Build a select input.


# Configure

@docs Config


# Render

@docs view, customView

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, andThen, succeed)
import Nri.Ui.Util


{-| Select-specific Choice.Config
-}
type alias Config a =
    { choices : List { label : String, value : a }
    , current : a
    }


{-| TODO: move this to View.Extra
-}
niceId : String -> a -> String
niceId prefix x =
    x
        |> toString
        |> Nri.Ui.Util.removePunctuation
        |> Nri.Ui.Util.dashify
        |> (++) (prefix ++ "-")


{-| A normal select dropdown
-}
view : Config a -> Html a
view config =
    customView [] config


{-| A select dropdown with custom attributes.
This should be phased out as the new Select style is implemented,
all pages should use the new, consistent style.
-}
customView : List (Html.Attribute a) -> Config a -> Html a
customView attributes config =
    let
        valueLookup =
            -- TODO: probably worth using Lazy here, since choices won't change often
            config.choices
                |> List.map (\x -> ( niceId "nri-select" x.value, x.value ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail ("Nri.Select: could not decode the value: " ++ toString string ++ "\nexpected one of: " ++ toString (Dict.keys valueLookup)))

        onSelectHandler =
            on "change" (targetValue |> andThen decodeValue)

        viewChoice choice =
            Html.option
                [ Html.Attributes.id (niceId "nri-select" choice.value)
                , Html.Attributes.value (niceId "nri-select" choice.value)
                , Html.Attributes.selected (choice.value == config.current)
                ]
                [ Html.text choice.label ]
    in
    config.choices
        |> List.map viewChoice
        |> Html.select (onSelectHandler :: attributes)
