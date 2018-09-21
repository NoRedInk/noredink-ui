module Nri.Ui.Select.V4
    exposing
        ( Config
        , view
        )

{-| Build a select input.


## Upgrading to V4


## The api doesn't change for this version, only the styles.


# Configure

@docs Config


# Render

@docs view

-}

import Css
import Css.Foreign
import Dict
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode exposing (Decoder, andThen, succeed)
import Nri.Ui
import Nri.Ui.Colors.V1
import Nri.Ui.InputStyles.V2 as InputStyles exposing (Theme)
import Nri.Ui.Util


{-| Select-specific Choice.Config
-}
type alias Config a =
    { choices : List { label : String, value : a }
    , current : a
    }


{-| TODO: Consider moving this to Nri.Ui.Util once the non-0.19-approved `toString` is removed
-}
niceId : String -> a -> String
niceId prefix x =
    prefix ++ "-" ++ Nri.Ui.Util.dashify (Nri.Ui.Util.removePunctuation (toString x))


{-| A normal select dropdown
-}
view : Config a -> Html a
view config =
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
                [ Attributes.id (niceId "nri-select" choice.value)
                , Attributes.value (niceId "nri-select" choice.value)
                , Attributes.selected (choice.value == config.current)
                ]
                [ Html.text choice.label ]
    in
    config.choices
        |> List.map viewChoice
        |> Nri.Ui.styled Html.select
            "nri-select-menu"
            [ InputStyles.input InputStyles.Standard False
            , Css.Foreign.withClass "override-sass-styles"
                [ Css.height (Css.px 45)
                ]
            ]
            [ onSelectHandler, class "override-sass-styles" ]
