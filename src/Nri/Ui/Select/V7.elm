module Nri.Ui.Select.V7 exposing (Choice, view)

{-| Build a select input.

@docs Choice, view

-}

import Color
import Css
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode exposing (Decoder)
import Nri.Ui
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Css.VendorPrefixed as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Util


{-| A single possible choice.
-}
type alias Choice a =
    { label : String, value : a }


{-| A select dropdown. Remember to add a label!
-}
view :
    { choices : List (Choice a)
    , current : Maybe a
    , id : String
    , valueToString : a -> String
    , defaultDisplayText : Maybe String
    , isInError : Bool
    }
    -> Html a
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
            [ -- border
              Css.border3 (Css.px 1)
                Css.solid
                (if config.isInError then
                    Colors.purple

                 else
                    Colors.gray75
                )
            , Css.borderBottomWidth (Css.px 3)
            , Css.borderRadius (Css.px 8)
            , Css.focus [ Css.borderColor Colors.azure ]

            -- Font and color
            , Css.color Colors.gray20
            , Fonts.baseFont
            , Css.fontSize (Css.px 15)
            , Css.fontWeight (Css.int 600)
            , Css.textOverflow Css.ellipsis
            , Css.overflow Css.hidden
            , Css.whiteSpace Css.noWrap

            -- Interaction
            , Css.cursor Css.pointer

            -- Size and spacing
            , Css.height (Css.px 45)
            , Css.width (Css.pct 100)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 30)

            -- Icons
            , selectArrowsCss
            ]
            [ onSelectHandler
            , Attributes.id config.id
            ]


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


niceId : String -> String
niceId x =
    "nri-select-" ++ Nri.Ui.Util.dashify (Nri.Ui.Util.removePunctuation x)


selectArrowsCss : Css.Style
selectArrowsCss =
    let
        color =
            Color.toRGBString (ColorsExtra.fromCssColor Colors.azure)
    in
    Css.batch
        [ """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="12px" height="16px" viewBox="0 0 12 16"><g fill=" """
            ++ color
            ++ """ "><path d="M2.10847,9.341803 C1.65347,8.886103 0.91427,8.886103 0.45857,9.341803 C0.23107,9.570003 0.11697,9.868203 0.11697,10.167103 C0.11697,10.465303 0.23107,10.763503 0.45857,10.991703 L5.12547,15.657903 C5.57977,16.114303 6.31897,16.114303 6.77537,15.657903 L11.44157,10.991703 C11.89727,10.536003 11.89727,9.797503 11.44157,9.341803 C10.98657,8.886103 10.24667,8.886103 9.79167,9.341803 L5.95007,13.182703 L2.10847,9.341803 Z"/><path d="M1.991556,6.658179 C1.536659,7.11394 0.797279,7.11394 0.3416911,6.658179 C0.1140698,6.43004 0,6.13173 0,5.83325 C0,5.53476 0.1140698,5.23645 0.3416911,5.00831 L5.008185,0.34182 C5.463081,-0.11394 6.202461,-0.11394 6.65805,0.34182 L11.32454,5.00831 C11.78031,5.4639 11.78031,6.202592 11.32454,6.658179 C10.86965,7.11394 10.13027,7.11394 9.674679,6.658179 L5.833118,2.81679 L1.991556,6.658179 Z"/></g></svg> """
            |> urlUtf8
            |> Css.property "background"
        , Css.backgroundColor Colors.white

        -- "appearance: none" removes the default dropdown arrows
        , VendorPrefixed.property "appearance" "none"
        , Css.backgroundRepeat Css.noRepeat
        , Css.property "background-position" "center right -20px"
        , Css.backgroundOrigin Css.contentBox
        ]


urlUtf8 : String -> String
urlUtf8 content =
    """url('data:image/svg+xml;utf8,""" ++ content ++ """')"""
