module Nri.Ui.FocusTrap.V1 exposing
    ( first, last
    , only
    )

{-| Create a focus trap.

@docs first, last
@docs only

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Browser.Events
import Html.Styled.Attributes as Attributes exposing (class, id)
import Html.Styled.Events as Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Task


{-| -}
only : { focusSelf : msg } -> List (Html.Attribute msg)
only { focusSelf } =
    [ onKeyDownPreventDefault
        [ Key.tab focusSelf
        , Key.tabBack focusSelf
        ]
    , class "focus-trap__only-focusable-element"
    ]


{-| -}
first : { focusLastId : msg } -> List (Html.Attribute msg)
first { focusLastId } =
    [ onKeyDownPreventDefault [ Key.tabBack focusLastId ]
    , class "focus-trap__first-focusable-element"
    ]


{-| -}
last : { focusFirstId : msg } -> List (Html.Attribute msg)
last { focusFirstId } =
    [ onKeyDownPreventDefault [ Key.tab focusFirstId ]
    , class "focus-trap__last-focusable-element"
    ]


onKeyDownPreventDefault : List (Decoder msg) -> Html.Attribute msg
onKeyDownPreventDefault decoders =
    Events.preventDefaultOn "keydown"
        (Decode.oneOf (List.map (Decode.map (\msg -> ( msg, True ))) decoders))
