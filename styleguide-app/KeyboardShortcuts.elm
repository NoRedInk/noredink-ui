module KeyboardShortcuts exposing
    ( view
    , KeyboardShortcut
    , Key(..), Direction(..)
    )

{-|

@docs view
@docs KeyboardShortcut
@docs Key, Direction

-}

import Html.Styled as Html exposing (..)


{-| -}
type alias KeyboardShortcut =
    { keys : List Key
    , result : String
    }


{-| -}
type Key
    = Shift
    | Enter
    | Arrow Direction
    | Tab


{-| -}
type Direction
    = Up
    | Right
    | Down
    | Left


{-| -}
view : List KeyboardShortcut -> Html msg
view keyboardShortcuts =
    div []
        [ text "TODO"
        ]
