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
view : List KeyboardShortcut -> Html msg
view keyboardShortcuts =
    case keyboardShortcuts of
        [] ->
            text ""

        _ ->
            details []
                [ summary [] [ text "Keyboard Support" ]
                , ul [] (List.map viewKeyboardActions keyboardShortcuts)
                ]


viewKeyboardActions : KeyboardShortcut -> Html msg
viewKeyboardActions { keys, result } =
    li []
        [ strong [] [ text (String.join "+" (List.map keyToString keys)) ]
        , text result
        ]


{-| -}
type Key
    = Shift
    | Enter
    | Arrow Direction
    | Tab


keyToString : Key -> String
keyToString key =
    case key of
        Shift ->
            "Shift"

        Enter ->
            "Enter"

        Arrow direction ->
            directionToString direction ++ " arrow"

        Tab ->
            "Tab"


{-| -}
type Direction
    = Up
    | Right
    | Down
    | Left


directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "Up"

        Right ->
            "Right"

        Down ->
            "Down"

        Left ->
            "Left"
