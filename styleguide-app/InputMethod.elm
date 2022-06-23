module InputMethod exposing (InputMethod(..), init, subscriptions, styles)

{-| If in the NRI monolith, please see Nri.InputMethod for the equivalent module.

Utilities for detecting input method and hiding focus rings when
appropriate. Inspired by a blog post from [David Gilbertson](https://medium.com/hackernoon/removing-that-ugly-focus-ring-and-keeping-it-too-6c8727fefcd2).

@docs InputMethod, init, subscriptions, styles

-}

import Browser.Events
import Css exposing (..)
import Css.Global exposing (Snippet)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Nri.Ui.FocusRing.V1 as FocusRing


{-| Represents the method of input the user is currently using to
iteract with our app.
-}
type InputMethod
    = Keyboard
    | Mouse


{-| Even though most users will probably be using a mouse, setting the initial
value to Keyboard makes it so that we display the focus ring when we do care
about which element is focused initially for users that rely on the keyboard.

There dont't seem to be any downsides of doing this for mouse users, since as
soon as they interact with the page the input method will be changed.

-}
init : InputMethod
init =
    Keyboard


{-| A subscription of the input method the user is currently using.
-}
subscriptions : Sub InputMethod
subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "Tab" ->
                                Decode.succeed Keyboard

                            " " ->
                                Decode.succeed Keyboard

                            _ ->
                                Decode.fail "Not a navigation key. Discarding event."
                    )
            )
        , Browser.Events.onMouseDown (Decode.succeed Mouse)
        ]


{-| A collection of global styles that will hide or show the focus ring if keyboard
navigation is detected from the user.
-}
styles : InputMethod -> List Snippet
styles inputMethod =
    case inputMethod of
        Keyboard ->
            FocusRing.forKeyboardUsers

        Mouse ->
            FocusRing.forMouseUsers
