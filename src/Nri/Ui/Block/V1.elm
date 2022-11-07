module Nri.Ui.Block.V1 exposing
    ( view, Attribute
    , plaintext
    , emphasize
    )

{-|

@docs view, Attribute


## Customization

@docs plaintext
@docs emphasize

-}

import Accessibility.Styled exposing (..)


{-|

    Block.view [ Block.plaintext "Hello, world!" ]

-}
view : List (Attribute msg) -> Html msg
view attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) defaultConfig
        |> render



-- Attributes


{-| Provide the main content of the block as a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext content =
    Attribute <| \config -> { config | content = [ text content ] }


{-| Mark content as emphasized.
-}
emphasize : Attribute msg
emphasize =
    Attribute <| \config -> { config | emphasized = True }



-- Internals


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


defaultConfig : Config msg
defaultConfig =
    { content = []
    , emphasized = False
    }


type alias Config msg =
    { content : List (Html msg)
    , emphasized : Bool
    }


render : Config msg -> Html msg
render config =
    span [] config.content
