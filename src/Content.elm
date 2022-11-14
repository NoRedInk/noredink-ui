module Content exposing (..)

{-| -}

import Html.Styled exposing (..)
import Markdown


{-| Provide a plain-text string.
-}
plaintext :
    String
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
plaintext content config =
    { config | content = [ text content ] }


{-| Provide a string that will be rendered as markdown.
-}
markdown :
    String
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
markdown content config =
    { config
        | content =
            Markdown.toHtml Nothing content
                |> List.map fromUnstyled
    }


{-| Provide a list of custom HTML.
-}
html :
    List (Html msg)
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
html content config =
    { config | content = content }
