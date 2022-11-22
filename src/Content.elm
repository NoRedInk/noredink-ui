module Content exposing (..)

{-| -}

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Markdown


{-| Provide a plain-text string.
-}
plaintext :
    String
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
plaintext content config =
    { config | content = [ text content ] }


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraphPlaintext :
    String
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
paragraphPlaintext content config =
    { config | content = [ p [ css [ Css.margin Css.zero ] ] [ text content ] ] }


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
