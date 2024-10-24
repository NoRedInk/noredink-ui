module Content exposing (..)

{-| -}

import Css
import Html as Unstyled
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Markdown
import Markdown.Block
import Markdown.Inline


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
paragraph :
    String
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
paragraph content config =
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


{-| -}
markdownContent :
    String
    -> List (Html msg)
markdownContent content =
    Markdown.toHtml Nothing content
        |> List.map fromUnstyled


{-| Provide a string that will be rendered as inline markdown, with the default wrapping paragraph removed.
-}
markdownInline : String -> List (Html msg)
markdownInline content =
    markdownInlineUnstyled content
        |> List.map fromUnstyled


{-| Provide a string that will be rendered as inline markdown, with the default wrapping paragraph removed.
-}
markdownInlineUnstyled : String -> List (Unstyled.Html msg)
markdownInlineUnstyled content =
    case Markdown.Block.parse Nothing content of
        [ Markdown.Block.Paragraph _ inlines ] ->
            -- The library always parses into a paragraph and never into `PlainInline`
            List.map Markdown.Inline.toHtml inlines

        _ ->
            Markdown.toHtml Nothing content


{-| Provide a list of custom HTML.
-}
html :
    List (Html msg)
    -> { config | content : List (Html msg) }
    -> { config | content : List (Html msg) }
html content config =
    { config | content = content }
