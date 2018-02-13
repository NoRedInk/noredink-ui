module Nri.Ui.Styles.V1 exposing (Keyframe, Styles, StylesWithAssets, keyframes, styles, stylesWithAssets, stylesWithExtraStylesheets, toString)

{-| Simplifies using elm-css in modules that expose views


## Creating namespaces stylesheets

@docs Styles, StylesWithAssets, styles, stylesWithExtraStylesheets, stylesWithAssets


## Helpers for CSS


### Keyframe animations

@docs Keyframe, keyframes, toString

-}

import Css
import Css.Namespace
import Html exposing (Attribute, Html)
import Html.CssHelpers


{-|

  - `css`: The resulting stylesheets to add to `Nri.Ui.Css.Site`
  - `id`, `class`, `classList`: The Html.Attribute helper functions

-}
type alias Styles id class msg =
    StylesWithAssets id class msg ()


{-| Styles that depend on assets, for instance because they use a background
image somewhere.
-}
type alias StylesWithAssets id class msg assets =
    { css : assets -> List Css.Stylesheet
    , id : id -> Attribute msg
    , class : List class -> Attribute msg
    , classList : List ( class, Bool ) -> Attribute msg
    , div : class -> List (Html msg) -> Html msg
    , span : class -> List (Html msg) -> Html msg
    }


{-| Use this instead of Html.CssHelpers.
This will help you make sure you don't mismatch the namespace you're using.

    { css, class } =
        Nri.Ui.Styles.styles "Nri-Ui-MyWidget-"
            [ Css.class Container
                [ backgroundColor Nri.Colors.ochre ]
            ]

-}
styles :
    String
    -> List Css.Snippet
    -> Styles id class msg
styles namespace snippets =
    stylesWithExtraStylesheets namespace [] snippets


{-| Use this instead of [`styles`](#styles) if you need to
include Css.Stylesheets from external packages,
or if you need to add styles without a namespace.
Remember, all stylesheets given here will be included
on all pages of the site.
-}
stylesWithExtraStylesheets :
    String
    -> List Css.Stylesheet
    -> List Css.Snippet
    -> Styles id class msg
stylesWithExtraStylesheets namespace extras snippets =
    let
        { id, class, classList } =
            Html.CssHelpers.withNamespace namespace

        sheet =
            snippets
                |> Css.Namespace.namespace namespace
                |> Css.stylesheet
    in
    { css = \assets -> sheet :: extras
    , id = id
    , class = class
    , classList = classList
    , div = \cl -> Html.div [ class [ cl ] ]
    , span = \cl -> Html.span [ class [ cl ] ]
    }


{-| Use this if your styles depend on asset files
-}
stylesWithAssets :
    String
    -> (assets -> List Css.Snippet)
    -> StylesWithAssets id class msg assets
stylesWithAssets namespace snippets =
    let
        { id, class, classList } =
            Html.CssHelpers.withNamespace namespace

        sheet assets =
            snippets assets
                |> Css.Namespace.namespace namespace
                |> Css.stylesheet
    in
    { css = \assets -> [ sheet assets ]
    , id = id
    , class = class
    , classList = classList
    , div = \cl -> Html.div [ class [ cl ] ]
    , span = \cl -> Html.span [ class [ cl ] ]
    }


{-| A CSS keyframe animation that will have vendor prefixes automatically added.
-}
type Keyframe
    = CompiledKeyframe String


{-| Create a CSS keyframe animation with appropriate vendor prefixes
-}
keyframes : String -> List ( String, String ) -> Keyframe
keyframes name stops =
    let
        stop ( when, what ) =
            when ++ " {" ++ what ++ "}"

        x prefix =
            "@"
                ++ prefix
                ++ "keyframes "
                ++ name
                ++ " {\n"
                ++ String.join "\n" (List.map stop stops)
                ++ "\n}\n"
    in
    [ "-webkit-", "-moz-", "" ]
        |> List.map x
        |> String.join ""
        |> CompiledKeyframe


{-| Turn a [`Keyframe`](#Keyframe) into a string that can be included in a CSS stylesheet.
-}
toString : Keyframe -> String
toString keyframe =
    case keyframe of
        CompiledKeyframe compiled ->
            compiled
