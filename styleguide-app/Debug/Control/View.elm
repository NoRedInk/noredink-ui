module Debug.Control.View exposing (codeFromList, view)

import Css exposing (..)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import Example
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Url.Builder


{-| -}
view :
    { name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , toExampleCode : a -> List { sectionName : String, code : String }
    }
    -> Html msg
view config =
    let
        value =
            Control.currentValue config.settings
    in
    div
        [ css
            [ displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            ]
        ]
        [ viewSection "Settings" <|
            [ fromUnstyled (Control.view config.update config.settings) ]
        , viewExampleCode config (config.toExampleCode value)
        ]


viewExampleCode :
    { component | name : String, version : Int }
    -> List { sectionName : String, code : String }
    -> Html msg
viewExampleCode component values =
    viewSection "Generated Code" <|
        List.concatMap
            (\example ->
                [ details
                    []
                    [ summary []
                        [ Heading.h4
                            [ Heading.css [ Css.display Css.inline ]
                            ]
                            [ text example.sectionName ]
                        ]
                    , ClickableText.link ("View " ++ example.sectionName ++ " example on Ellie")
                        [ ClickableText.linkExternal (generateEllieLink component example)
                        , ClickableText.small
                        ]
                    , code
                        [ css
                            [ display block
                            , whiteSpace preWrap
                            , Css.marginTop (px 8)
                            ]
                        ]
                        [ text example.code ]
                    ]
                ]
            )
            values


generateEllieLink : { component | name : String, version : Int } -> { sectionName : String, code : String } -> String
generateEllieLink component { sectionName, code } =
    Url.Builder.crossOrigin "https://ellie-app.com/a/example/v1"
        []
        [ Url.Builder.string "title" (component.name ++ " | " ++ sectionName)
        , Url.Builder.string "elmcode" (generateElmExampleModule component code)
        , Url.Builder.string "htmlcode" ellieHtmlSetup
        , -- At some point, a system of some kind will be required to keep these values
          -- in line with the allowed elm json values.
          -- I think in most cases, the API to use a noredink-ui component should require _only_ the following
          -- packages. Feel free to add packages if it seems necessary!
          Url.Builder.string "packages" "elm/core@1.0.5"
        , Url.Builder.string "packages" "elm/html@1.0.0"
        , Url.Builder.string "packages" "rtfeldman/elm-css@17.0.5"
        , Url.Builder.string "packages" "NoRedInk/noredink-ui@15.8.1"
        , Url.Builder.string "packages" "pablohirafuji/elm-markdown@2.0.5"
        , Url.Builder.string "elmversion" "0.19.1"
        ]


generateElmExampleModule : { component | name : String, version : Int } -> String -> String
generateElmExampleModule component code =
    [ "module Main exposing (main)"
    , ""
    , "import Css exposing (Style)"
    , "import Html as RootHtml"
    , "import Html.Styled exposing (..)"
    , "import Nri.Ui.Colors.V1 as Colors"
    , "import Nri.Ui.UiIcon.V1 as UiIcon"
    , "import " ++ Example.fullName component ++ " as " ++ component.name
    , ""
    , "main : RootHtml.Html msg"
    , "main ="
    , "    " ++ code
    , "    |> toUnstyled"
    ]
        |> String.join "\n"
        |> String.replace "\t" "    "


ellieHtmlSetup : String
ellieHtmlSetup =
    """
    <html> <head></head>
    <body>
      <main></main>
      <script>
        var app = Elm.Main.init({ node: document.querySelector('main') })
      </script>
    </body>
    </html>
    """


viewSection : String -> List (Html msg) -> Html msg
viewSection name children =
    section [ css [ flex (int 1) ] ]
        (Heading.h3 [] [ text name ]
            :: children
        )


codeFromList : List ( String, a ) -> String
codeFromList list =
    "\n\t[ "
        ++ String.join "\n\t, " (List.map Tuple.first list)
        ++ "\n\t] "
