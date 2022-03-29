module EllieLink exposing (view)

import Example
import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V3 as ClickableText
import Url.Builder


type alias SectionExample =
    { name : String
    , version : Int
    , sectionName : String
    , code : String
    }


view : SectionExample -> Html msg
view example =
    ClickableText.link ("View " ++ example.sectionName ++ " example on Ellie")
        [ ClickableText.linkExternal (generateEllieLink example)
        , ClickableText.small
        ]


generateEllieLink : SectionExample -> String
generateEllieLink example =
    Url.Builder.crossOrigin "https://ellie-app.com/a/example/v1"
        []
        [ Url.Builder.string "title" (example.name ++ " | " ++ example.sectionName)
        , Url.Builder.string "elmcode" (generateElmExampleModule example)
        , Url.Builder.string "htmlcode" ellieHtmlSetup
        , -- At some point, a system of some kind will be required to keep these values
          -- in line with the allowed elm json values.
          -- I think in most cases, the API to use a noredink-ui example should require _only_ the following
          -- packages. Feel free to add packages if it seems necessary!
          Url.Builder.string "packages" "elm/core@1.0.5"
        , Url.Builder.string "packages" "elm/html@1.0.0"
        , Url.Builder.string "packages" "rtfeldman/elm-css@17.0.5"
        , Url.Builder.string "packages" "NoRedInk/noredink-ui@15.8.1"
        , Url.Builder.string "packages" "pablohirafuji/elm-markdown@2.0.5"
        , Url.Builder.string "elmversion" "0.19.1"
        ]


generateElmExampleModule : SectionExample -> String
generateElmExampleModule example =
    [ "module Main exposing (main)"
    , ""
    , "import Css exposing (Style)"
    , "import Html as RootHtml"
    , "import Html.Styled exposing (..)"
    , "import Nri.Ui.Colors.V1 as Colors"
    , "import Nri.Ui.UiIcon.V1 as UiIcon"
    , "import " ++ Example.fullName example ++ " as " ++ example.name
    , ""
    , "main : RootHtml.Html msg"
    , "main ="
    , "    " ++ example.code
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
