module EllieLink exposing (Config, SectionExample, view)

import Accessibility.Styled.Aria as Aria
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Http
import Nri.Ui.Button.V10 as Button
import Url.Builder


type alias Config =
    { packageDependencies : Result Http.Error (Dict String String)
    }


type alias SectionExample =
    { name : String
    , fullModuleName : String
    , sectionName : String
    , mainType : Maybe String
    , extraCode : List String
    , renderExample : String -> String
    , code : String
    }


view : Config -> SectionExample -> Html msg
view config example =
    let
        linkName =
            "Open in Ellie"
    in
    Button.link linkName
        [ Button.linkExternal (generateEllieLink config example)
        , Button.tertiary
        , Button.small
        , Button.custom [ Aria.label (example.sectionName ++ " " ++ linkName) ]
        ]


generateEllieLink : Config -> SectionExample -> String
generateEllieLink config example =
    let
        packageDependencies =
            config.packageDependencies
                |> Result.map Dict.toList
                |> Result.withDefault []
                |> List.map toPackageDependencyQueryString

        toPackageDependencyQueryString ( name, version ) =
            Url.Builder.string "packages" (name ++ "@" ++ version)
    in
    Url.Builder.crossOrigin "https://ellie-app.com/a/example/v1"
        []
        ([ Url.Builder.string "title" (example.name ++ " | " ++ example.sectionName)
         , Url.Builder.string "elmcode" (generateElmExampleModule config example)
         , Url.Builder.string "htmlcode" ellieHtmlSetup
         , Url.Builder.string "elmversion" "0.19.1"
         ]
            ++ packageDependencies
        )


generateElmExampleModule : Config -> SectionExample -> String
generateElmExampleModule config example =
    let
        maybeErrorMessages =
            case config.packageDependencies of
                Err httpError ->
                    [ "{- "
                    , "Something went wrong fetching the package dependencies!"
                    , "You will need to install the packages by hand for this code to compile."
                    , ""
                    , "Error: "
                    , Debug.toString httpError
                    , "-}"
                    ]

                Ok _ ->
                    []
    in
    [ "module Main exposing (main)"
    , ""
    , "import Css exposing (Style)"
    , "import Html as RootHtml"
    , "import Html.Styled exposing (..)"
    , "import Nri.Ui.Colors.V1 as Colors"
    , "import Nri.Ui.UiIcon.V1 as UiIcon"
    , "import Nri.Ui.Svg.V1 as Svg"
    , "import Nri.Ui.ClickableSvg.V2 as ClickableSvg"
    , "import " ++ example.fullModuleName ++ " as " ++ example.name
    , String.join "\n" example.extraCode
    , ""
    , ""
    ]
        ++ maybeErrorMessages
        ++ [ Maybe.map (\type_ -> "main : " ++ type_) example.mainType
                |> Maybe.withDefault ""
           , "main =" ++ example.renderExample example.code
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
