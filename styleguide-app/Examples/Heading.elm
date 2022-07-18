module Examples.Heading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import CommonControls
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Heading.V3 as Heading
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "Heading"


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Text, Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Heading.h1 [] [ Html.text "h1" ]
        , Heading.h2 [] [ Html.text "h2" ]
        , Heading.h3 [] [ Html.text "h3" ]
        , Heading.h4 [] [ Html.text "h4" ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                examples =
                    [ ( "h1", Heading.h1, "This is the main page heading." )
                    , ( "h2", Heading.h2, "This is a subheading" )
                    , ( "h3", Heading.h3, "This is a smallHeading" )
                    , ( "h4", Heading.h4, "This is a smallHeading" )
                    , ( "h5", Heading.h5, "This is a smallHeading" )
                    ]

                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = "RootHtml.Html msg"
                , extraImports = []
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode ( name, _, content ) =
                                { sectionName = name
                                , code =
                                    moduleName
                                        ++ "."
                                        ++ name
                                        ++ "\n    [ "
                                        ++ String.join "\n    , " (List.map Tuple.first settings)
                                        ++ "\n    ]"
                                        ++ ("\n    [ Html.text \"" ++ content ++ "\" ]")
                                }
                        in
                        List.map toExampleCode examples
                }
            , examples
                |> List.map
                    (\( name, view, content ) ->
                        ( name, view attributes [ Html.text content ] )
                    )
                |> viewExamples
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    }


init : State
init =
    { control =
        ControlExtra.list
            |> CommonControls.css { moduleName = moduleName, use = Heading.css }
            |> ControlExtra.optionalListItem "style" controlStyle
    }


controlStyle : Control ( String, Heading.Attribute msg )
controlStyle =
    [ ( "Top", Heading.Top )
    , ( "Subhead", Heading.Subhead )
    , ( "Small", Heading.Small )
    ]
        |> List.map
            (\( name, val ) ->
                ( name
                , Control.value
                    ( "Heading.style Heading." ++ name
                    , Heading.style val
                    )
                )
            )
        |> Control.choice


type alias Settings =
    List ( String, Heading.Attribute Msg )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
