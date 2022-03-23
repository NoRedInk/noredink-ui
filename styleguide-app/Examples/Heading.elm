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
import Nri.Ui.Heading.V2 as Heading
import ViewHelpers exposing (viewExamples)


{-| -}
example : Example State Msg
example =
    { name = "Heading"
    , version = 2
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
        \state ->
            let
                examples =
                    [ ( "h1", Heading.h1, "This is the main page heading." )
                    , ( "h2", Heading.h2, "This is a tagline" )
                    , ( "h3", Heading.h3, "This is a subHeading" )
                    , ( "h4", Heading.h4, "This is a smallHeading" )
                    , ( "h5", Heading.h5, "This is also a smallHeading" )
                    ]

                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
            in
            [ ControlView.view
                { update = UpdateControl
                , settings = state.control
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode ( name, _, content ) =
                                { sectionName = name
                                , code =
                                    "Heading."
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
            |> CommonControls.css { moduleName = "Heading", use = Heading.css }
            |> ControlExtra.optionalBoolListItem "error" ( "Heading.error", Heading.error )
            |> ControlExtra.optionalListItem "style" controlStyle
    }


controlStyle : Control ( String, Heading.Attribute msg )
controlStyle =
    [ ( "Top", Heading.Top )
    , ( "Tagline", Heading.Tagline )
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
