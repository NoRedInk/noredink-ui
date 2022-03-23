module Examples.Heading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Colors.V1 as Colors
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
            [ ControlView.view
                { update = UpdateControl
                , settings = state.control
                , toExampleCode =
                    \settings ->
                        []
                }
            , viewExamples
                [ ( "h1", Heading.h1 [] [ Html.text "This is the main page heading." ] )
                , ( "h2", Heading.h2 [] [ Html.text "This is a tagline" ] )
                , ( "h3", Heading.h3 [] [ Html.text "This is a subHeading" ] )
                , ( "h4", Heading.h4 [] [ Html.text "This is a smallHeading" ] )
                ]
            , Heading.h2 [ Heading.style Heading.Top ]
                [ Html.text "Heading.h2 [ Heading.style Heading.Top ]" ]
            , Heading.h2 [ Heading.css [ Css.color Colors.highlightPurpleDark ] ]
                [ Html.text "Heading.h2 [ Heading.css [ Css.color Colors.highlightPurpleDark ] ]" ]
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
    }


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
