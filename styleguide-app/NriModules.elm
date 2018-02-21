module NriModules exposing (ModuleStates, Msg, init, nriThemedModules, styles, subscriptions, update)

import Css exposing (..)
import Examples.Text as TextExample
import Html exposing (Html, img)
import Html.Attributes exposing (..)
import ModuleExample exposing (Category(..), ModuleExample)
import Navigation
import Nri.Ui.Text.V1 as Text
import String.Extra


type alias ModuleStates =
    {}


init : ModuleStates
init =
    {}


type Msg
    = ShowItWorked String String
    | NoOp


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update msg moduleStates =
    case msg of
        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( moduleStates, Cmd.none )

        NoOp ->
            ( moduleStates, Cmd.none )


subscriptions : ModuleStates -> Sub Msg
subscriptions moduleStates =
    Sub.batch
        []


{-| A container with a visually-apparent size for demonstrating how style guide components
fill their parents.
-}
container : Int -> List (Html msg) -> Html msg
container width children =
    Html.div
        [ Html.Attributes.class "demo-container"
        , style [ ( "width", toString width ++ "px" ) ]
        ]
        children


nriThemedModules : ModuleStates -> List (ModuleExample Msg)
nriThemedModules model =
    [ TextExample.example
    ]


exampleMessages : (msg -> Msg) -> String -> ModuleExample.ModuleMessages msg Msg
exampleMessages exampleMessageWrapper exampleName =
    { noOp = NoOp
    , showItWorked = ShowItWorked exampleName
    , wrapper = exampleMessageWrapper
    }


route : Navigation.Location -> Maybe String
route location =
    location.hash
        |> String.dropLeft 1
        |> String.Extra.nonEmpty


styles : List Stylesheet
styles =
    List.concat
        [ -- NOTE: these will go away as the modules' styles are integrated with Nri.Css.Site.elm
          [ ModuleExample.styles
          ]
        , (Text.styles |> .css) ()
        ]
