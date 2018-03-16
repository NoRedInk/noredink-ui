module NriModules exposing (ModuleStates, Msg, init, nriThemedModules, styles, subscriptions, update)

import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import Examples.Fonts
import Examples.Text
import Examples.Text.Writing
import Examples.TextArea as TextAreaExample
import Html exposing (Html, img)
import Html.Attributes exposing (..)
import ModuleExample exposing (Category(..), ModuleExample)
import Navigation
import Nri.Ui.AssetPath as AssetPath exposing (Asset(Asset))
import Nri.Ui.Text.V1 as Text
import Nri.Ui.TextArea.V1 as TextArea
import String.Extra


type alias ModuleStates =
    { textAreaExampleState : TextAreaExample.State
    }


init : ModuleStates
init =
    { textAreaExampleState = TextAreaExample.init
    }


type Msg
    = ShowItWorked String String
    | TextAreaExampleMsg TextAreaExample.Msg
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

        TextAreaExampleMsg msg ->
            let
                ( textAreaExampleState, cmd ) =
                    TextAreaExample.update msg moduleStates.textAreaExampleState
            in
            ( { moduleStates | textAreaExampleState = textAreaExampleState }
            , Cmd.map TextAreaExampleMsg cmd
            )

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
    [ Examples.Text.example
    , Examples.Text.Writing.example
    , Examples.Fonts.example
    , TextAreaExample.example TextAreaExampleMsg model.textAreaExampleState
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
        , (TextArea.styles |> .css) assets
        ]


type alias Assets =
    { iconCalendar_svg : Asset
    , icons_searchGray_svg : Asset
    , icons_xBlue_svg : Asset
    }


assets : Assets
assets =
    { iconCalendar_svg = Asset ""
    , icons_searchGray_svg = Asset ""
    , icons_xBlue_svg = Asset ""
    }
