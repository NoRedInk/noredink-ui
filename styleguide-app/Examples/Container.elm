module Examples.Container exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading


moduleName : String
moduleName =
    "Container"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Container.view []
        , Container.view
            [ Container.disabled
            , Container.css [ Css.marginTop (Css.px 8) ]
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
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
                            stringAttributes =
                                List.map Tuple.first settings
                        in
                        [ { sectionName = "Default Container"
                          , code = viewExampleCode stringAttributes
                          }
                        , { sectionName = "Gray Container"
                          , code = viewExampleCode ("Container.gray" :: stringAttributes)
                          }
                        , { sectionName = "Pillow Container"
                          , code = viewExampleCode ("Container.pillow" :: stringAttributes)
                          }
                        , { sectionName = "Buttony Container"
                          , code = viewExampleCode ("Container.buttony" :: stringAttributes)
                          }
                        , { sectionName = "Disabled Container"
                          , code = viewExampleCode ("Container.disabled" :: stringAttributes)
                          }
                        ]
                }
            , viewExample
                { name = "Default Container"
                , description = "Your go-to container."
                }
                (Container.default :: attributes)
            , viewExample
                { name = "Gray Container"
                , description = "A container that doesn’t draw too much attention to itself."
                }
                (Container.gray :: attributes)
            , viewExample
                { name = "Pillow Container"
                , description = "When you want something big and soft."
                }
                (Container.pillow :: attributes)
            , viewExample
                { name = "Buttony Container"
                , description = "Used for clickable button card things."
                }
                (Container.buttony :: attributes)
            , viewExample
                { name = "Disabled Container"
                , description = "Used to indicate content is locked/inaccessible"
                }
                (Container.disabled :: attributes)
            ]
    }


viewExample : { name : String, description : String } -> List (Container.Attribute msg) -> Html msg
viewExample { name, description } attributes =
    Html.section
        [ css
            [ Css.marginTop (Css.px 20)
            ]
        ]
        [ Heading.h3 [] [ Html.text name ]
        , Html.text description
        , Container.view attributes
        ]


viewExampleCode : List String -> String
viewExampleCode attributes =
    moduleName
        ++ ".view\n    [ "
        ++ String.join "\n    , " attributes
        ++ "\n    ]"


{-| -}
type alias State =
    { control : Control (List ( String, Container.Attribute Msg ))
    }


{-| -}
init : State
init =
    { control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "paddingPx" controlPaddingPx
            |> CommonControls.css { moduleName = moduleName, use = Container.css }
            |> CommonControls.mobileCss { moduleName = moduleName, use = Container.mobileCss }
            |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Container.quizEngineMobileCss }
            |> CommonControls.notMobileCss { moduleName = moduleName, use = Container.notMobileCss }
            |> ControlExtra.listItem "content" controlContent
    }


controlPaddingPx : Control ( String, Container.Attribute msg )
controlPaddingPx =
    Control.map
        (\val ->
            ( "Container.paddingPx " ++ String.fromFloat val
            , Container.paddingPx val
            )
        )
        (ControlExtra.float 20)


controlContent : Control ( String, Container.Attribute msg )
controlContent =
    CommonControls.content
        { moduleName = "Container"
        , plaintext = Container.plaintext
        , markdown = Just Container.markdown
        , html = Container.html
        , httpError = Nothing
        }


{-| -}
type Msg
    = UpdateControl (Control (List ( String, Container.Attribute Msg )))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )
