module Examples.Block exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Markdown
import Nri.Ui.Block.V1 as Block
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "Block"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Interactions ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        -- TODO: add a useful preview!
        []
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.list (List.map Tuple.first attributes)
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Interactive example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Block.view (List.map Tuple.second attributes)
            , Heading.h2
                [ Heading.plaintext "Non-interactive examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Table.view
                [ Table.string
                    { header = "Type"
                    , value = .name
                    , width = Css.pct 15
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontWeight Css.bold ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "About"
                    , view = .description >> Markdown.toHtml Nothing >> List.map fromUnstyled >> span []
                    , width = Css.px 125
                    , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = .example >> p []
                    , width = Css.px 200
                    , cellStyles = always [ Css.textAlign Css.center ]
                    , sort = Nothing
                    }
                ]
                [ { name = Code.fromModule moduleName "emphasize"
                  , usage = ""
                  , description = "Uses the Highlighter component to mark content as emphasized"
                  , example =
                        [ Block.view [ Block.plaintext "The Crossover", Block.emphasize ]
                        , Block.view [ Block.plaintext " is Thor’s favorite book." ]
                        ]
                  }
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = initControl
    }


type alias Settings =
    List ( String, Block.Attribute Msg )


initControl : Control Settings
initControl =
    ControlExtra.list
        |> ControlExtra.listItem "plaintext"
            (Control.string "Bananas"
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext \"" ++ str ++ "\""
                        , Block.plaintext str
                        )
                    )
            )
        |> ControlExtra.optionalBoolListItem "emphasize" ( Code.fromModule moduleName "emphasize", Block.emphasize )


{-| -}
type Msg
    = UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings newControl ->
            ( { state | settings = newControl }
            , Cmd.none
            )
