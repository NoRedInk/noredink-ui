module Examples.COMPONENT_NAME exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.COMPONENT_NAME.V1 as COMPONENT_NAME
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table


moduleName : String
moduleName =
    "COMPONENT_NAME"


version : Int
version =
    1


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ text "🚧 Preview under construction 🚧"
        ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                currentValue =
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
                    \{ label, attributes } ->
                        -- TODO: are there different view APIs that you need to
                        -- model from this component?
                        -- you can add additional examples to this list.
                        --
                        -- Please use description names for each
                        -- example section.
                        [ { sectionName = "Example"
                          , code =
                                -- TODO: be sure that the Ellie will compile
                                -- once the new component is released.
                                --
                                -- You can add extra imports & change the
                                -- type of the `main` if you need to in the
                                -- settings passed to `ControlView.view`
                                Code.fromModule moduleName "view"
                                    ++ Code.listMultiline
                                        (List.map Tuple.first attributes)
                                        1
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , -- TODO: depending on your API, this example may not compile yet.
              COMPONENT_NAME.view (List.map Tuple.second currentValue.attributes)
            , -- TODO: The section that follows is optional.
              -- Often, it's nice to include a table with static examples
              -- of various states in order to prevent visual regressions.
              -- if your component is stateful, you will likely want
              -- to include a table like the one shown below.
              Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Table.view []
                [ Table.string
                    { header = "State"
                    , value = .state
                    , width = Css.pct 30
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Type A"
                    , view = .typeA
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Type B"
                    , view = .typeB
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                ]
                [ { state = "State 1"
                  , typeA = text "[TODO: add Type A view]"
                  , typeB = text "[TODO: add Type A view]"
                  }
                ]
            ]
    , categories =
        [-- TODO: Categorize your component. This is important for discoverability, so please don't skip this!
        ]
    , keyboardSupport =
        [-- TODO: Use this section to describe any keyboard behavior that your
         -- component has, if any.
        ]
    }


{-| -}
type alias State =
    { attributes : List ( String, COMPONENT_NAME.Attribute Msg )
    }


init : State
init =
    { attributes = initAttributes
    }


initAttributes : Control (List ( String, COMPONENT_NAME.Attribute msg ))
initAttributes =
    ControlExtra.list


{-| -}
type Msg
    = UpdateSettings (Control (List ( String, COMPONENT_NAME.Attribute Msg )))


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings settings ->
            ( { state | settings = settings }
            , Cmd.none
            )
