module Examples.QuestionBox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled as Html
import Markdown
import Nri.Ui.QuestionBox.V1 as QuestionBox
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "QuestionBox"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Interactions ]
    , keyboardSupport = []
    , preview = []
    , view = view
    }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        attributes =
            Control.currentValue state.attributes
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControls
        , settings = state.attributes
        , mainType = Nothing
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = Code.fromModule moduleName "view"
                  , code =
                        Code.fromModule moduleName "view "
                            ++ Code.list (List.map Tuple.first attributes)
                  }
                ]
        }
    , viewExamplesTable
    ]


viewExamplesTable : Html Msg
viewExamplesTable =
    Table.view
        [ Table.string
            { header = "Pattern"
            , value = .pattern
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = Html.text "About"
            , view = .description >> Markdown.toHtml Nothing >> List.map Html.fromUnstyled >> Html.span []
            , width = Css.px 50
            , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        , Table.custom
            { header = Html.text "Example"
            , view = .example
            , width = Css.pct 75
            , cellStyles = always [ Css.textAlign Css.center ]
            , sort = Nothing
            }
        ]
        [ { pattern = "QuestionBox.viewAnchored"
          , description = "???"
          , example =
                QuestionBox.viewAnchored
                    { markdown = exampleNotQuiteMarkdown
                    , actions = [ { label = "Try again", onClick = NoOp } ]
                    }
                    "fake-id-string"
                    (QuestionBox.hackyHardcodedOffset 123)
                    [ Html.text "QuestionBox content"
                    ]
          }
        ]


exampleNotQuiteMarkdown : String
exampleNotQuiteMarkdown =
    """
Not quite. **Plural** means **more than one person.**

This subject is **only one person.**
    """


{-| -}
init : State
init =
    { attributes = initAttributes
    }


{-| -}
type alias State =
    { attributes : Control (List ( String, () ))
    }


initAttributes : Control (List ( String, () ))
initAttributes =
    ControlExtra.list


{-| -}
type Msg
    = UpdateControls (Control (List ( String, () )))
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls configuration ->
            ( { state | attributes = configuration }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )
