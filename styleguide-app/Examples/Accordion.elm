module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (..)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Accordion.V2 as Accordion
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V4 as Text
import Task


{-| -}
example : Example State Msg
example =
    { name = "Accordion"
    , version = 2
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , categories = [ Layout ]
    , atomicDesignType = Molecule
    , keyboardSupport =
        [ { keys = [ Arrow KeyboardSupport.Up ]
          , result = "Moves the focus to the previous accordion header button (wraps focus to the last header button)"
          }
        , { keys = [ Arrow KeyboardSupport.Down ]
          , result = "Moves the focus to the next accordion header button (wraps focus to the first header button)"
          }
        ]
    }


{-| -}
view : State -> List (Html Msg)
view model =
    [ Heading.h3 [] [ Html.text "Accordion.view with default styles" ]
    , Accordion.view
        { entries =
            [ { id = 1, title = "Entry 1", content = "Content for the first accordion" }
            , { id = 2, title = "Entry 2", content = "Content for the second accordion" }
            , { id = 3, title = "Super long entry that is very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very long", content = "Content for the third accordion" }
            ]
                |> List.map
                    (\entry ->
                        { headerId = "accordion-entry__" ++ String.fromInt entry.id
                        , entry = entry
                        , isExpanded = Dict.get entry.id model |> Maybe.withDefault False
                        }
                    )
        , headerLevel = Accordion.H4
        , viewHeader = .title >> Html.text
        , viewContent = \{ content } -> Text.smallBody [ Html.text content ]
        , customStyles = Nothing
        , toggle = \entry toExpand -> Toggle entry.id toExpand
        , focus = Focus
        , caret = Accordion.DefaultCaret
        }
    , Heading.h3 [] [ Html.text "Accordion.view with custom styles from peer reviews" ]
    , Accordion.view
        { entries =
            [ { id = 4
              , title = "Firstname Lastname"
              , content =
                    Html.div
                        [ css [ Fonts.baseFont, fontSize (px 13) ]
                        ]
                        [ Html.text "has not started writing" ]
              }
            , { id = 5
              , title = "LongFirstnameAnd EvenLongerLastname"
              , content =
                    Html.div
                        [ css [ Fonts.baseFont, fontSize (px 13) ] ]
                        [ Html.text "has started writing" ]
              }
            ]
                |> List.map
                    (\entry ->
                        { headerId = "accordion-entry__" ++ String.fromInt entry.id
                        , entry = entry
                        , isExpanded = Dict.get entry.id model |> Maybe.withDefault False
                        }
                    )
        , headerLevel = Accordion.H4
        , viewHeader = .title >> Html.text
        , viewContent = .content
        , customStyles =
            Just
                (\_ ->
                    { entryStyles =
                        [ borderTop3 (px 1) solid Colors.gray75
                        , marginBottom zero
                        , width (px 284)
                        ]
                    , entryExpandedStyles = []
                    , entryClosedStyles = []
                    , headerStyles =
                        [ height (px 46)
                        , paddingLeft (px 8)
                        , paddingRight (px 8)
                        , Css.alignItems Css.center
                        ]
                    , headerExpandedStyles =
                        [ backgroundColor Colors.gray96
                        , borderRadius zero
                        ]
                    , headerClosedStyles = [ backgroundColor transparent ]
                    , contentStyles =
                        [ backgroundColor Colors.gray96
                        , paddingLeft (px 8)
                        , paddingRight (px 8)
                        , paddingBottom (px 8)
                        ]
                    }
                )
        , toggle = \entry toExpand -> Toggle entry.id toExpand
        , focus = Focus
        , caret = Accordion.DefaultCaret
        }
    ]


type Msg
    = Toggle Int Bool
    | Focus String
    | Focused (Result Dom.Error ())


{-| -}
init : State
init =
    Dict.fromList
        [ ( 1, False )
        , ( 2, False )
        , ( 3, False )
        , ( 4, False )
        , ( 5, False )
        ]


{-| -}
type alias State =
    Dict Int Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        Toggle id toExpanded ->
            ( Dict.insert id toExpanded model, Cmd.none )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )
