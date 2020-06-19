module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Accordion.V1 as Accordion
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V4 as Text


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Accordion.V1"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , categories = [ Layout ]
    , atomicDesignType = AtomicDesignType.Molecule
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
                        ( entry, Dict.get entry.id model |> Maybe.withDefault False )
                    )
        , viewHeader = .title >> Html.text
        , viewContent = \{ content } -> Text.smallBody [ Html.text content ]
        , customStyles = Nothing
        , toggle = \entry toExpand -> Toggle entry.id toExpand
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
                        ( entry, Dict.get entry.id model |> Maybe.withDefault False )
                    )
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
        , caret = Accordion.DefaultCaret
        }
    ]


type Msg
    = Toggle Int Bool


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
update (Toggle id toExpanded) model =
    ( Dict.insert id toExpanded model, Cmd.none )
