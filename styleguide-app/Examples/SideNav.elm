module Examples.SideNav exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.SideNav.V2 as SideNav


{-| -}
example : Example State Msg
example =
    { name = "SideNav"
    , version = 2
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport = []
    , preview = [ viewPreview ]
    , view = view
    }


viewPreview : Html msg
viewPreview =
    div
        [ css
            [ Css.height (Css.px 80)
            , Css.backgroundColor Colors.white
            , Css.padding (Css.px 8)
            , Css.displayFlex
            ]
        ]
        [ div
            [ css
                [ Css.flexGrow (Css.int 1)
                , Css.backgroundColor Colors.gray96
                , Css.borderRadius (Css.px 2)
                ]
            ]
            [ div
                [ css
                    [ Css.height (Css.px 8)
                    , Css.backgroundColor Colors.glacier
                    , Css.borderRadius (Css.px 2)
                    , Css.margin2 (Css.px 8) (Css.px 4)
                    ]
                ]
                []
            ]
        , div [ css [ Css.flexGrow (Css.int 2) ] ] []
        ]


view : State -> List (Html Msg)
view state =
    let
        entriesSettings =
            Control.currentValue state.settings
                |> List.map Tuple.second
    in
    [ ControlView.view
        { update = SetControls
        , settings = state.settings
        , toExampleCode =
            \entries ->
                [ { sectionName = "View"
                  , code =
                        String.join ""
                            [ "SideNav.view"
                            , "\n\t{ isCurrentRoute = isCurrentRoute"
                            , "\n\t, routeToString = routeToString"
                            , "\n\t, onSkipNav = SkipToContent"
                            , "\n\t, css = []"
                            , "\n\t}"
                            , "\n\t[ "
                            , String.join "\n\t" (List.map Tuple.first entries)
                            , "\n\t]"
                            ]
                  }
                ]
        }
    , SideNav.view
        { isCurrentRoute =
            -- TODO: show current route
            \_ -> False
        , routeToString = \_ -> ""
        , onSkipNav = SkipToContent
        , css = []
        }
        entriesSettings
    ]


{-| -}
type alias State =
    { settings : Settings
    }


type alias Settings =
    Control (List ( String, SideNav.Entry () Msg ))


{-| -}
init : State
init =
    { settings =
        ControlExtra.list
            |> ControlExtra.listItem "entry" controlEntry
    }


controlEntry : Control ( String, SideNav.Entry () Msg )
controlEntry =
    Control.choice
        [ ( "entry"
          , Control.record
                (\title attributes ->
                    ( "SideNav.entry "
                        ++ title
                        ++ " [\n\t"
                        ++ String.join "\n\t," (List.map Tuple.first attributes)
                        ++ "\n\t]"
                    , SideNav.entry title (List.map Tuple.second attributes)
                    )
                )
                |> Control.field "title" (Control.string "Entry Category")
                |> Control.field "attributes" controlEntryAttributes
          )
        , ( "entryWithChildren"
          , Control.record
                (\title attributes children ->
                    ( "SideNav.entryWithChildren "
                        ++ title
                        ++ " [\n\t"
                        ++ String.join "\n\t," (List.map Tuple.first attributes)
                        ++ "\n\t]"
                        ++ " [\n\t"
                        ++ String.join "\n\t," (List.map Tuple.first children)
                        ++ "\n\t]"
                    , SideNav.entryWithChildren title
                        (List.map Tuple.second attributes)
                        (List.map Tuple.second children)
                    )
                )
                |> Control.field "title" (Control.string "Entry Category")
                |> Control.field "attributes" controlEntryAttributes
                |> -- TODO: support sub-categories
                   Control.field "children" (Control.value [])
          )
        , ( "html"
          , Control.map
                (\html ->
                    ( "SideNav.html "
                        ++ " [\n\t"
                        ++ String.join "\n\t," (List.map Tuple.first html)
                        ++ "\n\t]"
                    , SideNav.html (List.map Tuple.second html)
                    )
                )
                -- TODO: support HTML examples
                (Control.value [])
          )
        ]


controlEntryAttributes : Control (List ( String, SideNav.Attribute () Msg ))
controlEntryAttributes =
    ControlExtra.list


{-| -}
type Msg
    = SetControls Settings
    | SkipToContent


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SkipToContent ->
            ( state, Cmd.none )
