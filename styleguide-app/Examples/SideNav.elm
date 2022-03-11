module Examples.SideNav exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import CommonControls
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
        settings =
            Control.currentValue state.settings
    in
    [ ControlView.view
        { update = SetControls
        , settings = state.settings
        , toExampleCode =
            \{ entries } ->
                [ { sectionName = "View"
                  , code =
                        String.join ""
                            [ "SideNav.view"
                            , "\n\t{ isCurrentRoute = (==) "
                            , String.fromInt settings.currentRoute
                            , "\n\t, routeToString = String.fromInt"
                            , "\n\t, onSkipNav = SkipToContent"
                            , "\n\t, css = " ++ Tuple.first settings.css
                            , "\n\t}"
                            , "\n\t[ "
                            , String.join "\n\t" (List.map Tuple.first entries)
                            , "\n\t]"
                            ]
                  }
                ]
        }
    , SideNav.view
        { isCurrentRoute = (==) settings.currentRoute
        , routeToString = String.fromInt
        , onSkipNav = SkipToContent
        , css = Tuple.second settings.css
        }
        (List.map Tuple.second settings.entries)
    ]


{-| -}
type alias State =
    { settings : Control Settings
    }


type alias Settings =
    { currentRoute : Int
    , css : ( String, List Css.Style )
    , entries : List ( String, SideNav.Entry Int Msg )
    }


{-| -}
init : State
init =
    { settings =
        Control.record Settings
            |> Control.field "currentRoute" (ControlExtra.int 1)
            |> Control.field "css"
                (Control.maybe True
                    (Control.choice
                        [ ( "maxWidth"
                          , Control.value
                                ( "[ Css.maxWidth (Css.px 300) ]"
                                , [ Css.maxWidth (Css.px 300) ]
                                )
                          )
                        , ( "purple border"
                          , Control.value
                                ( "[ Css.border3 (Css.px 3) Css.dotted Colors.purple ]"
                                , [ Css.border3 (Css.px 3) Css.dotted Colors.purple ]
                                )
                          )
                        ]
                    )
                    |> Control.map (Maybe.withDefault ( "[]", [] ))
                )
            |> Control.field "entries" (Control.map List.singleton controlEntryType)
    }


controlEntryType : Control ( String, SideNav.Entry Int Msg )
controlEntryType =
    Control.choice
        [ ( "entry", controlEntry )
        , ( "entryWithChildren", controlEntryWithChildren )
        , ( "html", controlHtml )
        ]


controlEntry : Control ( String, SideNav.Entry Int Msg )
controlEntry =
    Control.record
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


controlEntryWithChildren : Control ( String, SideNav.Entry Int Msg )
controlEntryWithChildren =
    Control.record
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


controlHtml : Control ( String, SideNav.Entry Int Msg )
controlHtml =
    Control.map
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


controlEntryAttributes : Control (List ( String, SideNav.Attribute Int Msg ))
controlEntryAttributes =
    ControlExtra.list
        |> ControlExtra.listItem "href"
            (Control.map
                (\int ->
                    ( "SideNav.href " ++ String.fromInt int
                    , SideNav.href int
                    )
                )
                (ControlExtra.int 1)
            )
        |> CommonControls.css { moduleName = "SideNav", use = SideNav.css }
        |> CommonControls.iconNotCheckedByDefault "SideNav" SideNav.icon


{-| -}
type Msg
    = SetControls (Control Settings)
    | SkipToContent


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SkipToContent ->
            ( state, Cmd.none )
