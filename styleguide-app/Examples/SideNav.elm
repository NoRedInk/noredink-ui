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
import EllieLink
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SideNav.V2 as SideNav


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport = []
    , preview = [ viewPreview ]
    , view = view
    }


moduleName : String
moduleName =
    "SideNav"


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


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        settings =
            Control.currentValue state.settings
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetControls
        , settings = state.settings
        , mainType = "RootHtml.Html msg"
        , extraImports = []
        , toExampleCode =
            \{ entries } ->
                [ { sectionName = "View"
                  , code =
                        String.join ""
                            [ moduleName ++ ".view"
                            , "\n\t{ isCurrentRoute = (==) \"" ++ settings.currentRoute ++ "\""
                            , "\n\t, routeToString = identity"
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
        , routeToString = identity
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
    { currentRoute : String
    , css : ( String, List Css.Style )
    , entries : List ( String, SideNav.Entry String Msg )
    }


{-| -}
init : State
init =
    { settings =
        Control.record Settings
            |> Control.field "currentRoute" (Control.string "#some-route")
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
            |> Control.field "entries" (Control.map List.singleton (controlEntryType "#some-route"))
    }


controlEntryType : String -> Control ( String, SideNav.Entry String Msg )
controlEntryType href =
    Control.choice
        [ ( "entry", controlEntry href )
        , ( "entryWithChildren", controlEntryWithChildren href )
        , ( "html", controlHtml )
        ]


controlEntry : String -> Control ( String, SideNav.Entry String Msg )
controlEntry href =
    Control.record
        (\title attributes ->
            ( "SideNav.entry \""
                ++ title
                ++ "\"\n\t\t[ "
                ++ String.join "\n\t\t, " (List.map Tuple.first attributes)
                ++ "\n\t\t]"
            , SideNav.entry title (List.map Tuple.second attributes)
            )
        )
        |> Control.field "title" (Control.string "Entry Category")
        |> Control.field "attributes" (controlEntryAttributes href)


controlEntryWithChildren : String -> Control ( String, SideNav.Entry String Msg )
controlEntryWithChildren href =
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
        |> Control.field "attributes" (controlEntryAttributes href)
        |> Control.field "children"
            (Control.lazy
                (\() ->
                    Control.map List.singleton (controlEntryType (href ++ "-child"))
                )
            )


controlHtml : Control ( String, SideNav.Entry String Msg )
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


controlEntryAttributes : String -> Control (List ( String, SideNav.Attribute String Msg ))
controlEntryAttributes href =
    ControlExtra.list
        |> ControlExtra.listItem "href"
            (Control.map (\v -> ( "SideNav.href \"" ++ v ++ "\"", SideNav.href v ))
                (Control.string href)
            )
        |> CommonControls.css { moduleName = "SideNav", use = SideNav.css }
        |> CommonControls.iconNotCheckedByDefault "SideNav" SideNav.icon
        |> ControlExtra.optionalBoolListItem "secondary" ( "SideNav.secondary", SideNav.secondary )
        |> ControlExtra.optionalListItem "premiumDisplay"
            (Control.map
                (\( displayStr, display ) ->
                    ( "SideNav.premiumDisplay " ++ displayStr
                    , SideNav.premiumDisplay display (ConsoleLog "Premium pennant clicked")
                    )
                )
                CommonControls.premiumDisplay
            )


{-| -}
type Msg
    = SetControls (Control Settings)
    | SkipToContent
    | ConsoleLog String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SkipToContent ->
            ( state, Cmd.none )

        ConsoleLog message ->
            ( Debug.log "SideNav" message |> always state, Cmd.none )
