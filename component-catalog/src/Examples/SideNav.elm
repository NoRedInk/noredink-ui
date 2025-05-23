module Examples.SideNav exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SideNav.V5 as SideNav
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V2 as UiIcon


version : Int
version =
    5


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout, Navigation ]
    , keyboardSupport = []
    , preview = [ viewPreview ]
    , about =
        [ Text.smallBody
            [ Text.html
                [ text "Ensure you manage the user's focus properly when using SideNav in a SPA by reviewing the "
                , ClickableText.link "Assistive technology notification design & development guide"
                    [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
                    , ClickableText.appearsInline
                    ]
                , text "."
                ]
            ]
        ]
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
        , mainType = Just "RootHtml.Html Msg"
        , extraCode =
            [ Code.newlines
            , Code.unionType "Msg" [ "SkipToContent" ]
            ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \{ navAttributes, entries } ->
                [ { sectionName = "View"
                  , code =
                        Code.fromModule moduleName "view"
                            ++ Code.recordMultiline
                                [ ( "isCurrentRoute", "(==) \"" ++ settings.currentRoute ++ "\"" )
                                , ( "routeToString", "identity" )
                                , ( "onSkipNav", "SkipToContent" )
                                ]
                                1
                            ++ Code.listMultiline (List.map Tuple.first navAttributes) 1
                            ++ Code.listMultiline (List.map Tuple.first entries) 1
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Interactive example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , SideNav.view
        { isCurrentRoute = (==) settings.currentRoute
        , routeToString = identity
        , onSkipNav = SkipToContent
        }
        (List.map Tuple.second settings.navAttributes)
        (List.map Tuple.second settings.entries)
    , Heading.h2
        [ Heading.plaintext "Basic example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , SideNav.view
        { isCurrentRoute = \route -> route == "nested-example__grandchild-2"
        , routeToString = identity
        , onSkipNav = SkipToContent
        }
        [ SideNav.navLabel "Nested example"
        , SideNav.navId "nested-example-sidenav"
        ]
        [ SideNav.entryWithChildren "Entry with Children"
            []
            [ SideNav.entry "Child 1"
                [ SideNav.href "nested-example__child-1"
                ]
            , SideNav.entryWithChildren "Child 2"
                []
                [ SideNav.entry "Grandchild 1"
                    [ SideNav.href "nested-example__grandchild-1"
                    ]
                , SideNav.entry "Grandchild 2"
                    [ SideNav.href "nested-example__grandchild-2"
                    ]
                ]
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Complex example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , SideNav.view
        { isCurrentRoute = \route -> route == "complex-example__child-2"
        , routeToString = identity
        , onSkipNav = SkipToContent
        }
        [ SideNav.navLabel "Complex example"
        , SideNav.navId "complex-example-sidenav"
        ]
        [ SideNav.html
            [ Text.smallBody
                [ Text.plaintext "(Arbitrary HTML content)"
                , Text.css [ Css.paddingBottom (Css.px 10) ]
                ]
            ]
        , SideNav.entry "Entry" [ SideNav.icon UiIcon.person ]
        , SideNav.entryWithChildren "Entry with Children"
            [ SideNav.icon UiIcon.baldBulb ]
            [ SideNav.entry "Child 1"
                [ SideNav.href "complex-example__child-1"
                ]
            , SideNav.entry "Child 2"
                [ SideNav.href "complex-example__child-2"
                ]
            ]
        , SideNav.compactGroup "Compact Group"
            []
            [ SideNav.entry "Child 1"
                [ SideNav.href "compact-group__child-1"
                ]
            , SideNav.entry "Child 2"
                [ SideNav.href "compact-group__child-2"
                ]
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Compact Groups example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , SideNav.view
        { isCurrentRoute = \route -> route == "complex-example__child-2"
        , routeToString = identity
        , onSkipNav = SkipToContent
        }
        [ SideNav.navLabel "Compact groups example"
        , SideNav.navId "compact-groups-example-sidenav"
        ]
        [ SideNav.compactGroup "Support"
            []
            [ SideNav.entry "Announcements" []
            , SideNav.entry "Schools" []
            , SideNav.entry "Users" []
            , SideNav.entry "Assignments" []
            , SideNav.entry "Blueprints" []
            , SideNav.entry "Compliance" []
            , SideNav.entry "Back to School" []
            , SideNav.entry "Essay recovery" []
            ]
        , SideNav.compactGroup "Curriculum"
            []
            [ SideNav.entry "Legacy Content Creation" []
            , SideNav.entry "Guided Tutorial Editor" []
            , SideNav.entry "Questions" []
            ]
        , SideNav.compactGroup "Engineering"
            []
            [ SideNav.entry "Configurable" []
            , SideNav.entry "Quiz Engine Feature Flags" []
            , SideNav.html [ Text.smallBody [ Text.plaintext "Demo Accounts disabled on staging" ] ]
            , SideNav.entry "Sham Assignment" []
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Premium Display"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , SideNav.view
        { isCurrentRoute = \route -> route == "/subchildren-unlocked"
        , routeToString = identity
        , onSkipNav = SkipToContent
        }
        [ SideNav.navLabel "Premium Display"
        , SideNav.navId "premium-display-sidenav"
        ]
        (premiumDisplayEntries "/"
            ++ [ SideNav.entryWithChildren "As subchildren"
                    []
                    (premiumDisplayEntries "/subchildren")
               , SideNav.compactGroup "In a compact group"
                    []
                    (premiumDisplayEntries "/compact")
               ]
        )
    ]


premiumDisplayEntries : String -> List (SideNav.Entry String Msg)
premiumDisplayEntries hrefPrefix =
    [ SideNav.entry "Free"
        [ SideNav.premiumDisplay
            PremiumDisplay.Free
            (ConsoleLog "Clicked Free SideNav Entry")
        , SideNav.href (hrefPrefix ++ "-free")
        ]
    , SideNav.entry "Unlocked"
        [ SideNav.premiumDisplay
            PremiumDisplay.PremiumUnlocked
            (ConsoleLog "Clicked PremiumUnlocked SideNav Entry")
        , SideNav.href (hrefPrefix ++ "-unlocked")
        ]
    , SideNav.entry "Locked"
        [ SideNav.premiumDisplay
            PremiumDisplay.PremiumLocked
            (ConsoleLog "Clicked PremiumLocked SideNav Entry")
        , SideNav.href (hrefPrefix ++ "-locked")
        ]
    , SideNav.entry "Vouchered"
        [ SideNav.premiumDisplay
            PremiumDisplay.PremiumVouchered
            (ConsoleLog "Clicked PremiumVouchered SideNav Entry")
        , SideNav.href (hrefPrefix ++ "-vouchered")
        ]
    ]


{-| -}
type alias State =
    { settings : Control Settings
    }


type alias Settings =
    { currentRoute : String
    , navAttributes : List ( String, SideNav.NavAttribute Msg )
    , entries : List ( String, SideNav.Entry String Msg )
    }


{-| -}
init : State
init =
    { settings =
        Control.record Settings
            |> Control.field "currentRoute" (Control.string "#some-route")
            |> Control.field "navAttributes" controlNavAttributes
            |> Control.field "Level 1"
                (Control.map List.singleton (controlEntryType 2 "#some-route")
                    |> Control.revealed "entry type"
                )
    }


controlNavAttributes : Control (List ( String, SideNav.NavAttribute Msg ))
controlNavAttributes =
    Control.list
        |> ControlExtra.optionalListItemDefaultChecked "navLabel"
            (Control.map
                (\val ->
                    ( "SideNav.navLabel \"" ++ val ++ "\""
                    , SideNav.navLabel val
                    )
                )
                (Control.string "Example")
            )
        |> ControlExtra.optionalListItemDefaultChecked "navNotMobileCss"
            (Control.choice
                [ ( "maxWidth"
                  , Control.value
                        ( "SideNav.navNotMobileCss [ Css.maxWidth (Css.px 300) ]"
                        , SideNav.navNotMobileCss [ Css.maxWidth (Css.px 300) ]
                        )
                  )
                , ( "purple border"
                  , Control.value
                        ( "SideNav.navNotMobileCss [ Css.border3 (Css.px 3) Css.dotted Colors.purple ]"
                        , SideNav.navNotMobileCss [ Css.border3 (Css.px 3) Css.dotted Colors.purple ]
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "navMobileCss"
            (Control.value
                ( "SideNav.navMobileCss [ Css.width (Css.pct 100) ]"
                , SideNav.navMobileCss [ Css.width (Css.pct 100) ]
                )
            )


controlEntryType : Int -> String -> Control ( String, SideNav.Entry String Msg )
controlEntryType level href =
    Control.choice
        [ ( "entry", controlEntry level href )
        , ( "entryWithChildren", controlEntryWithChildren level href )
        , ( "html", controlHtml level )
        , ( "compactGroup", controlCompactGroup level href )
        ]


controlEntry : Int -> String -> Control ( String, SideNav.Entry String Msg )
controlEntry level href =
    Control.record
        (\title attributes ->
            ( Code.fromModule moduleName "entry "
                ++ Code.string title
                ++ Code.listMultiline (List.map Tuple.first attributes) level
            , SideNav.entry title (List.map Tuple.second attributes)
            )
        )
        |> Control.field "title" (Control.string "Entry Category")
        |> Control.field "" (controlEntryAttributes href)


controlEntryWithChildren : Int -> String -> Control ( String, SideNav.Entry String Msg )
controlEntryWithChildren level href =
    Control.record
        (\title attributes children ->
            ( Code.fromModule moduleName "entryWithChildren "
                ++ Code.string title
                ++ Code.listMultiline (List.map Tuple.first attributes) level
                ++ Code.listMultiline (List.map Tuple.first children) level
            , SideNav.entryWithChildren title
                (List.map Tuple.second attributes)
                (List.map Tuple.second children)
            )
        )
        |> Control.field "title" (Control.string "Entry Category")
        |> Control.field "" (controlEntryAttributes href)
        |> Control.field ("Level " ++ String.fromInt level)
            (Control.lazy
                (\() ->
                    Control.map List.singleton (controlEntryType (level + 1) (href ++ "-child"))
                        |> Control.revealed "entry type"
                )
            )


controlHtml : Int -> Control ( String, SideNav.Entry String Msg )
controlHtml level =
    Control.map
        (\html ->
            ( Code.fromModule moduleName "html " ++ Code.list (List.map Tuple.first html)
            , SideNav.html (List.map Tuple.second html)
            )
        )
        -- TODO: support HTML examples
        (Control.value [])


controlCompactGroup : Int -> String -> Control ( String, SideNav.Entry String Msg )
controlCompactGroup level href =
    Control.record
        (\title attributes children ->
            ( Code.fromModule moduleName "compactGroup "
                ++ Code.string title
                ++ Code.listMultiline (List.map Tuple.first attributes) level
                ++ Code.listMultiline (List.map Tuple.first children) level
            , SideNav.compactGroup title
                (List.map Tuple.second attributes)
                (List.map Tuple.second children)
            )
        )
        |> Control.field "title" (Control.string "Entry Category")
        |> Control.field "attributes" controlGroupAttributes
        |> Control.field "children"
            (Control.lazy
                (\() ->
                    Control.map List.singleton (controlEntryType (level + 1) (href ++ "-child"))
                )
            )


controlGroupOrEntryAttributes :
    List
        (Control (List ( String, SideNav.Attribute entryOrGroup msg ))
         -> Control (List ( String, SideNav.Attribute entryOrGroup msg ))
        )
controlGroupOrEntryAttributes =
    [ CommonControls.css { moduleName = moduleName, use = SideNav.css }
    , CommonControls.iconNotCheckedByDefault moduleName SideNav.icon
    , CommonControls.rightIcon moduleName SideNav.rightIcon
    ]


controlGroupAttributes : Control (List ( String, SideNav.GroupAttribute ))
controlGroupAttributes =
    List.foldl (\f acc -> f acc) Control.list controlGroupOrEntryAttributes


controlEntryAttributes : String -> Control (List ( String, SideNav.EntryAttribute String Msg ))
controlEntryAttributes href =
    ([ ControlExtra.listItem "href"
        (Control.map (\v -> ( Code.fromModule moduleName "href \"" ++ v ++ "\"", SideNav.href v ))
            (Control.string href)
        )
     , ControlExtra.optionalListItem "premiumDisplay"
        (Control.map
            (\( displayStr, display ) ->
                ( Code.fromModule moduleName "premiumDisplay " ++ displayStr
                , SideNav.premiumDisplay display (ConsoleLog "Premium pennant clicked")
                )
            )
            CommonControls.premiumDisplay
        )
     ]
        ++ controlGroupOrEntryAttributes
    )
        |> List.foldl (\f acc -> f acc) Control.list


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
            ( Debug.log moduleName message |> always state, Cmd.none )
