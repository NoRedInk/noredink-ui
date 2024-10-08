module App exposing (Effect(..), Model, Msg(..), init, perform, subscriptions, update, view)

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key)
import Category exposing (Category)
import Css exposing (..)
import Css.Global
import Css.Media exposing (withMedia)
import Dict exposing (Dict)
import Example exposing (Example)
import Examples
import Html.Styled.Attributes exposing (..)
import Http
import InputMethod exposing (InputMethod)
import Json.Decode as Decode
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Header.V1 as Header
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Page.V3 as Page
import Nri.Ui.SideNav.V5 as SideNav
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Sprite.V1 as Sprite
import Nri.Ui.Tabs.V9 as Tabs
import Routes exposing (Route)
import Sort.Set as Set
import Task
import Url exposing (Url)
import UsageExample exposing (UsageExample)
import UsageExamples


type alias Model key =
    { -- Global UI
      route : Route
    , previousRoute : Maybe Route
    , moduleStates : Dict String ( Examples.State, Cmd Examples.Msg )
    , usageExampleStates : Dict String UsageExamples.State
    , isSideNavOpen : Bool
    , openTooltip : Maybe TooltipId
    , selectedContent : Content
    , navigationKey : key
    , elliePackageDependencies : Result Http.Error (Dict String String)
    , inputMethod : InputMethod
    }


init : () -> Url -> key -> ( Model key, Effect )
init () url key =
    ( { route = Routes.fromLocation url
      , previousRoute = Nothing
      , moduleStates = Dict.map (\_ example -> example.init) examplesDict
      , usageExampleStates = Dict.map (\_ example -> example.init) usageExamplesDict
      , isSideNavOpen = False
      , openTooltip = Nothing
      , selectedContent = ComponentExamples
      , navigationKey = key
      , elliePackageDependencies = Ok Dict.empty
      , inputMethod = InputMethod.init
      }
    , Cmd.batch
        [ loadPackage
        , loadApplicationDependencies
        ]
        |> Command
    )


type TooltipId
    = SideNavOpenCloseTooltip


type Msg
    = UpdateModuleStates String Examples.Msg
    | UpdateUsageExamples String UsageExamples.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | ChangeRoute Route
    | SkipToMainContent
    | ToggleSideNav Bool
    | ToggleTooltip TooltipId Bool
    | SelectContent { select : Content, focus : Maybe String }
    | LoadedPackages (Result Http.Error (Dict String String))
    | Focused (Result Browser.Dom.Error ())
    | NewInputMethod InputMethod
    | SwallowEvent


examplesDict : Dict String (Example Examples.State Examples.Msg)
examplesDict =
    Dict.fromList
        (List.map
            (\example -> ( Example.routeName example, example ))
            Examples.all
        )


findExample : Model k -> String -> Maybe ( Example Examples.State Examples.Msg, Examples.State, Cmd Examples.Msg )
findExample model key =
    Dict.get key model.moduleStates
        |> Maybe.andThen
            (\( state, initCmd ) ->
                Dict.get key examplesDict
                    |> Maybe.map (\example -> ( example, state, initCmd ))
            )


usageExamplesDict : Dict String (UsageExample UsageExamples.State UsageExamples.Msg)
usageExamplesDict =
    Dict.fromList
        (List.map (\example -> ( UsageExample.routeName example, example ))
            UsageExamples.all
        )


findUsageExample : Model k -> String -> Maybe ( UsageExample UsageExamples.State UsageExamples.Msg, UsageExamples.State )
findUsageExample model key =
    Dict.get key model.usageExampleStates
        |> Maybe.andThen
            (\state ->
                Dict.get key usageExamplesDict
                    |> Maybe.map (\example -> ( example, state ))
            )


update : Msg -> Model key -> ( Model key, Effect )
update action model =
    case action of
        UpdateModuleStates key exampleMsg ->
            case findExample model key of
                Just ( example, exampleState, initCmd ) ->
                    example.update exampleMsg exampleState
                        |> Tuple.mapFirst
                            (\newState ->
                                { model
                                    | moduleStates = Dict.insert key ( newState, initCmd ) model.moduleStates
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map (UpdateModuleStates key) >> Command)

                Nothing ->
                    ( model, None )

        UpdateUsageExamples key exampleMsg ->
            case findUsageExample model key of
                Just ( usageExample, usageExampleState ) ->
                    usageExample.update exampleMsg usageExampleState
                        |> Tuple.mapFirst
                            (\newState ->
                                { model
                                    | usageExampleStates = Dict.insert key newState model.usageExampleStates
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map (UpdateUsageExamples key) >> Command)

                Nothing ->
                    ( model, None )

        OnUrlRequest request ->
            case request of
                Internal loc ->
                    ( model, GoToUrl loc )

                External loc ->
                    ( model, Load loc )

        OnUrlChange location ->
            let
                route =
                    Routes.fromLocation location
            in
            ( { model
                | route = route
                , previousRoute = Just model.route
                , isSideNavOpen = False
              }
            , Batch
                [ Maybe.map FocusOn (Routes.headerId route examplesDict usageExamplesDict)
                    |> Maybe.withDefault None
                , case route of
                    Routes.Doodad exampleName ->
                        case findExample model exampleName of
                            Just ( _, _, initCmd ) ->
                                let
                                    _ =
                                        Debug.log "initing" exampleName
                                in
                                Command (Cmd.map (UpdateModuleStates exampleName) initCmd)

                            Nothing ->
                                None

                    _ ->
                        None
                ]
            )

        ChangeRoute route ->
            ( model
            , GoToRoute route
            )

        SkipToMainContent ->
            ( model
            , FocusOn "maincontent"
            )

        ToggleSideNav isOpen ->
            ( { model | isSideNavOpen = isOpen }, None )

        ToggleTooltip tooltipId True ->
            ( { model | openTooltip = Just tooltipId }, None )

        ToggleTooltip _ False ->
            ( { model | openTooltip = Nothing }, None )

        SelectContent { select, focus } ->
            ( { model | selectedContent = select }
            , Maybe.map FocusOn focus
                |> Maybe.withDefault None
            )

        LoadedPackages newPackagesResult ->
            let
                -- Ellie gets really slow to compile if we include all the packages, unfortunately!
                -- feel free to adjust the settings here if you need more packages for a particular example.
                removedPackages =
                    [ "avh4/elm-debug-controls"
                    , "BrianHicks/elm-particle"
                    , "elm-community/random-extra"
                    , "elm/browser"
                    , "elm/http"
                    , "elm/json"
                    , "elm/parser"
                    , "elm/random"
                    , "elm/regex"
                    , "elm/svg"
                    , "elm/url"
                    , "elm-community/string-extra"
                    , "Gizra/elm-keyboard-event"
                    , "pablohirafuji/elm-markdown"
                    , "tesk9/accessible-html-with-css"
                    , "tesk9/palette"
                    , "wernerdegroot/listzipper"
                    ]
            in
            ( { model
                | elliePackageDependencies =
                    List.foldl (\name -> Result.map (Dict.remove name))
                        (Result.map2 Dict.union model.elliePackageDependencies newPackagesResult)
                        removedPackages
              }
            , None
            )

        Focused _ ->
            ( model, None )

        NewInputMethod inputMethod ->
            ( { model | inputMethod = inputMethod }, None )

        SwallowEvent ->
            ( model, None )


type Effect
    = GoToRoute Route
    | GoToUrl Url
    | Load String
    | FocusOn String
    | None
    | Command (Cmd Msg)
    | Batch (List Effect)


perform : Key -> Effect -> Cmd Msg
perform navigationKey effect =
    case effect of
        GoToRoute route ->
            Browser.Navigation.pushUrl navigationKey (Routes.toString route)

        GoToUrl url ->
            Browser.Navigation.pushUrl navigationKey (Url.toString url)

        Load loc ->
            Browser.Navigation.load loc

        FocusOn id ->
            Task.attempt Focused (Browser.Dom.focus id)

        None ->
            Cmd.none

        Command cmd ->
            cmd

        Batch effects ->
            Cmd.batch (List.map (perform navigationKey) effects)


subscriptions : Model key -> Sub Msg
subscriptions model =
    let
        exampleSubs exampleName =
            case findExample model exampleName of
                Just ( example, exampleState, _ ) ->
                    Sub.map (UpdateModuleStates exampleName)
                        (example.subscriptions exampleState)

                Nothing ->
                    Sub.none
    in
    Sub.batch
        [ case model.route of
            Routes.Doodad exampleName ->
                exampleSubs exampleName

            Routes.CategoryDoodad _ exampleName ->
                exampleSubs exampleName

            Routes.Usage exampleName ->
                case findUsageExample model exampleName of
                    Just ( example, exampleState ) ->
                        Sub.map (UpdateUsageExamples exampleName)
                            (example.subscriptions exampleState)

                    Nothing ->
                        Sub.none

            _ ->
                Sub.none
        , Sub.map NewInputMethod InputMethod.subscriptions
        ]


view : Model key -> Document Msg
view model =
    let
        toBody view_ =
            List.map Html.toUnstyled
                [ view_
                , Html.map never Sprite.attach
                , Css.Global.global (InputMethod.styles model.inputMethod)
                , Css.Global.global
                    [ Css.Global.everything [ Css.boxSizing Css.borderBox ]
                    , Css.Global.body [ Css.margin Css.zero ]
                    ]
                ]

        exampleDocument exampleName =
            case findExample model exampleName of
                Just ( example, exampleState, _ ) ->
                    { title = example.name ++ " in the NoRedInk Component Catalog"
                    , body = viewExample model example exampleState |> toBody
                    }

                Nothing ->
                    { title =
                        "Component example \""
                            ++ Example.fromRouteName exampleName
                            ++ "\" was not found in the NoRedInk Component Catalog"
                    , body = toBody notFound
                    }
    in
    case model.route of
        Routes.Doodad exampleName ->
            exampleDocument exampleName

        Routes.CategoryDoodad _ exampleName ->
            exampleDocument exampleName

        Routes.Category category ->
            { title = Category.forDisplay category ++ " Category in the NoRedInk Component Catalog"
            , body = toBody (viewCategory model category)
            }

        Routes.Usage exampleName ->
            case findUsageExample model exampleName of
                Just ( example, state ) ->
                    { title = example.name ++ " Usage Example in the NoRedInk Component Catalog"
                    , body = viewUsageExample model example state |> toBody
                    }

                Nothing ->
                    { title =
                        "Usage example \""
                            ++ UsageExample.fromRouteName exampleName
                            ++ "\" was not found in the NoRedInk Component Catalog"
                    , body = toBody notFound
                    }

        Routes.All ->
            { title = "NoRedInk Component Catalog"
            , body = toBody (viewAll model)
            }


viewExample : Model key -> Example a Examples.Msg -> a -> Html Msg
viewExample model example state =
    Example.view { packageDependencies = model.elliePackageDependencies } example state
        |> Html.map (UpdateModuleStates example.name)
        |> viewLayout model [ Example.extraLinks (UpdateModuleStates example.name) example ]


viewUsageExample : Model key -> UsageExample a UsageExamples.Msg -> a -> Html Msg
viewUsageExample model example state =
    UsageExample.view example state
        |> Html.map (UpdateUsageExamples (UsageExample.routeName example))
        |> viewLayout model []


notFound : Html Msg
notFound =
    Page.notFound
        { link = ChangeRoute Routes.All
        , recoveryText = Page.ReturnTo "Component Catalog"
        }


viewAll : Model key -> Html Msg
viewAll model =
    viewLayout model [] <|
        viewExamplePreviews "all"
            { swallowEvent = SwallowEvent
            , navigate = Example.routeName >> Routes.Doodad >> ChangeRoute
            , exampleHref = Example.routeName >> Routes.Doodad >> Routes.toString
            }
            { swallowEvent = SwallowEvent
            , navigate = UsageExample.routeName >> Routes.Usage >> ChangeRoute
            , exampleHref = UsageExample.routeName >> Routes.Usage >> Routes.toString
            }
            Examples.all
            UsageExamples.all
            model.selectedContent


viewCategory : Model key -> Category -> Html Msg
viewCategory model category =
    let
        filtered items =
            List.filter
                (\item ->
                    Set.memberOf
                        (Set.fromList Category.sorter item.categories)
                        category
                )
                items
    in
    viewLayout model [] <|
        viewExamplePreviews (Category.forId category)
            { swallowEvent = SwallowEvent
            , navigate = Example.routeName >> Routes.CategoryDoodad category >> ChangeRoute
            , exampleHref = Example.routeName >> Routes.CategoryDoodad category >> Routes.toString
            }
            { swallowEvent = SwallowEvent
            , navigate = UsageExample.routeName >> Routes.Usage >> ChangeRoute
            , exampleHref = UsageExample.routeName >> Routes.Usage >> Routes.toString
            }
            (filtered Examples.all)
            (filtered UsageExamples.all)
            model.selectedContent


viewLayout : Model key -> List (Header.Attribute Route Msg) -> Html Msg -> Html Msg
viewLayout model headerExtras content =
    Html.div []
        [ Html.header []
            [ Routes.viewHeader model.route
                examplesDict
                usageExamplesDict
                headerExtras
            ]
        , Html.div
            [ css
                [ displayFlex
                , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
                , alignItems flexStart
                , Spacing.centeredContentWithSidePaddingAndCustomWidth (Css.px 1400)
                , Spacing.pageTopWhitespace
                , Spacing.pageBottomWhitespace
                ]
            ]
            [ navigation model
            , Html.main_
                [ css [ flexGrow (int 1) ]
                , id "maincontent"
                , Key.tabbable False
                ]
                [ content
                ]
            ]
        ]


type Content
    = ComponentExamples
    | UsageExamples


viewExamplePreviews :
    String
    ->
        { swallowEvent : Msg
        , navigate : Example Examples.State Examples.Msg -> Msg
        , exampleHref : Example Examples.State Examples.Msg -> String
        }
    ->
        { swallowEvent : Msg
        , navigate : UsageExample UsageExamples.State UsageExamples.Msg -> Msg
        , exampleHref : UsageExample UsageExamples.State UsageExamples.Msg -> String
        }
    -> List (Example Examples.State Examples.Msg)
    -> List (UsageExample UsageExamples.State UsageExamples.Msg)
    -> Content
    -> Html Msg
viewExamplePreviews containerId exampleNavConfig usageNavConfig examples usageExamples selectedContent =
    let
        viewBothTabs =
            Tabs.view
                { focusAndSelect = SelectContent
                , selected = selectedContent
                }
                [ Tabs.alignment Tabs.Left
                ]
                [ Tabs.build { id = ComponentExamples, idString = "component-examples" }
                    [ Tabs.tabString "Component Examples"
                    , examples
                        |> List.map (Example.preview exampleNavConfig)
                        |> examplesContainer [ Spacing.pageTopWhitespace ]
                        |> Tabs.panelHtml
                    ]
                , Tabs.build { id = UsageExamples, idString = "usage-examples" }
                    [ Tabs.tabString "Usage Examples"
                    , usageExamples
                        |> List.map (UsageExample.preview usageNavConfig)
                        |> examplesContainer [ Spacing.pageTopWhitespace ]
                        |> Tabs.panelHtml
                    ]
                ]
    in
    Html.div [ id containerId ]
        [ if List.isEmpty usageExamples then
            examplesContainer [] (List.map (Example.preview exampleNavConfig) examples)

          else
            viewBothTabs
        ]


examplesContainer : List Css.Style -> List (Html msg) -> Html msg
examplesContainer extraStyles =
    Html.div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "repeat(auto-fit, minmax(200px, 1fr))"
            , Css.justifyContent Css.start
            , Css.property "row-gap" (.value Spacing.verticalSpacerPx)
            , Css.property "column-gap" (.value Spacing.horizontalSpacerPx)
            , Css.batch extraStyles
            ]
        ]


navigation : Model key -> Html Msg
navigation { route, isSideNavOpen, openTooltip } =
    let
        examples =
            Examples.all

        exampleEntriesForCategory category =
            List.filter (\{ categories } -> List.any ((==) category) categories) examples
                |> List.map
                    (\example ->
                        SideNav.entry example.name
                            [ SideNav.href
                                (Routes.CategoryDoodad category (Example.routeName example))
                            ]
                    )

        categoryNavLinks : List (SideNav.Entry Route Msg)
        categoryNavLinks =
            List.map
                (\category ->
                    SideNav.entryWithChildren (Category.forDisplay category)
                        [ SideNav.href (Routes.Category category)
                        ]
                        (exampleEntriesForCategory category)
                )
                Category.all
    in
    SideNav.view
        { isCurrentRoute = (==) route
        , routeToString = Routes.toString
        , onSkipNav = SkipToMainContent
        }
        [ SideNav.navCss [ Css.zIndex (Css.int 1) ]
        , SideNav.navNotMobileCss
            [ VendorPrefixed.value "position" "sticky"
            , top (px 8)
            ]
        , SideNav.collapsible
            { isOpen = isSideNavOpen
            , toggle = ToggleSideNav
            , isTooltipOpen = openTooltip == Just SideNavOpenCloseTooltip
            , toggleTooltip = ToggleTooltip SideNavOpenCloseTooltip
            }
        , SideNav.navLabel "categories"
        , SideNav.navId "sidenav__categories"
        ]
        (SideNav.entry "All" [ SideNav.href Routes.All ]
            :: categoryNavLinks
            ++ [ SideNav.compactGroup "Resources"
                    []
                    [ SideNav.entry "Style Guide"
                        [ SideNav.linkExternal "https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BhJHYronm1RGM1hRfnkvhrZMAg-PvOLxeX3oyujYEzdJx5pu"
                        ]
                    , SideNav.entry "Additional Components"
                        [ SideNav.linkExternal "https://www.noredink.com/assorted_components/"
                        ]
                    ]
               ]
        )


loadPackage : Cmd Msg
loadPackage =
    Http.get
        { url = "/package.json"
        , expect =
            Http.expectJson
                LoadedPackages
                (Decode.map2 Dict.singleton
                    (Decode.field "name" Decode.string)
                    (Decode.field "version" Decode.string)
                )
        }


loadApplicationDependencies : Cmd Msg
loadApplicationDependencies =
    Http.get
        { url = "/application.json"
        , expect =
            Http.expectJson
                LoadedPackages
                (Decode.at [ "dependencies", "direct" ] (Decode.dict Decode.string))
        }
