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
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Header.V1 as Header
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Page.V3 as Page
import Nri.Ui.SideNav.V5 as SideNav
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Sprite.V1 as Sprite
import Nri.Ui.UiIcon.V1 as UiIcon
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
    , moduleStates : Dict String (Example Examples.State Examples.Msg)
    , usageExampleStates : Dict String (UsageExample UsageExamples.State UsageExamples.Msg)
    , isSideNavOpen : Bool
    , openTooltip : Maybe TooltipId
    , navigationKey : key
    , elliePackageDependencies : Result Http.Error (Dict String String)
    , inputMethod : InputMethod
    }


init : () -> Url -> key -> ( Model key, Effect )
init () url key =
    ( { route = Routes.fromLocation url
      , previousRoute = Nothing
      , moduleStates =
            Dict.fromList
                (List.map
                    (\example -> ( Example.routeName example, example ))
                    Examples.all
                )
      , usageExampleStates =
            Dict.fromList
                (List.map (\example -> ( UsageExample.routeName example, example ))
                    UsageExamples.all
                )
      , isSideNavOpen = False
      , openTooltip = Nothing
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
    | LoadedPackages (Result Http.Error (Dict String String))
    | Focused (Result Browser.Dom.Error ())
    | NewInputMethod InputMethod
    | SwallowEvent


update : Msg -> Model key -> ( Model key, Effect )
update action model =
    case action of
        UpdateModuleStates key exampleMsg ->
            case Dict.get key model.moduleStates of
                Just example ->
                    example.update exampleMsg example.state
                        |> Tuple.mapFirst
                            (\newState ->
                                let
                                    newExample =
                                        { example | state = newState }
                                in
                                { model
                                    | moduleStates = Dict.insert key newExample model.moduleStates
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map (UpdateModuleStates key) >> Command)

                Nothing ->
                    ( model, None )

        UpdateUsageExamples key exampleMsg ->
            case Dict.get key model.usageExampleStates of
                Just usageExample ->
                    usageExample.update exampleMsg usageExample.state
                        |> Tuple.mapFirst
                            (\newState ->
                                let
                                    newExample =
                                        { usageExample | state = newState }
                                in
                                { model
                                    | usageExampleStates = Dict.insert key newExample model.usageExampleStates
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
            , Maybe.map FocusOn (Routes.headerId route model.moduleStates model.usageExampleStates)
                |> Maybe.withDefault None
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
                    , "rtfeldman/elm-sorter-experiment"
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


subscriptions : Model key -> Sub Msg
subscriptions model =
    let
        exampleSubs exampleName =
            case Dict.get exampleName model.moduleStates of
                Just example ->
                    Sub.map (UpdateModuleStates exampleName)
                        (example.subscriptions example.state)

                Nothing ->
                    Sub.none
    in
    Sub.batch
        [ case model.route of
            Routes.Doodad exampleName ->
                exampleSubs exampleName

            Routes.CategoryDoodad _ exampleName ->
                exampleSubs exampleName

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
            case Dict.get exampleName model.moduleStates of
                Just example ->
                    { title = example.name ++ " in the NoRedInk Component Catalog"
                    , body = viewExample model example |> toBody
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

        Routes.NotFound name ->
            { title = name ++ " was not found in the NoRedInk Component Catalog"
            , body = toBody notFound
            }

        Routes.Category category ->
            { title = Category.forDisplay category ++ " Category in the NoRedInk Component Catalog"
            , body = toBody (viewCategory model category)
            }

        Routes.Usage exampleName ->
            case Dict.get exampleName model.usageExampleStates of
                Just example ->
                    { title = example.name ++ " Usage Example in the NoRedInk Component Catalog"
                    , body = viewUsageExample model example |> toBody
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


viewExample : Model key -> Example a Examples.Msg -> Html Msg
viewExample model example =
    Example.view { packageDependencies = model.elliePackageDependencies } example
        |> Html.map (UpdateModuleStates example.name)
        |> viewLayout model [ Example.extraLinks (UpdateModuleStates example.name) example ]


viewUsageExample : Model key -> UsageExample a UsageExamples.Msg -> Html Msg
viewUsageExample model example =
    UsageExample.view example
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
            (Dict.values model.moduleStates)
            (Dict.values model.usageExampleStates)


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
                (Dict.values items)
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
            (filtered model.moduleStates)
            (filtered model.usageExampleStates)


viewLayout : Model key -> List (Header.Attribute Route Msg) -> Html Msg -> Html Msg
viewLayout model headerExtras content =
    Html.div []
        [ Html.header []
            [ Routes.viewHeader model.route
                model.moduleStates
                model.usageExampleStates
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
    -> Html Msg
viewExamplePreviews containerId exampleNavConfig usageNavConfig examples usageExamples =
    Html.div [ id containerId ]
        [ Heading.h2 [ Heading.plaintext "Components" ]
        , examplesContainer (List.map (Example.preview exampleNavConfig) examples)
        , viewIf
            (\_ ->
                Heading.h2
                    [ Heading.plaintext "Usage Examples"
                    , Heading.css [ Css.marginTop (Css.px 30) ]
                    ]
            )
            (List.length usageExamples > 0)
        , examplesContainer (List.map (UsageExample.preview usageNavConfig) usageExamples)
        ]


examplesContainer : List (Html msg) -> Html msg
examplesContainer =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "row-gap" (.value Spacing.verticalSpacerPx)
            , Css.property "column-gap" (.value Spacing.horizontalSpacerPx)
            ]
        ]


navigation : Model key -> Html Msg
navigation { moduleStates, route, isSideNavOpen, openTooltip } =
    let
        examples =
            Dict.values moduleStates

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
        [ SideNav.navNotMobileCss
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
