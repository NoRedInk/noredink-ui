module App exposing (Effect(..), Model, Msg(..), init, perform, subscriptions, update, view)

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key)
import Category exposing (Category)
import Css exposing (..)
import Css.Media exposing (withMedia)
import Dict exposing (Dict)
import Example exposing (Example)
import Examples
import Html.Styled.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Page.V3 as Page
import Nri.Ui.SideNav.V4 as SideNav
import Nri.Ui.Sprite.V1 as Sprite
import Nri.Ui.UiIcon.V1 as UiIcon
import Routes
import Sort.Set as Set
import Task
import Url exposing (Url)


type alias Route =
    Routes.Route Examples.State Examples.Msg


type alias Model key =
    { -- Global UI
      route : Route
    , previousRoute : Maybe Route
    , moduleStates : Dict String (Example Examples.State Examples.Msg)
    , isSideNavOpen : Bool
    , openTooltip : Maybe TooltipId
    , navigationKey : key
    , elliePackageDependencies : Result Http.Error (Dict String String)
    }


init : () -> Url -> key -> ( Model key, Effect )
init () url key =
    let
        moduleStates =
            Dict.fromList
                (List.map (\example -> ( example.name, example )) Examples.all)
    in
    ( { route = Routes.fromLocation moduleStates url
      , previousRoute = Nothing
      , moduleStates = moduleStates
      , isSideNavOpen = True
      , openTooltip = Nothing
      , navigationKey = key
      , elliePackageDependencies = Ok Dict.empty
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
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | ChangeRoute Route
    | SkipToMainContent
    | ToggleSideNav Bool
    | ToggleTooltip TooltipId Bool
    | LoadedPackages (Result Http.Error (Dict String String))
    | Focused (Result Browser.Dom.Error ())


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
                                    , route =
                                        Maybe.withDefault model.route
                                            (Routes.updateExample newExample model.route)
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map (UpdateModuleStates key) >> Command)

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
                    Routes.fromLocation model.moduleStates location
            in
            ( { model | route = route, previousRoute = Just model.route }
            , Maybe.map FocusOn (Routes.headerId route)
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
    Dict.values model.moduleStates
        |> List.map (\example -> Sub.map (UpdateModuleStates example.name) (example.subscriptions example.state))
        |> Sub.batch


view : Model key -> Document Msg
view model =
    let
        toBody view_ =
            List.map Html.toUnstyled
                [ view_
                , Html.map never Sprite.attach
                ]
    in
    case model.route of
        Routes.Doodad example ->
            { title = example.name ++ " in the NoRedInk Style Guide"
            , body = viewExample model example |> toBody
            }

        Routes.CategoryDoodad _ example ->
            { title = example.name ++ " in the NoRedInk Style Guide"
            , body = viewExample model example |> toBody
            }

        Routes.NotFound name ->
            { title = name ++ " was not found in the NoRedInk Style Guide"
            , body = toBody notFound
            }

        Routes.Category category ->
            { title = Category.forDisplay category ++ " Category in the NoRedInk Style Guide"
            , body = toBody (viewCategory model category)
            }

        Routes.All ->
            { title = "NoRedInk Style Guide"
            , body = toBody (viewAll model)
            }


viewExample : Model key -> Example a Examples.Msg -> Html Msg
viewExample model example =
    Example.view { packageDependencies = model.elliePackageDependencies } example
        |> Html.map (UpdateModuleStates example.name)
        |> withSideNav model


notFound : Html Msg
notFound =
    Page.notFound
        { link = ChangeRoute Routes.All
        , recoveryText = Page.ReturnTo "Component Library"
        }


viewAll : Model key -> Html Msg
viewAll model =
    withSideNav model <|
        viewPreviews "all"
            { navigate = Routes.Doodad >> ChangeRoute
            , exampleHref = Routes.Doodad >> Routes.toString
            }
            (Dict.values model.moduleStates)


viewCategory : Model key -> Category -> Html Msg
viewCategory model category =
    withSideNav model
        (model.moduleStates
            |> Dict.values
            |> List.filter
                (\doodad ->
                    Set.memberOf
                        (Set.fromList Category.sorter doodad.categories)
                        category
                )
            |> viewPreviews (Category.forId category)
                { navigate = Routes.CategoryDoodad category >> ChangeRoute
                , exampleHref = Routes.CategoryDoodad category >> Routes.toString
                }
        )


withSideNav : Model key -> Html Msg -> Html Msg
withSideNav model content =
    Html.div
        [ css
            [ displayFlex
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            , alignItems flexStart
            , maxWidth (Css.px 1400)
            , margin auto
            ]
        ]
        [ navigation model
        , Html.main_
            [ css
                [ flexGrow (int 1)
                , margin2 (px 40) zero
                , Css.minHeight (Css.vh 100)
                ]
            , id "maincontent"
            , Key.tabbable False
            ]
            [ Html.div [ css [ Css.marginBottom (Css.px 30) ] ]
                [ Routes.viewBreadCrumbs model.route
                ]
            , content
            ]
        ]


viewPreviews :
    String
    ->
        { navigate : Example Examples.State Examples.Msg -> Msg
        , exampleHref : Example Examples.State Examples.Msg -> String
        }
    -> List (Example Examples.State Examples.Msg)
    -> Html Msg
viewPreviews containerId navConfig examples =
    examples
        |> List.map (Example.preview navConfig)
        |> Html.div
            [ id containerId
            , css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.property "gap" "10px"
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
                            [ SideNav.href (Routes.CategoryDoodad category example)
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
            , top (px 55)
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
        (SideNav.entry "Usage Guidelines"
            [ SideNav.linkExternal "https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BhJHYronm1RGM1hRfnkvhrZMAg-PvOLxeX3oyujYEzdJx5pu"
            , SideNav.icon UiIcon.openInNewTab
            ]
            :: SideNav.entry "All" [ SideNav.href Routes.All ]
            :: categoryNavLinks
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
