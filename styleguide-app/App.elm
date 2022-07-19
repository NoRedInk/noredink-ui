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
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import EllieLink
import Example exposing (Example)
import Examples
import Html.Styled.Attributes exposing (..)
import Http
import Json.Decode as Decode
import KeyboardSupport
import Nri.Ui.Accordion.V3 as Accordion
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Page.V3 as Page
import Nri.Ui.SideNav.V3 as SideNav
import Nri.Ui.Sprite.V1 as Sprite
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import Routes
import Section exposing (Section)
import Sort.Set as Set exposing (Set)
import Task
import Url exposing (Url)


type alias Route =
    Routes.Route Examples.State Examples.Msg


type alias Model key =
    { -- Global UI
      route : Route
    , previousRoute : Maybe Route
    , moduleStates : Dict String (Example Examples.State Examples.Msg)
    , settings : Dict String (Control Examples.Settings)
    , toCode : Dict String ToCode
    , expandedAccordions : Set Section
    , navigationKey : key
    , elliePackageDependencies : Result Http.Error (Dict String String)
    }


{-| TODO: extract to Code modul
-}
type alias ToCode =
    { mainType : String
    , extraImports : List String
    , toExampleCode : Examples.Settings -> List { sectionName : String, code : String }
    }


init : () -> Url -> key -> ( Model key, Effect )
init () url key =
    let
        moduleStates =
            Dict.fromList (List.map (\example -> ( example.name, example )) Examples.all)

        ( settings, toCode ) =
            Examples.allWithConfig
                |> List.map
                    (\example ->
                        ( ( example.name, example.settings )
                        , ( example.name
                          , { mainType = example.mainType
                            , extraImports = example.extraImports
                            , toExampleCode = example.toExampleCode
                            }
                          )
                        )
                    )
                |> List.unzip
                |> Tuple.mapBoth Dict.fromList Dict.fromList
    in
    ( { route = Routes.fromLocation moduleStates url
      , previousRoute = Nothing
      , moduleStates = moduleStates
      , settings = settings
      , toCode = toCode
      , expandedAccordions = Section.initiallyExpanded
      , navigationKey = key
      , elliePackageDependencies = Ok Dict.empty
      }
    , Cmd.batch
        [ loadPackage
        , loadApplicationDependencies
        ]
        |> Command
    )


type Msg
    = UpdateAttributes String (Control Examples.Settings)
    | UpdateModuleStates String Examples.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | ChangeRoute Route
    | SkipToMainContent
    | LoadedPackages (Result Http.Error (Dict String String))
    | SetAccordion Section Bool
    | Focus String
    | Focused (Result Browser.Dom.Error ())


update : Msg -> Model key -> ( Model key, Effect )
update action model =
    case action of
        UpdateAttributes key settings ->
            ( { model | settings = Dict.insert key settings model.settings }
            , None
            )

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

        SetAccordion section isOpen ->
            ( { model
                | expandedAccordions =
                    if isOpen then
                        Set.insert section model.expandedAccordions

                    else
                        Set.remove section model.expandedAccordions
              }
            , None
            )

        Focus id ->
            ( model, FocusOn id )

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
                , Accordion.styleAccordion
                    { entryStyles = []
                    , entryExpandedStyles = []
                    , entryClosedStyles = []
                    , headerStyles = []
                    , headerExpandedStyles = []
                    , headerClosedStyles = []
                    , contentStyles = []
                    }
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
    Html.div [ id (String.replace "." "-" example.name) ]
        [ Example.viewExampleNav example
        , Accordion.view { entries = List.filterMap identity (entries model example), focus = Focus }
        , Example.viewExample
            { packageDependencies = model.elliePackageDependencies }
            example
            |> Html.map (UpdateModuleStates example.name)
        ]
        |> withSideNav model


entries model example =
    let
        accordionEntry_ =
            accordionEntry model.expandedAccordions
    in
    (case Dict.get example.name model.settings of
        Just settings_ ->
            let
                { mainType, extraImports, toExampleCode } =
                    Dict.get example.name model.toCode
                        |> Maybe.withDefault
                            { mainType = ""
                            , extraImports = []
                            , toExampleCode = \_ -> []
                            }

                value =
                    Control.currentValue settings_

                ellieLink =
                    EllieLink.view { packageDependencies = model.elliePackageDependencies }

                exampleCodes =
                    toExampleCode value
            in
            [ Html.div
                [ css [ Css.Global.descendants [ Css.Global.everything [ Fonts.baseFont ] ] ] ]
                [ Control.view (UpdateAttributes example.name) settings_
                    |> Html.fromUnstyled
                ]
                |> accordionEntry_ Section.Settings
                |> Just
            , if not (List.isEmpty exampleCodes) then
                ControlView.viewExampleCode ellieLink
                    { name = example.name
                    , version = example.version
                    , mainType = mainType
                    , extraImports = extraImports
                    }
                    exampleCodes
                    |> Html.div []
                    |> accordionEntry_ Section.ExampleCode
                    |> Just

              else
                Nothing
            ]

        Nothing ->
            []
    )
        ++ [ KeyboardSupport.view example.keyboardSupport
                |> Maybe.map (accordionEntry_ Section.KeyboardSupport)
           ]


accordionEntry : Set Section -> Section -> Html Msg -> Accordion.AccordionEntry Msg
accordionEntry expandedAccordions section view_ =
    Accordion.AccordionEntry
        { caret =
            DisclosureIndicator.large [ Css.marginRight (Css.px 8) ]
                >> Svg.toHtml
        , content = \() -> view_
        , entryClass = "example-section"
        , headerContent = Html.text (Section.name section)
        , headerId = Section.headerId section
        , headerLevel = Accordion.H2
        , isExpanded = Set.memberOf expandedAccordions section
        , toggle = Just (SetAccordion section)
        }
        []


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


withSideNav :
    { model | route : Route, moduleStates : Dict String (Example Examples.State Examples.Msg) }
    -> Html Msg
    -> Html Msg
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


navigation :
    { model | route : Route, moduleStates : Dict String (Example Examples.State Examples.Msg) }
    -> Html Msg
navigation { moduleStates, route } =
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
