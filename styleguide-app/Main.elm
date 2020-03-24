module Main exposing (init, main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key)
import Category
import Css exposing (..)
import Html as RootHtml
import Html.Attributes
import Html.Styled as Html exposing (Html, img)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events
import ModuleExample as ModuleExample exposing (ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Css.VendorPrefixed as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import NriModules as NriModules exposing (ModuleStates, nriThemedModules)
import Routes as Routes exposing (Route(..))
import Sort.Set as Set
import Task
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { -- Global UI
      route : Route
    , moduleStates : ModuleStates
    , navigationKey : Key
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    ( { route = Routes.fromLocation url
      , moduleStates = NriModules.init
      , navigationKey = key
      }
    , Cmd.none
    )


type Msg
    = UpdateModuleStates NriModules.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | SkipToMainContent
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UpdateModuleStates msg ->
            let
                ( moduleStates, cmd ) =
                    NriModules.update msg model.moduleStates
            in
            ( { model | moduleStates = moduleStates }
            , Cmd.map UpdateModuleStates cmd
            )

        OnUrlRequest request ->
            case request of
                Internal loc ->
                    ( model, Browser.Navigation.pushUrl model.navigationKey (Url.toString loc) )

                External loc ->
                    ( model, Browser.Navigation.load loc )

        OnUrlChange route ->
            ( { model | route = Routes.fromLocation route }, Cmd.none )

        SkipToMainContent ->
            ( model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "maincontent")
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateModuleStates (NriModules.subscriptions model.moduleStates)


view : Model -> Document Msg
view model =
    { title = "Style Guide"
    , body = [ view_ model |> Html.toUnstyled ]
    }


view_ : Model -> Html Msg
view_ model =
    Html.styled Html.div
        [ displayFlex
        , alignItems flexStart
        , minHeight (vh 100)
        ]
        []
        [ navigation model.route
        , Html.styled Html.main_
            [ flexGrow (int 1) ]
            [ id "maincontent", Attributes.tabindex -1 ]
            (case model.route of
                Routes.Doodad doodad ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Heading.h2 [] [ Html.text ("Viewing " ++ doodad ++ " doodad only") ]
                        , nriThemedModules model.moduleStates
                            |> List.filter (\m -> m.name == doodad)
                            |> List.map (ModuleExample.view False)
                            |> Html.div []
                            |> Html.map UpdateModuleStates
                        ]
                    ]

                Routes.Category category ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Heading.h2 [] [ Html.text (Category.forDisplay category) ]
                        , nriThemedModules model.moduleStates
                            |> List.filter (\doodad -> Set.memberOf doodad.categories category)
                            |> List.map (ModuleExample.view True)
                            |> Html.div [ id (Category.forId category) ]
                            |> Html.map UpdateModuleStates
                        ]
                    ]

                Routes.All ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Heading.h2 [] [ Html.text "All" ]
                        , nriThemedModules model.moduleStates
                            |> List.map (ModuleExample.view True)
                            |> Html.div []
                            |> Html.map UpdateModuleStates
                        ]
                    ]
            )
        ]


navigation : Route -> Html Msg
navigation route =
    let
        isActive category =
            case route of
                Routes.Category routeCategory ->
                    category == routeCategory

                _ ->
                    False

        link active hash displayName =
            Html.styled Html.a
                [ backgroundColor transparent
                , borderStyle none
                , textDecoration none
                , if active then
                    color Colors.navy

                  else
                    color Colors.azure
                , Fonts.baseFont
                ]
                [ Attributes.href hash ]
                [ Html.text displayName ]

        navLink category =
            link (isActive category)
                ("#/category/" ++ Debug.toString category)
                (Category.forDisplay category)

        toNavLi element =
            Html.li
                [ css
                    [ margin2 (px 10) zero
                    , listStyle none
                    , textDecoration none
                    ]
                ]
                [ element ]
    in
    Html.styled Html.nav
        [ flexBasis (px 200)
        , backgroundColor Colors.gray96
        , marginRight (px 40)
        , padding (px 25)
        , VendorPrefixed.value "position" "sticky"
        , top (px 150)
        , flexShrink zero
        ]
        [ attribute "aria-label" "Main Navigation"
        ]
        [ Html.styled Html.button
            [ backgroundColor transparent
            , borderStyle none
            , textDecoration none
            , color Colors.azure
            , Fonts.baseFont
            , Css.marginBottom (px 20)
            ]
            [ Events.onClick SkipToMainContent, id "skip" ]
            [ Html.text "Skip to main content" ]
        , Heading.h4 [] [ Html.text "Categories" ]
        , (link (route == Routes.All) "#/" "All"
            :: List.map navLink Category.all
          )
            |> List.map toNavLi
            |> Html.styled Html.ul
                [ margin4 zero zero (px 40) zero
                , padding zero
                ]
                [ id "categories" ]
        ]


sectionStyles : Css.Style
sectionStyles =
    Css.batch [ margin2 (px 40) zero ]
