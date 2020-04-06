module Main exposing (init, main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key)
import Category
import Css exposing (..)
import Dict exposing (Dict)
import Example exposing (Example)
import Examples
import Html as RootHtml
import Html.Attributes
import Html.Styled as Html exposing (Html, img)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Css.VendorPrefixed as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
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
    , moduleStates : Dict String (Example Examples.State Examples.Msg)
    , navigationKey : Key
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    ( { route = Routes.fromLocation url
      , moduleStates =
            Dict.fromList
                (List.map (\example -> ( example.name, example )) Examples.all)
      , navigationKey = key
      }
    , Cmd.none
    )


type Msg
    = UpdateModuleStates String Examples.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | SkipToMainContent
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UpdateModuleStates key exampleMsg ->
            case Dict.get key model.moduleStates of
                Just example ->
                    example.update exampleMsg example.state
                        |> Tuple.mapFirst
                            (\newState ->
                                { model
                                    | moduleStates =
                                        Dict.insert key
                                            { example | state = newState }
                                            model.moduleStates
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map (UpdateModuleStates key))

                Nothing ->
                    ( model, Cmd.none )

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
    Dict.values model.moduleStates
        |> List.map (\example -> Sub.map (UpdateModuleStates example.name) (example.subscriptions example.state))
        |> Sub.batch


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
                        , Dict.values model.moduleStates
                            |> List.filter (\m -> m.name == doodad)
                            |> List.map
                                (\example ->
                                    Example.view False example
                                        |> Html.map (UpdateModuleStates example.name)
                                )
                            |> Html.div []
                        ]
                    ]

                Routes.Category category ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Heading.h2 [] [ Html.text (Category.forDisplay category) ]
                        , Dict.values model.moduleStates
                            |> List.filter
                                (\doodad ->
                                    Set.memberOf
                                        (Set.fromList Category.sorter doodad.categories)
                                        category
                                )
                            |> List.map
                                (\example ->
                                    Example.view True example
                                        |> Html.map (UpdateModuleStates example.name)
                                )
                            |> Html.div [ id (Category.forId category) ]
                        ]
                    ]

                Routes.All ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Heading.h2 [] [ Html.text "All" ]
                        , Dict.values model.moduleStates
                            |> List.map
                                (\example ->
                                    Example.view True example
                                        |> Html.map (UpdateModuleStates example.name)
                                )
                            |> Html.div []
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
