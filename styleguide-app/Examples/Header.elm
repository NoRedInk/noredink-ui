module Examples.Header exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled.Role as Role
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Header.V1 as Header
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "Header"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = [ viewPreview ]
    , view =
        \ellieLinkConfig state ->
            let
                examples =
                    []

                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode ( name, _ ) =
                                { sectionName = name
                                , code =
                                    moduleName
                                        ++ "."
                                        ++ name
                                        ++ "\n    [ "
                                        ++ String.join "\n    , " (List.map Tuple.first settings)
                                        ++ "\n    ]"
                                }
                        in
                        List.map toExampleCode examples
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Header.view
                attributes
                { breadcrumbs =
                    BreadCrumbs.init
                        { id = "page-header"
                        , text = "Page"
                        , route = ()
                        }
                        []
                , isCurrentRoute = \_ -> True
                }
            , examples
                |> List.map (\( name, view ) -> ( name, view attributes ))
                |> viewExamples
            ]
    }


viewPreview : Html msg
viewPreview =
    div
        [ css
            [ Css.height (Css.px 80)
            , Css.backgroundColor Colors.white
            , Css.padding (Css.px 8)
            ]
        , Role.presentation
        ]
        [ div
            [ css
                [ Css.backgroundColor Colors.gray96
                ]
            ]
            [ div
                [ css
                    [ Css.color Colors.navy
                    , Css.fontSize (Css.px 10)
                    , Css.fontWeight Css.bold
                    , Fonts.baseFont
                    , Css.padding (Css.px 3)
                    ]
                ]
                [ text "All"
                , UiIcon.arrowRight
                    |> Svg.withWidth (Css.px 8)
                    |> Svg.withHeight (Css.px 8)
                    |> Svg.withColor Colors.gray75
                    |> Svg.withCss [ Css.margin2 Css.zero (Css.px 3) ]
                    |> Svg.toHtml
                , text "Category 1"
                ]
            ]
        ]


{-| -}
type alias State =
    { control : Control Settings
    }


init : State
init =
    { control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "extraContent"
                (Control.value
                    ( "Header.extraContent [ Html.text \"…\" ]"
                    , Header.extraContent CommonControls.exampleHtml
                    )
                )
            |> ControlExtra.optionalListItem "description"
                (Control.map
                    (\value ->
                        ( "Header.description " ++ Code.string value
                        , Header.description value
                        )
                    )
                    (Control.string "This page has some good content.")
                )
            |> ControlExtra.optionalListItem "extraSubheadContent"
                (Control.value
                    ( "Header.extraSubheadContent [ Html.text \"…\" ]"
                    , Header.extraSubheadContent CommonControls.exampleHtml
                    )
                )
            |> ControlExtra.optionalListItem "customPageWidth"
                (Control.map
                    (\width ->
                        ( "Header.customPageWidth (Css.px" ++ String.fromFloat width ++ ")"
                        , Header.customPageWidth (Css.px width)
                        )
                    )
                    (ControlExtra.float 750)
                )
    }


type alias Settings =
    List ( String, Header.Attribute () Msg )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
