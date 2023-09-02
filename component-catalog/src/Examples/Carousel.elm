module Examples.Carousel exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Carousel.V2 as Carousel
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as Attributes
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


type alias State =
    { selected : Int
    , settings : Control Settings
    }


init : State
init =
    { selected = 0
    , settings = initSettings
    }


type alias Settings =
    { items : Int
    , controlListStyles : ( String, List Style )
    , controlStyles : ( String, Bool -> List Style )
    , carouselType : CarouselType
    }


type CarouselType
    = Tabs
    | PrevNext
    | Combined


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "items" (Debug.Control.Extra.int 4)
        |> Control.field "controlListStyles" controlControlListStyles
        |> Control.field "controlStyles" controlControlStyles
        |> Control.field "carouselType" controlCarouselType


controlCarouselType : Control CarouselType
controlCarouselType =
    Control.choice
        [ ( "viewWithTabControls", Control.value Tabs )
        , ( "viewWithPreviousAndNextControls", Control.value PrevNext )
        , ( "viewWithCombinedControls", Control.value Combined )
        ]


controlControlListStyles : Control ( String, List Style )
controlControlListStyles =
    ( "[ Css.displayFlex, Css.property \"gap\" \"20px\" ]"
    , [ Css.displayFlex, Css.property "gap" "20px" ]
    )
        |> Control.value
        |> Control.maybe False
        |> Control.map (Maybe.withDefault ( "[]", [] ))


controlStyles : Bool -> List Css.Style
controlStyles isSelected =
    let
        ( backgroundColor, textColor ) =
            if isSelected then
                ( Colors.azure, Colors.white )

            else
                ( Colors.gray92, Colors.gray20 )
    in
    [ Css.padding2 (Css.px 10) (Css.px 20)
    , Css.backgroundColor backgroundColor
    , Css.borderRadius (Css.px 8)
    , Css.border Css.zero
    , Css.color textColor
    , Css.cursor Css.pointer
    ]


controlControlStyles : Control ( String, Bool -> List Css.Style )
controlControlStyles =
    let
        simplifiedCodeVersion =
            "\\isSelected -> [ -- styles that depend on selection status\n    ]"
    in
    controlStyles
        |> Control.value
        |> Control.maybe False
        |> Control.map (Maybe.withDefault (\_ -> []))
        |> Control.map (\v -> ( simplifiedCodeVersion, v ))


type Msg
    = FocusAndSelectItem { select : Int, focus : Maybe String }
    | SelectAndAnnounce { select : Int, announce : String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        FocusAndSelectItem { select, focus } ->
            ( { model | selected = select }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        Focused error ->
            ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Cmd.none )

        SelectAndAnnounce { select, announce } ->
            ( { model | selected = select }
            , Cmd.none
            )


moduleName : String
moduleName =
    "Carousel"


version : Int
version =
    2


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Navigation ]
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Move focus to the currently-selected Tab's tab panel"
          }
        , { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Select the tab to the left of the currently-selected Tab"
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Select the tab to the right of the currently-selected Tab"
          }
        ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Carousel component to give Component Catalog users a sense of what the
          -- component might look like
          Html.div []
            [ Html.text "1 slide"
            , Html.div [ Attributes.css [ Css.displayFlex, Css.property "gap" "5px" ] ]
                [ Html.div [ Attributes.css (controlStyles True) ] [ Html.text "1" ]
                , Html.div [ Attributes.css (controlStyles False) ] [ Html.text "2" ]
                , Html.div [ Attributes.css (controlStyles False) ] [ Html.text "2" ]
                ]
            ]
        ]
    , about = []
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings

                ( code, view ) =
                    case settings.carouselType of
                        Tabs ->
                            viewWithTabControls model

                        PrevNext ->
                            viewWithPreviousAndNextControls model

                        Combined ->
                            viewWithCombinedControls model
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = Just "RootHtml.Html { select : Int, focus : Maybe String }"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code = code
                          }
                        ]
                }
            , view
            ]
    }


viewWithPreviousAndNextControls : State -> ( String, Html Msg )
viewWithPreviousAndNextControls model =
    let
        settings =
            Control.currentValue model.settings

        allItems =
            List.repeat settings.items ()
                |> List.indexedMap
                    (\id _ ->
                        ( Code.recordMultiline
                            [ ( "id", String.fromInt id )
                            , ( "idString", "\"" ++ String.fromInt id ++ "\"" )
                            , ( "slideHtml", "Html.text " ++ Code.string (String.fromInt (id + 1) ++ " slide") )
                            , ( "accessibleLabel"
                              , Code.fromModule moduleName "LabelledByIdOfVisibleLabel "
                                    ++ Code.string (String.fromInt (id + 1) ++ " of " ++ String.fromInt settings.items)
                              )
                            , ( "visibleLabelId", "Nothing" )
                            ]
                            3
                        , { id = id
                          , slideHtml = Html.text (String.fromInt (id + 1))
                          , accessibleLabel = String.fromInt (id + 1) ++ " of " ++ String.fromInt settings.items
                          , visibleLabelId = Nothing
                          , idString = String.fromInt id
                          }
                        )
                    )

        { viewPreviousButton, viewNextButton, slides, containerAttributes } =
            Carousel.viewWithPreviousAndNextControls
                { selected = model.selected
                , panels = List.map Tuple.second allItems
                , previousButton =
                    { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                , nextButton =
                    { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                , accessibleLabel = "Items"
                , visibleLabelId = Nothing
                , role = Carousel.Group
                , selectAndAnnounce = SelectAndAnnounce
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithPreviousAndNextControls"
            ++ Code.recordMultiline
                [ ( "selected", Code.string (String.fromInt model.selected) )
                , ( "panels", Code.listMultiline (List.map Tuple.first allItems) 3 )
                , ( "previousButton", "{ attributes = [], icon = UiIcon.arrowLeft , name = \"Previous\" }" )
                , ( "nextButton", "{ attributes = [], icon = UiIcon.arrowLeft , name = \"Next\" }" )
                , ( "accessibleLabel", "Items" )
                , ( "visibleLabelId", "Nothing" )
                , ( "role", Code.fromModule moduleName "Group" )
                , ( "focusAndSelect", "FocusAndSelectItem" )
                ]
                1
        , Code.anonymousFunction "{ viewPreviousButton, viewNextButton, slides, containerAttributes }"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [ slides, controls, viewPreviousButton, viewNextButton ]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, viewPreviousButton, viewNextButton ]
    )


viewWithCombinedControls : State -> ( String, Html Msg )
viewWithCombinedControls model =
    let
        settings =
            Control.currentValue model.settings

        allItems =
            List.repeat settings.items ()
                |> List.indexedMap toCarouselItem

        { tabControls, slides, viewPreviousButton, viewNextButton, containerAttributes } =
            Carousel.viewWithCombinedControls
                { focusAndSelect = FocusAndSelectItem
                , selected = model.selected
                , tabControlListStyles = Tuple.second settings.controlListStyles
                , tabControlStyles = Tuple.second settings.controlStyles
                , panels = List.map Tuple.second allItems
                , previousButton =
                    { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                , nextButton =
                    { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                , labelledBy = Carousel.LabelledByIdOfVisibleLabel "Items"
                , role = Carousel.Group
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithCombinedControls"
            ++ Code.record
                [ ( "focusAndSelect", "identity" )
                , ( "selected", Code.string (String.fromInt model.selected) )
                , ( "tabControlListStyles", Tuple.first settings.controlListStyles )
                , ( "tabControlStyles", Tuple.first settings.controlStyles )
                , ( "panels", Code.listMultiline (List.map Tuple.first allItems) 2 )
                , ( "previousButton", "{ attributes = [], icon = UiIcon.arrowLeft , name = \"Previous\" }" )
                , ( "nextButton", "{ attributes = [], icon = UiIcon.arrowLeft , name = \"Next\" }" )
                , ( "labelledBy", Code.fromModule moduleName "LabelledByIdOfVisibleLabel " ++ Code.string "Items" )
                , ( "role", Code.fromModule moduleName "Group" )
                ]
        , Code.anonymousFunction "{ tabControls, slides, viewPreviousButton, viewNextButton, containerAttributes }"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [ slides, tabControls, viewPreviousButton, viewNextButton  ]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, tabControls, viewPreviousButton, viewNextButton ]
    )


viewWithTabControls : State -> ( String, Html Msg )
viewWithTabControls model =
    let
        settings =
            Control.currentValue model.settings

        allItems =
            List.repeat settings.items ()
                |> List.indexedMap toCarouselItem

        { controls, slides, containerAttributes } =
            Carousel.viewWithTabControls
                { focusAndSelect = FocusAndSelectItem
                , selected = model.selected
                , tabControlListStyles = Tuple.second settings.controlListStyles
                , tabControlStyles = Tuple.second settings.controlStyles
                , panels = List.map Tuple.second allItems
                , labelledBy = Carousel.LabelledByIdOfVisibleLabel "Items"
                , role = Carousel.Group
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithTabControls"
            ++ Code.record
                [ ( "focusAndSelect", "identity" )
                , ( "selected", Code.string (String.fromInt model.selected) )
                , ( "tabControlListStyles", Tuple.first settings.controlListStyles )
                , ( "tabControlStyles", Tuple.first settings.controlStyles )
                , ( "panels", Code.listMultiline (List.map Tuple.first allItems) 2 )
                , ( "labelledBy", Code.fromModule moduleName "LabelledByIdOfVisibleLabel " ++ Code.string "Items" )
                , ( "role", Code.fromModule moduleName "Group" )
                ]
        , Code.anonymousFunction "{ controls, slides, containerAttributes }"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [ slides, controls ]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, controls ]
    )


toCarouselItem :
    Int
    -> a
    ->
        ( String
        , { id : Int
          , slideHtml : Html msg
          , tabControlHtml : Html Never
          , idString : String
          }
        )
toCarouselItem id _ =
    let
        idString =
            Attributes.safeIdWithPrefix "slide" <| String.fromInt id
    in
    ( Code.recordMultiline
        [ ( "id", Code.string (String.fromInt id) )
        , ( "idString", Code.string "idString" )
        , ( "tabControlHtml", "Html.text " ++ Code.string (String.fromInt (id + 1)) )
        , ( "slideHtml", "Html.text " ++ Code.string (String.fromInt (id + 1) ++ " slide") )
        ]
        2
    , { id = id
      , idString = idString
      , tabControlHtml = Html.text (String.fromInt (id + 1))
      , slideHtml = Html.text (String.fromInt (id + 1) ++ " slide")
      }
    )
