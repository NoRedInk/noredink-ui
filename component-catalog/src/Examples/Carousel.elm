module Examples.Carousel exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Accessibility.Styled.Live as Live
import Accessibility.Styled.Style as Style
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.ClickableSvg
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Carousel.V2 as Carousel exposing (Role(..))
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.Attributes.V2 as Attributes
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Routes
import Task


type alias State =
    { selected : Int
    , settings : Control Settings
    , tip : Tip
    , testimonial : Testimonial
    , package : Package
    , announcement : Maybe String
    }


init : State
init =
    { selected = 0
    , settings = initSettings
    , tip = AvoidWhiteAfterLaborDay
    , testimonial = GreatService
    , package = FreeTrial
    , announcement = Nothing
    }


type alias Settings =
    { items : Int
    , carouselType : CarouselType
    , role : ( String, Role )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "items" (Control.int 4)
        |> Control.field "carouselType" controlCarouselType
        |> Control.field "role" controlRole


type CarouselType
    = Tabs
    | PrevNext
    | Combined


controlCarouselType : Control CarouselType
controlCarouselType =
    Control.choice
        [ ( "viewWithPreviousAndNextControls", Control.value PrevNext )
        , ( "viewWithTabControls", Control.value Tabs )
        , ( "viewWithCombinedControls", Control.value Combined )
        ]


controlRole : Control ( String, Role )
controlRole =
    CommonControls.choice moduleName
        [ ( "Group", Group ), ( "Region", Region ) ]


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


type Msg
    = Select { select : Int, announce : Maybe String, focus : Maybe String }
    | SelectTip { select : Tip, announce : String }
    | SelectTestimonial { select : Testimonial, focus : Maybe String }
    | SelectPackage { select : Package, announce : Maybe String, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        Select { select, announce, focus } ->
            ( { model
                | selected = select
                , announcement =
                    case announce of
                        Just announcement ->
                            Just announcement

                        Nothing ->
                            model.announcement
              }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        Focused error ->
            ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Cmd.none )

        SelectTip { select, announce } ->
            ( { model
                | tip = select
                , announcement = Just announce
              }
            , Cmd.none
            )

        SelectTestimonial { select, focus } ->
            ( { model | testimonial = select }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        SelectPackage { select, announce, focus } ->
            ( { model
                | package = select
                , announcement =
                    case announce of
                        Just announcement ->
                            Just announcement

                        Nothing ->
                            model.announcement
              }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
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
    , categories = [ Layout, Category.Tabs ]
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
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Carousel component to give Component Catalog users a sense of what the
          -- component might look like
          Html.div [ css [ Css.position Css.relative ] ]
            [ Html.div
                [ css
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.spaceBetween
                    , Css.marginBottom (Css.px 10)
                    ]
                ]
                [ ClickableSvg.button "Previous" UiIcon.arrowLeft []
                , Html.text "First Slide"
                , ClickableSvg.button "Next" UiIcon.arrowRight []
                ]
            , Html.div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.property "gap" "5px"
                    ]
                ]
                [ Html.div [ Attributes.css (controlStyles True) ] [ Html.text "1" ]
                , Html.div [ Attributes.css (controlStyles False) ] [ Html.text "2" ]
                , Html.div [ Attributes.css (controlStyles False) ] [ Html.text "3" ]
                ]
            ]
        ]
    , about =
        [ Text.smallBody
            [ Text.html
                [ text "Watch "
                , ClickableText.link "Tessa's Carousel demo"
                    [ ClickableText.linkExternal "https://www.dropbox.com/scl/fi/ilb105gyk6adcpt72f5vf/Carousel-demo.mov?rlkey=if2o9dqgfq1ucjkp9tput1rez&dl=0"
                    , ClickableText.appearsInline
                    ]
                , text " for a brief intro on the Carousel component and its usage."
                ]
            ]
        ]
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings

                ( code_, view ) =
                    case settings.carouselType of
                        Tabs ->
                            viewCustomizableWithTabControls settings model.selected

                        PrevNext ->
                            viewCustomizableWithPreviousAndNextControls settings model.selected

                        Combined ->
                            viewCustomizableWithCombinedControls settings model.selected
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = Just "Program () Model Msg"
                , extraCode =
                    [ "import Browser"
                    , Code.newline
                    , "type alias Model = {}"
                    , Code.varWithTypeAnnotation "init" "Model" "{}"
                    , Code.unionType "Msg"
                        [ "AnnounceAndSelect { select : Int, announce : String }"
                        , "FocusAndSelect { select : Int, focus : Maybe String }"
                        , "FocusSelectAndAnnounce { select : Int, focus : Maybe String, announce : Maybe String }"
                        ]
                    , Code.funcWithType "update"
                        "Msg -> Model -> ( Model, Cmd Msg )"
                        "msg model"
                        (Code.caseExpression "msg"
                            [ ( "_", Code.tuple "model" "Cmd.none" )
                            ]
                            1
                        )
                    ]
                , renderExample =
                    \viewCode ->
                        Code.browserElement
                            { init = Code.always (Code.tuple "init" "Cmd.none")
                            , view = "view >> toUnstyled"
                            , update = "update"
                            , subscriptions = Code.always "Sub.none"
                            }
                            ++ Code.newlines
                            ++ Code.funcWithType "view"
                                "Model -> Html Msg"
                                "model"
                                viewCode
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code = code_
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , div
                [ css
                    [ Css.minHeight (Css.px 200)
                    , Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.alignItems Css.center
                    ]
                ]
                [ view ]
            , Heading.h2
                [ Heading.plaintext "Usage examples"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Table.view []
                [ Table.rowHeader
                    { header = text (moduleName ++ " view name")
                    , view = \{ viewName } -> code [] [ text viewName ]
                    , width = Css.zero
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.middle
                            , Css.textAlign Css.left
                            , Css.fontWeight Css.normal
                            ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example with added styles"
                    , view = .example
                    , width = Css.pct 40
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.middle
                            , Css.lineHeight (Css.num 2)
                            ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Usage"
                    , view = .about >> div []
                    , width = Css.px 200
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.middle
                            ]
                    , sort = Nothing
                    }
                ]
                [ { viewName = "viewWithPreviousAndNextControls"
                  , example = viewTips model.tip
                  , about =
                        [ Text.smallBody
                            [ Text.html [ text "Use this view when you want to control slides with \"next\" and \"previous\" buttons." ]
                            ]
                        , Text.smallBody [ Text.html [ text "You can style the slides however you want." ] ]
                        , Text.smallBody
                            [ Text.html
                                [ text "The buttons are "
                                , ClickableText.link "ClickableSvg"
                                    [ ClickableText.href (Routes.exampleHref Examples.ClickableSvg.example)
                                    , ClickableText.appearsInline
                                    ]
                                , text "s, and can be styled/modified using ClickableSvg Attributes."
                                ]
                            ]
                        ]
                  }
                , { viewName = "viewWithTabControls"
                  , example = viewTestimonials model.testimonial
                  , about =
                        [ Text.smallBody
                            [ Text.html [ text "Use this view when you want to control the slides with direct jump-to-slide-style controls." ]
                            ]
                        , Text.smallBody [ Text.html [ text "You can style the slides however you want." ] ]
                        , Text.smallBody [ Text.html [ text "The controls require custom styling, as unfortunately they don't currently compose with any other noredink-ui component." ] ]
                        ]
                  }
                , { viewName = "viewWithCombinedControls"
                  , example = viewPackages model.package
                  , about =
                        [ Text.smallBody
                            [ Text.html [ text "Use this view when you want to control the slides with \"next\" and \"previous\" buttons AND direct jump-to-slide-style controls." ]
                            ]
                        ]
                  }
                ]
            , Heading.h2
                [ Heading.plaintext "Assistive Technology Announcement Center"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Text.smallBody
                [ Text.html
                    [ text "NRI employees can learn more about the real ATAC in "
                    , ClickableText.link "Assistive Technology Announcement Center (“ATAC”)"
                        [ ClickableText.appearsInline
                        , ClickableText.linkExternal "https://paper.dropbox.com/doc/Assistive-Technology-Announcement-Center-ATAC--B_GuqwWltzU432ueq7p6Z42mAg-bOnmcnzOj631NRls1IBe3"
                        ]
                    ]
                ]
            , div
                [ Live.polite
                , Live.atomic True
                ]
                [ viewJust
                    (\announcement ->
                        Text.mediumBody [ Text.plaintext announcement ]
                    )
                    model.announcement
                ]
            ]
    }


viewCustomizableWithPreviousAndNextControls : Settings -> Int -> ( String, Html Msg )
viewCustomizableWithPreviousAndNextControls settings selected =
    let
        allItems =
            settings.items
                |> indicesForItemCount
                |> List.map toNonTabbedCarouselItem

        { viewPreviousButton, viewNextButton, slides, containerAttributes } =
            Carousel.viewWithPreviousAndNextControls
                { selected = selected
                , slides = List.map Tuple.second allItems
                , previousButton =
                    { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                , nextButton =
                    { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                , name = "Items"
                , visibleLabelId = Nothing
                , role = Tuple.second settings.role
                , announceAndSelect =
                    \{ select, announce } ->
                        Select
                            { select = select
                            , announce = Just announce
                            , focus = Nothing
                            }
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithPreviousAndNextControls"
            ++ Code.recordMultiline
                [ ( "selected", Code.int selected )
                , ( "slides", Code.listOfRecordsMultiline (List.take 2 <| List.map Tuple.first allItems) 2 )
                , ( "previousButton"
                  , Code.recordMultiline
                        [ ( "name", Code.string "Previous" )
                        , ( "icon", "UiIcon.arrowLeft" )
                        , ( "attributes", Code.list [] )
                        ]
                        2
                  )
                , ( "nextButton"
                  , Code.recordMultiline
                        [ ( "name", Code.string "Previous" )
                        , ( "icon", "UiIcon.arrowRight" )
                        , ( "attributes", Code.list [] )
                        ]
                        2
                  )
                , ( "name", Code.string "Items" )
                , ( "visibleLabelId", Code.maybe Nothing )
                , ( "role", Tuple.first settings.role )
                , ( "announceAndSelect", "AnnounceAndSelect" )
                ]
                1
        , Code.anonymousFunction "{viewPreviousButton, viewNextButton, slides, containerAttributes}"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [ slides, viewPreviousButton, viewNextButton ]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, viewPreviousButton, viewNextButton ]
    )


viewCustomizableWithCombinedControls : Settings -> Int -> ( String, Html Msg )
viewCustomizableWithCombinedControls settings selected =
    let
        allItems =
            settings.items
                |> indicesForItemCount
                |> List.map toCombinedCarouselItem

        { tabs, slides, viewPreviousButton, viewNextButton, containerAttributes } =
            Carousel.viewWithCombinedControls
                { selected = selected
                , slides = List.map Tuple.second allItems
                , tabStyles = \_ -> []
                , tabListStyles = []
                , previousButton =
                    { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                , nextButton =
                    { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                , role = Tuple.second settings.role
                , name = "Items"
                , visibleLabelId = Nothing
                , select = Select
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithCombinedControls"
            ++ Code.record
                [ ( "selected", Code.int selected )
                , ( "slides", Code.listOfRecordsMultiline (List.take 2 <| List.map Tuple.first allItems) 2 )
                , ( "tabStyles", Code.anonymousFunction "_" (Code.list []) )
                , ( "tabListStyles", Code.list [] )
                , ( "previousButton"
                  , Code.recordMultiline
                        [ ( "name", Code.string "Previous" )
                        , ( "icon", "UiIcon.arrowLeft" )
                        , ( "attributes", Code.list [] )
                        ]
                        2
                  )
                , ( "nextButton"
                  , Code.recordMultiline
                        [ ( "name", Code.string "Previous" )
                        , ( "icon", "UiIcon.arrowRight" )
                        , ( "attributes", Code.list [] )
                        ]
                        2
                  )
                , ( "role", Tuple.first settings.role )
                , ( "name", Code.string "Items" )
                , ( "visibleLabelId", Code.maybe Nothing )
                , ( "select", "FocusSelectAndAnnounce" )
                ]
        , Code.anonymousFunction "{tabs, slides, viewPreviousButton, viewNextButton, containerAttributes}"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [slides, tabs, viewPreviousButton, viewNextButton]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, tabs, viewPreviousButton, viewNextButton ]
    )


viewCustomizableWithTabControls : Settings -> Int -> ( String, Html Msg )
viewCustomizableWithTabControls settings selected =
    let
        allItems =
            settings.items
                |> indicesForItemCount
                |> List.map toTabbedCarouselItem

        { tabs, slides, containerAttributes } =
            Carousel.viewWithTabControls
                { selected = selected
                , slides = List.map Tuple.second allItems
                , tabStyles = \_ -> []
                , tabListStyles = []
                , role = Tuple.second settings.role
                , name = "Items"
                , visibleLabelId = Nothing
                , focusAndSelect =
                    \{ select, focus } ->
                        Select
                            { select = select
                            , announce = Nothing
                            , focus = focus
                            }
                }
    in
    ( Code.pipelineMultiline
        [ Code.fromModule moduleName "viewWithTabControls"
            ++ Code.record
                [ ( "selected", Code.int selected )
                , ( "slides", Code.listOfRecordsMultiline (List.take 2 <| List.map Tuple.first allItems) 2 )
                , ( "tabStyles", "(\\_ -> [])" )
                , ( "tabListStyles", Code.list [] )
                , ( "role", Tuple.first settings.role )
                , ( "name", Code.string "Items" )
                , ( "visibleLabelId", Code.maybe Nothing )
                , ( "focusAndSelect", "FocusAndSelect" )
                ]
        , Code.anonymousFunction "{ tabs, slides, containerAttributes }"
            (Code.newlineWithIndent 2
                ++ "section containerAttributes [ slides, tabs ]"
            )
        ]
        0
    , Html.div containerAttributes [ slides, tabs ]
    )


indicesForItemCount : Int -> List Int
indicesForItemCount itemCount =
    List.range 0 (max 0 (itemCount - 1))


toNonTabbedCarouselItem :
    Int
    ->
        ( List ( String, String )
        , { id : Int
          , idString : String
          , name : String
          , visibleLabelId : Maybe String
          , slideView : Html msg
          }
        )
toNonTabbedCarouselItem id =
    let
        idString =
            Attributes.safeIdWithPrefix "slide" <| String.fromInt id

        humanizedId =
            String.fromInt (id + 1)
    in
    ( [ ( "id", Code.int id )
      , ( "idString", Code.string (String.fromInt id) )
      , ( "name", Code.string ("Slide " ++ humanizedId) )
      , ( "visibleLabelId", Code.maybe Nothing )
      , ( "slideView", "text " ++ Code.string ("Contents for slide " ++ humanizedId) )
      ]
    , { id = id
      , idString = idString
      , name = "Slide " ++ humanizedId
      , visibleLabelId = Nothing
      , slideView = Html.text ("Contents for slide " ++ humanizedId)
      }
    )


toTabbedCarouselItem :
    Int
    ->
        ( List ( String, String )
        , { id : Int
          , idString : String
          , slideView : Html msg
          , tabView : Html Never
          , tabAttributes : List (Html.Attribute msg)
          }
        )
toTabbedCarouselItem id =
    let
        idString =
            Attributes.safeIdWithPrefix "slide" <| String.fromInt id

        humanizedId =
            String.fromInt (id + 1)
    in
    ( [ ( "id", Code.int id )
      , ( "idString", Code.string (String.fromInt id) )
      , ( "slideView", "text " ++ Code.string ("Contents for slide " ++ humanizedId) )
      , ( "tabView", "text " ++ Code.string ("Slide " ++ humanizedId) )
      , ( "tabAttributes", Code.list [] )
      ]
    , { id = id
      , idString = idString
      , slideView = Html.text ("Contents for slide " ++ humanizedId)
      , tabView = Html.text ("Slide " ++ humanizedId)
      , tabAttributes = []
      }
    )


toCombinedCarouselItem :
    Int
    ->
        ( List ( String, String )
        , { id : Int
          , idString : String
          , name : String
          , visibleLabelId : Maybe String
          , slideView : Html msg
          , tabView : Html Never
          , tabAttributes : List (Html.Attribute msg)
          }
        )
toCombinedCarouselItem id =
    let
        idString =
            Attributes.safeIdWithPrefix "slide" <| String.fromInt id

        humanizedId =
            String.fromInt (id + 1)
    in
    ( [ ( "id", Code.int id )
      , ( "idString", Code.string (String.fromInt id) )
      , ( "name", Code.string ("Slide " ++ humanizedId) )
      , ( "visibleLabelId", Code.maybe Nothing )
      , ( "slideView", "text " ++ Code.string ("Contents for slide " ++ humanizedId) )
      , ( "tabView", "text " ++ Code.string ("Slide " ++ humanizedId) )
      , ( "tabAttributes", Code.list [] )
      ]
    , { id = id
      , idString = idString
      , name = "Slide " ++ humanizedId
      , visibleLabelId = Nothing
      , slideView = Html.text ("Contents for slide " ++ humanizedId)
      , tabView = Html.text ("Slide " ++ humanizedId)
      , tabAttributes = []
      }
    )


viewTips : Tip -> Html Msg
viewTips selected =
    let
        { viewPreviousButton, viewNextButton, slides, containerAttributes } =
            Carousel.viewWithPreviousAndNextControls
                { selected = selected
                , slides =
                    [ { id = AvoidWhiteAfterLaborDay
                      , idString = "avoid-white-after-labor-day"
                      , name = "Avoid White After Labor Day"
                      , visibleLabelId = Nothing
                      , slideView = text "Avoid wearing white after Labor Day"
                      }
                    , { id = AvoidNavyAndBlack
                      , idString = "avoid-navy-and-black"
                      , name = "Avoid pairing navy and black"
                      , visibleLabelId = Nothing
                      , slideView = text "Avoid pairing navy and black"
                      }
                    , { id = TailorOffTheShelfClothes
                      , idString = "tailor-off-the-shelf-clothes"
                      , name = "Tailor off the shelf clothes"
                      , visibleLabelId = Nothing
                      , slideView = text "Tailor off the shelf clothes"
                      }
                    ]
                , previousButton =
                    { attributes = [ ClickableSvg.small, ClickableSvg.withBorder ]
                    , icon = UiIcon.arrowTop
                    , name = "Previous"
                    }
                , nextButton =
                    { attributes = [ ClickableSvg.small, ClickableSvg.withBorder ]
                    , icon = UiIcon.arrowDown
                    , name = "Next"
                    }
                , name = "Tips"
                , visibleLabelId = Nothing
                , role = Carousel.Group
                , announceAndSelect = SelectTip
                }
    in
    Container.view
        [ Container.custom containerAttributes
        , Container.css
            [ displayFlex
            , alignItems center
            , position relative
            , marginRight (px 20)
            , maxWidth (px 210)
            ]
        , Container.html
            [ slides
            , div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , property "gap" "4px"
                    , right (px -20)
                    , position absolute
                    ]
                ]
                [ viewPreviousButton
                , viewNextButton
                ]
            ]
        ]


type Tip
    = AvoidWhiteAfterLaborDay
    | AvoidNavyAndBlack
    | TailorOffTheShelfClothes


viewTestimonials : Testimonial -> Html Msg
viewTestimonials selected =
    let
        { tabs, slides, containerAttributes } =
            Carousel.viewWithTabControls
                { selected = selected
                , slides =
                    [ { id = GreatService
                      , idString = "great-service"
                      , slideView = text "Great service!"
                      , tabView = span Style.invisible [ text "Testimonial 1" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    , { id = GreatProduct
                      , idString = "great-product"
                      , slideView = text "Great product!"
                      , tabView = span Style.invisible [ text "Testimonial 2" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    , { id = GreatMission
                      , idString = "great-mission"
                      , slideView = text "Great mission!"
                      , tabView = span Style.invisible [ text "Testimonial 3" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    ]
                , tabStyles = tabStyles
                , tabListStyles = tabListStyles
                , role = Carousel.Group
                , name = "Testimonials"
                , visibleLabelId = Nothing
                , focusAndSelect = SelectTestimonial
                }
    in
    Container.view
        [ Container.custom containerAttributes
        , Container.css [ maxWidth (px 210) ]
        , Container.html
            [ slides
            , tabs
            ]
        ]


type Testimonial
    = GreatService
    | GreatProduct
    | GreatMission


viewPackages : Package -> Html Msg
viewPackages selected =
    let
        { tabs, slides, viewPreviousButton, viewNextButton, containerAttributes } =
            Carousel.viewWithCombinedControls
                { selected = selected
                , slides =
                    [ { id = FreeTrial
                      , idString = "free-trial"
                      , name = "Free trial"
                      , visibleLabelId = Nothing
                      , slideView = text "Free trial"
                      , tabView = span Style.invisible [ text "Free trial" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    , { id = DeveloperTier
                      , idString = "developer-tier"
                      , name = "Developer Tier"
                      , visibleLabelId = Nothing
                      , slideView = text "Developer Tier"
                      , tabView = span Style.invisible [ text "Developer tier" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    , { id = EnterpriseTier
                      , idString = "enterprise-tier"
                      , name = "Enterprise Tier"
                      , visibleLabelId = Nothing
                      , slideView = text "Enterprise Tier"
                      , tabView = span Style.invisible [ text "Enterprise tier" ]
                      , tabAttributes = [ class FocusRing.customClass ]
                      }
                    ]
                , tabStyles = tabStyles
                , tabListStyles = tabListStyles
                , previousButton =
                    { attributes =
                        [ ClickableSvg.withBorder
                        , ClickableSvg.css
                            [ position absolute
                            , left (px -15)
                            , top (px 20)
                            ]
                        ]
                    , icon = UiIcon.arrowLeft
                    , name = "Previous"
                    }
                , nextButton =
                    { attributes =
                        [ ClickableSvg.withBorder
                        , ClickableSvg.css
                            [ position absolute
                            , right (px -15)
                            , top (px 20)
                            ]
                        ]
                    , icon = UiIcon.arrowRight
                    , name = "Next"
                    }
                , role = Carousel.Group
                , name = "Packages"
                , visibleLabelId = Nothing
                , select = SelectPackage
                }
    in
    Container.view
        [ Container.custom containerAttributes
        , Container.css
            [ maxWidth (px 210)
            , position relative
            , marginLeft (px 15)
            , marginRight (px 15)
            , paddingLeft (px 30)
            , paddingRight (px 30)
            , displayFlex
            , flexDirection column
            , alignItems center
            ]
        , Container.html
            [ div [] [ viewPreviousButton, slides, viewNextButton ]
            , tabs
            ]
        ]


type Package
    = FreeTrial
    | DeveloperTier
    | EnterpriseTier


tabStyles : Bool -> List Style
tabStyles isSelected =
    [ borderRadius (pct 100)
    , overflow hidden
    , padding zero
    , height (px 22)
    , width (px 22)
    , border zero
    , batch <|
        if isSelected then
            [ pseudoClass "focus-visible"
                [ FocusRing.boxShadows []
                , outline zero
                ]
            , backgroundColor Colors.azure
            ]

        else
            [ cursor pointer
            , backgroundColor Colors.gray92
            ]
    ]


tabListStyles : List Style
tabListStyles =
    [ displayFlex
    , property "gap" "15px"
    , justifyContent center
    ]
