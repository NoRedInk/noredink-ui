module Examples.ClickableText exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


version : Int
version =
    4


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ ClickableText.link "Small"
            [ ClickableText.icon UiIcon.link
            , ClickableText.small
            , ClickableText.custom [ Key.tabbable False ]
            ]
        , ClickableText.link "Medium"
            [ ClickableText.icon UiIcon.link
            , ClickableText.medium
            , ClickableText.custom [ Key.tabbable False ]
            ]
        , ClickableText.link "Large"
            [ ClickableText.icon UiIcon.link
            , ClickableText.large
            , ClickableText.custom [ Key.tabbable False ]
            ]
        ]
    , about = []
    , view = \ellieLinkConfig state -> [ viewExamples ellieLinkConfig state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
    }


moduleName : String
moduleName =
    "ClickableText"


{-| -}
type State
    = State (Control (Settings Msg))


{-| -}
init : State
init =
    Control.record Settings
        |> Control.field "label" (Control.string "Clickable Text")
        |> Control.field ""
            (Control.list
                |> ControlExtra.listItems "Icons"
                    (Control.list
                        |> CommonControls.icon moduleName ClickableText.icon
                        |> CommonControls.rightIcon moduleName ClickableText.rightIcon
                        |> ControlExtra.optionalBoolListItem "hideIconForMobile"
                            ( "ClickableText.hideIconForMobile", ClickableText.hideIconForMobile )
                    )
                |> ControlExtra.listItems "State & Type"
                    (Control.list
                        |> ControlExtra.optionalBoolListItem "disabled"
                            ( "ClickableText.disabled True", ClickableText.disabled True )
                        |> ControlExtra.optionalBoolListItem "submit (button only)"
                            ( "ClickableText.submit", ClickableText.submit )
                        |> ControlExtra.optionalBoolListItem "opensModal (button only)"
                            ( "ClickableText.opensModal", ClickableText.opensModal )
                    )
                |> ControlExtra.listItems "CSS & Extra style options"
                    (Control.list
                        |> ControlExtra.optionalBoolListItem "appearsInline"
                            ( "ClickableText.appearsInline", ClickableText.appearsInline )
                        |> ControlExtra.optionalBoolListItem "hideTextForMobile"
                            ( "ClickableText.hideTextForMobile", ClickableText.hideTextForMobile )
                        |> CommonControls.css
                            { moduleName = moduleName
                            , use = ClickableText.css
                            }
                        |> CommonControls.mobileCss
                            { moduleName = moduleName
                            , use = ClickableText.mobileCss
                            }
                        |> CommonControls.quizEngineMobileCss
                            { moduleName = moduleName
                            , use = ClickableText.quizEngineMobileCss
                            }
                        |> CommonControls.notMobileCss
                            { moduleName = moduleName
                            , use = ClickableText.notMobileCss
                            }
                    )
            )
        |> State


type alias Settings msg =
    { label : String
    , attributes : List ( String, ClickableText.Attribute msg )
    }


{-| -}
type Msg
    = SetState (Control (Settings Msg))
    | ShowItWorked String String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState controls ->
            ( State controls, Cmd.none )

        ShowItWorked group message ->
            ( Debug.log group message |> always state, Cmd.none )



-- INTERNAL


viewExamples : EllieLink.Config -> State -> Html Msg
viewExamples ellieLinkConfig (State control) =
    let
        settings =
            Control.currentValue control
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetState
        , settings = control
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \{ label, attributes } ->
                let
                    toCode fName =
                        Code.fromModule moduleName fName
                            ++ " "
                            ++ Code.string label
                            ++ Code.listMultiline (List.map Tuple.first attributes) 1
                in
                [ { sectionName = "Button"
                  , code = toCode "button"
                  }
                , { sectionName = "Link"
                  , code = toCode "link"
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable Example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.minHeight (Css.px 150)
            ]
        ]
        [ ClickableText.button settings.label
            (ClickableText.onClick (ShowItWorked moduleName "customizable example")
                :: List.map Tuple.second settings.attributes
            )
        ]
    , Heading.h2
        [ Heading.plaintext "Set Sizes Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , sizeExamples settings
    , Heading.h2
        [ Heading.plaintext "Inline ClickableText Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Text.caption [ Text.markdown "Be sure to add the `appearsInline` property if the `ClickableText` appears inline. Otherwise, your styles will not match up nicely. You should not add an explicit size." ]
    , Table.view []
        [ Table.custom
            { header = text "Displayed"
            , view = \( view, name ) -> view (inlineExample name)
            , width = Css.pct 70
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Pattern"
            , view =
                \( _, textName ) ->
                    code [ css [ Css.fontSize (Css.px 12) ] ]
                        [ text
                            (Code.fromModule "Text"
                                textName
                                ++ Code.listMultiline
                                    [ Code.fromModule "Text" "html"
                                        ++ Code.listMultiline
                                            [ "…"
                                            , Code.fromModule moduleName
                                                "link "
                                                ++ Code.string "internal links"
                                                ++ Code.listMultiline
                                                    [ Code.fromModule moduleName "appearsInline"
                                                    , Code.fromModule moduleName "href " ++ Code.string "/"
                                                    ]
                                                    3
                                            , "…"
                                            ]
                                            2
                                    ]
                                    1
                            )
                        ]
            , width = Css.px 10
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    , Css.whiteSpace Css.preWrap
                    ]
            , sort = Nothing
            }
        ]
        [ ( Text.caption, "caption" )
        , ( Text.smallBody, "smallBody" )
        , ( Text.smallBodyGray, "smallBodyGray" )
        , ( Text.mediumBody, "mediumBody" )
        , ( Text.mediumBodyGray, "mediumBodyGray" )
        , ( Text.ugSmallBody, "ugSmallBody" )
        , ( Text.ugMediumBody, "ugMediumBody" )
        ]
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]
    ]
        |> div []


inlineExample : String -> List (Text.Attribute Msg)
inlineExample textSizeName =
    [ Text.html
        [ text "Sometimes, we'll want our "
        , ClickableText.link "internal links"
            [ ClickableText.appearsInline
            , ClickableText.href "/"
            ]
        , text ", "
        , ClickableText.link "external links"
            [ ClickableText.appearsInline
            , ClickableText.linkExternal "https://www.google.com/search?q=puppies"
            ]
        , text ", "
        , ClickableText.button "buttons"
            [ ClickableText.appearsInline
            , ClickableText.onClick (ShowItWorked moduleName "in-line button")
            ]
        , text ", "
        , ClickableText.button "ClickableTexts with left icons"
            [ ClickableText.appearsInline
            , ClickableText.onClick (ShowItWorked moduleName "in-line button")
            , ClickableText.icon UiIcon.starFilled
            ]
        , text ", "
        , ClickableText.button "ClickableTexts with right icons"
            [ ClickableText.appearsInline
            , ClickableText.onClick (ShowItWorked moduleName "in-line button")
            , ClickableText.rightIcon UiIcon.starFilled
            ]
        , text ", and "
        , ClickableText.button "ClickableTexts with left and right icons"
            [ ClickableText.appearsInline
            , ClickableText.onClick (ShowItWorked moduleName "in-line button")
            , ClickableText.icon UiIcon.starFilled
            , ClickableText.rightIcon UiIcon.starFilled
            ]
        , text (" to show up in-line with " ++ textSizeName ++ " content.")
        ]
    ]


sizes : List ( ClickableText.Attribute msg, String )
sizes =
    [ ( ClickableText.small, "small" )
    , ( ClickableText.medium, "medium" )
    , ( ClickableText.large, "large" )
    , ( ClickableText.modal, "modal" )
    ]


sizeExamples : Settings Msg -> Html Msg
sizeExamples settings =
    Table.view []
        [ Table.custom
            { header = text "Size"
            , view =
                \{ sizeName } ->
                    code [ css [ Css.fontSize (Css.px 12) ] ]
                        [ text (Code.fromModule "ClickableText" sizeName)
                        ]
            , width = Css.px 10
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Button"
            , view = \{ size } -> sizeExamplesFor ClickableText.button [ size ]
            , width = Css.px 10
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Link"
            , view = \{ size } -> sizeExamplesFor ClickableText.link [ size ]
            , width = Css.px 10
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "External link"
            , view = \{ size } -> sizeExamplesFor ClickableText.link [ size, ClickableText.linkExternal "google.com" ]
            , width = Css.px 10
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    ]
            , sort = Nothing
            }
        ]
        (List.map (\( size, sizeName ) -> { size = size, sizeName = sizeName }) sizes)
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]


sizeExamplesFor : (String -> List (ClickableText.Attribute msg) -> Html msg) -> List (ClickableText.Attribute msg) -> Html msg
sizeExamplesFor render attrs =
    ul [ css [ Css.margin Css.zero ] ]
        [ li [] [ render "No icons" attrs ]
        , li [] [ render "Left icon" (attrs ++ [ ClickableText.icon UiIcon.arrowLeft ]) ]
        , li [] [ render "Right icon" (attrs ++ [ ClickableText.rightIcon UiIcon.arrowRight ]) ]
        , li []
            [ render "Disabled w/icons"
                (attrs
                    ++ [ ClickableText.icon UiIcon.arrowLeft
                       , ClickableText.rightIcon UiIcon.arrowRight
                       , ClickableText.disabled True
                       ]
                )
            ]
        ]
