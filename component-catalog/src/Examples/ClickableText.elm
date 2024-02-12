module Examples.ClickableText exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
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
        [ Heading.plaintext "Customizable Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , buttons settings
    , Heading.h2
        [ Heading.plaintext "Inline ClickableText Examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Text.caption (inlineExample "Text.caption")
    , Text.smallBody (inlineExample "Text.smallBody")
    , Text.smallBodyGray (inlineExample "Text.smallBodyGray")
    , Text.mediumBody (inlineExample "Text.mediumBody")
    , Text.mediumBodyGray (inlineExample "Text.mediumBodyGray")
    , Text.ugSmallBody (inlineExample "Text.ugSmallBody")
    , Text.ugMediumBody (inlineExample "Text.ugMediumBody")
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


buttons : Settings Msg -> Html Msg
buttons settings =
    let
        sizeRow label render =
            row label (List.map render sizes)
    in
    table []
        [ tr [] (td [] [] :: List.map (\( size, sizeLabel ) -> th [] [ text sizeLabel ]) sizes)
        , sizeRow ".link"
            (\( size, sizeLabel ) ->
                ClickableText.link settings.label
                    (size :: List.map Tuple.second settings.attributes)
                    |> exampleCell
            )
        , sizeRow ".button"
            (\( size, sizeLabel ) ->
                ClickableText.button settings.label
                    (size
                        :: ClickableText.onClick (ShowItWorked moduleName sizeLabel)
                        :: List.map Tuple.second settings.attributes
                    )
                    |> exampleCell
            )
        ]
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]


row : String -> List (Html msg) -> Html msg
row label tds =
    tr [] (th [] [ td [] [ text label ] ] :: tds)


exampleCell : Html msg -> Html msg
exampleCell view =
    td
        [ css
            [ verticalAlign middle
            , Css.width (Css.px 200)
            , Css.borderTop3 (Css.px 1) Css.solid Colors.gray85
            ]
        ]
        [ view ]
