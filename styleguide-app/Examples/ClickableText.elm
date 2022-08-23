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
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
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
        |> Control.field "attributes"
            (ControlExtra.list
                |> CommonControls.icon moduleName ClickableText.icon
                |> ControlExtra.optionalBoolListItem "hideIconForMobile"
                    ( "ClickableText.hideIconForMobile", ClickableText.hideIconForMobile )
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

        clickableAttributes =
            List.map Tuple.second settings.attributes
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetState
        , settings = control
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , toExampleCode =
            \{ label, attributes } ->
                let
                    toCode fName =
                        "ClickableText."
                            ++ fName
                            ++ " \""
                            ++ label
                            ++ "\"\n\t"
                            ++ Code.list (List.map Tuple.first attributes)
                in
                [ { sectionName = "Button"
                  , code = toCode "button"
                  }
                , { sectionName = "Link"
                  , code = toCode "link"
                  }
                ]
        }
    , buttons settings
    , Text.smallBody
        [ Text.html
            [ text "Sometimes, we'll want our clickable links: "
            , ClickableText.link settings.label
                (ClickableText.small :: clickableAttributes)
            , text " and clickable buttons: "
            , ClickableText.button settings.label
                (ClickableText.small
                    :: ClickableText.onClick (ShowItWorked moduleName "in-line button")
                    :: clickableAttributes
                )
            , text " to show up in-line."
            ]
        ]
    ]
        |> div []


sizes : List ( ClickableText.Attribute msg, String )
sizes =
    [ ( ClickableText.small, "small" )
    , ( ClickableText.medium, "medium" )
    , ( ClickableText.large, "large" )
    ]


buttons : Settings Msg -> Html Msg
buttons settings =
    let
        sizeRow label render =
            row label (List.map render sizes)
    in
    table []
        [ sizeRow "" (\( size, sizeLabel ) -> th [] [ text sizeLabel ])
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


row : String -> List (Html msg) -> Html msg
row label tds =
    tr [] (th [] [ td [] [ text label ] ] :: tds)


exampleCell : Html msg -> Html msg
exampleCell view =
    td [ css [ verticalAlign middle, Css.width (Css.px 200) ] ] [ view ]
