module Examples.ClickableText exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import CommonControls
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "ClickableText"
    , version = 3
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
    , view = \state -> [ viewExamples state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
    }


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
                |> ControlExtra.optionalListItemDefaultChecked "icon"
                    (Control.map
                        (\( iconName, icon ) ->
                            ( "ClickableText.icon " ++ iconName, ClickableText.icon icon )
                        )
                        CommonControls.uiIcon
                    )
                |> CommonControls.css
                    { moduleName = "ClickableText"
                    , use = ClickableText.css
                    , default = "border: 2px solid red;"
                    }
                |> CommonControls.mobileCss
                    { moduleName = "ClickableText"
                    , use = ClickableText.mobileCss
                    , default = "[role=img] { display: none !important; }"
                    }
                |> CommonControls.quizEngineMobileCss
                    { moduleName = "ClickableText"
                    , use = ClickableText.quizEngineMobileCss
                    , default = "[role=img] { display: none !important; }"
                    }
                |> CommonControls.notMobileCss
                    { moduleName = "ClickableText"
                    , use = ClickableText.notMobileCss
                    , default = ""
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
            let
                _ =
                    Debug.log group message
            in
            ( state, Cmd.none )



-- INTERNAL


viewExamples : State -> Html Msg
viewExamples (State control) =
    let
        settings =
            Control.currentValue control

        clickableAttributes =
            List.map Tuple.second settings.attributes
    in
    [ ControlView.view
        { update = SetState
        , settings = control
        , toExampleCode =
            \{ label, attributes } ->
                let
                    toCode fName =
                        "ClickableText."
                            ++ fName
                            ++ " \""
                            ++ label
                            ++ "\"\n\t"
                            ++ ControlView.codeFromList attributes
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
                    :: ClickableText.onClick (ShowItWorked "ClickableText" "in-line button")
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
                        :: ClickableText.onClick (ShowItWorked "ClickableText" sizeLabel)
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
