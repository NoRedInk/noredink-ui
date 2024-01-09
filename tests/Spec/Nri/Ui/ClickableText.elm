module Spec.Nri.Ui.ClickableText exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Attributes as Attributes
import Html.Styled exposing (..)
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.UiIcon.V1 as UiIcon
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.ClickableText.V4"
        [ describe "elements" elementTests
        , describe "attributes" attributeTests
        , describe "icon accessibility" iconAccessibilityTests
        , describe "disabled behavior and attributes" disabledStateTests
        ]


elementTests : List Test
elementTests =
    [ test "the `button` type renders as a button element" <|
        \() ->
            programButton []
                |> ensureViewHas [ tag "button" ]
                |> done
    , test "the `link` type renders as an anchor element" <|
        \() ->
            programLink []
                |> ensureViewHas [ tag "a" ]
                |> done
    , test "renders an svg element when an icon is provided" <|
        \() ->
            programButton [ ClickableText.icon UiIcon.arrowLeft ]
                |> ensureViewHas [ tag "svg" ]
                |> done
    , test "renders an svg element when a right icon is provided" <|
        \() ->
            programButton [ ClickableText.rightIcon UiIcon.arrowLeft ]
                |> ensureViewHas [ tag "svg" ]
                |> done
    , test "renders an svg element when an external link is provided" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas [ tag "svg" ]
                |> done
    ]


attributeTests : List Test
attributeTests =
    [ test "a button has the `type` attribute set to `\"button\"`" <|
        \() ->
            programButton []
                |> ensureViewHas [ attribute (Attributes.type_ "button") ]
                |> done
    , test "a button with the `submit` type has the `type` attribute set to `\"submit\"`" <|
        \() ->
            programButton [ ClickableText.submit ]
                |> ensureViewHas [ attribute (Attributes.type_ "submit") ]
                |> done
    , test "a link has the `href` attribute set to the provided value" <|
        \() ->
            programLink [ ClickableText.href "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.href "https://example.com")
                    ]
                |> done
    , test "an external link has the `href` attribute set to the provided value" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.href "https://example.com")
                    ]
                |> done
    , test "a default link has the `target` attribute set to `\"_self\"`" <|
        \() ->
            programLink [ ClickableText.href "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.target "_self")
                    ]
                |> done
    , test "an external link has the `target` attribute set to `\"_blank\"`" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.target "_blank")
                    ]
                |> done
    , test "an external link has the `rel` attribute set to `\"noopener noreferrer\"`" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.rel "noopener noreferrer")
                    ]
                |> done
    ]


iconAccessibilityTests : List Test
iconAccessibilityTests =
    [ test "the icon has the `aria-hidden` attribute set to `\"true\"`" <|
        \() ->
            programButton [ ClickableText.icon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute (Aria.hidden True)
                    ]
                |> done
    , test "the icon has the `role` attribute set to `\"img\"`" <|
        \() ->
            programButton [ ClickableText.icon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute Role.img
                    ]
                |> done
    , test "the icon has the `focusable` attribute set to `\"false\"`" <|
        \() ->
            programButton [ ClickableText.icon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute (Attributes.attribute "focusable" "false")
                    ]
                |> done
    , test "the right icon has the `aria-hidden` attribute set to `\"true\"`" <|
        \() ->
            programButton [ ClickableText.rightIcon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute (Aria.hidden True)
                    ]
                |> done
    , test "the right icon has the `role` attribute set to `\"img\"`" <|
        \() ->
            programButton [ ClickableText.rightIcon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute Role.img
                    ]
                |> done
    , test "the right icon has the `focusable` attribute set to `\"false\"`" <|
        \() ->
            programButton [ ClickableText.rightIcon UiIcon.arrowLeft ]
                |> ensureViewHas
                    [ attribute (Attributes.attribute "focusable" "false")
                    ]
                |> done
    , test "the `aria-hidden` attribute is not present for an external link icon" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHasNot
                    [ attribute (Aria.hidden True)
                    ]
                |> done
    , test "the external link icon has the `role` attribute set to `\"img\"`" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute Role.img
                    ]
                |> done
    , test "the external link icon has the `focusable` attribute set to `\"false\"`" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.attribute "focusable" "false")
                    ]
                |> done
    , test "the external link icon has the `title` tag set to `\"Opens in a new tab\"`" <|
        \() ->
            programLink [ ClickableText.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ tag "title"
                    , containing [ Selector.text "Opens in a new tab" ]
                    ]
                |> done
    ]


disabledStateTests : List Test
disabledStateTests =
    [ test "the `aria-disabled` attribute is not present for an enabled ClickableText" <|
        \() ->
            programButton []
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "the `aria-disabled` attribute is present and set to `\"true\"` for a disabled ClickableText" <|
        \() ->
            programButton [ ClickableText.disabled True ]
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when enabled" <|
        \() ->
            programButton
                [ ClickableText.onClick NoOp
                ]
                |> clickOnButton
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            programButton
                [ ClickableText.disabled True
                ]
                |> clickOnButton
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    , test "the `type` attribute is present and set to `\"button\"` for a disabled ClickableText" <|
        \() ->
            programButton
                [ ClickableText.disabled True
                ]
                |> ensureViewHas [ attribute (Attributes.type_ "button") ]
                |> done
    , test "the `type` attribute is present and set to `\"button\"` for a disabled ClickableText with the `submit` type" <|
        \() ->
            programButton
                [ ClickableText.disabled True
                , ClickableText.submit
                ]
                |> ensureViewHas [ attribute (Attributes.type_ "button") ]
                |> done
    ]


buttonSelectors : List Selector
buttonSelectors =
    [ tag "button"
    ]


type alias TestContext =
    ProgramTest Model Msg ()


clickOnButton : TestContext -> TestContext
clickOnButton =
    MouseHelpers.click mouseHelperConfig buttonSelectors


type alias Model =
    ()


init : Model
init =
    ()


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg state =
    case msg of
        NoOp ->
            state


viewLink : List (ClickableText.Attribute Msg) -> Model -> Html Msg
viewLink attributes _ =
    div []
        [ ClickableText.link "Accessible name" attributes
        ]


viewButton : List (ClickableText.Attribute Msg) -> Model -> Html Msg
viewButton attributes _ =
    div []
        [ ClickableText.button "Accessible name" attributes
        ]


programLink : List (ClickableText.Attribute Msg) -> TestContext
programLink attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewLink attributes >> toUnstyled
        }
        |> ProgramTest.start ()


programButton : List (ClickableText.Attribute Msg) -> TestContext
programButton attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewButton attributes >> toUnstyled
        }
        |> ProgramTest.start ()


mouseHelperConfig : MouseHelpers.Config (ProgramTest model msg effect) Selector.Selector (Query.Single msg)
mouseHelperConfig =
    { programTest_simulateDomEvent = ProgramTest.simulateDomEvent
    , query_find = Query.find
    , event_click = Event.click
    , event_mouseDown = Event.mouseDown
    , event_mouseUp = Event.mouseUp
    , event_mouseOver = Event.mouseOver
    , event_custom = Event.custom
    }
