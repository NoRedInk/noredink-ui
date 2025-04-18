module Spec.Nri.Ui.ClickableSvg exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Expect
import Html.Attributes as Attributes
import Html.Styled exposing (..)
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.UiIcon.V2 as UiIcon
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.ClickableSvg.V2"
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
    , test "renders an svg element" <|
        \() ->
            programButton []
                |> ensureViewHas [ tag "svg" ]
                |> done
    , test "renders an svg element when a right icon is provided" <|
        \() ->
            programButton [ ClickableSvg.rightIcon UiIcon.arrowLeft ]
                |> ensureView (Query.findAll [ tag "svg" ] >> Query.count (Expect.equal 2))
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
            programButton [ ClickableSvg.submit ]
                |> ensureViewHas [ attribute (Attributes.type_ "submit") ]
                |> done
    , test "a link has the `href` attribute set to the provided value" <|
        \() ->
            programLink [ ClickableSvg.href "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.href "https://example.com")
                    ]
                |> done
    , test "an external link has the `href` attribute set to the provided value" <|
        \() ->
            programLink [ ClickableSvg.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.href "https://example.com")
                    ]
                |> done
    , test "a default link has the `target` attribute set to `\"_self\"`" <|
        \() ->
            programLink [ ClickableSvg.href "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.target "_self")
                    ]
                |> done
    , test "an external link has the `target` attribute set to `\"_blank\"`" <|
        \() ->
            programLink [ ClickableSvg.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.target "_blank")
                    ]
                |> done
    , test "an external link has the `rel` attribute set to `\"noopener noreferrer\"`" <|
        \() ->
            programLink [ ClickableSvg.linkExternal "https://example.com" ]
                |> ensureViewHas
                    [ attribute (Attributes.rel "noopener noreferrer")
                    ]
                |> done
    ]


iconAccessibilityTests : List Test
iconAccessibilityTests =
    [ test "the icon has the `aria-hidden` attribute set to `\"true\"`" <|
        \() ->
            programButton []
                |> ensureViewHas
                    [ attribute (Aria.hidden True)
                    ]
                |> done
    , test "the icon has the `role` attribute set to `\"img\"`" <|
        \() ->
            programButton []
                |> ensureViewHas
                    [ attribute Role.img
                    ]
                |> done
    , test "the icon has the `focusable` attribute set to `\"false\"`" <|
        \() ->
            programButton []
                |> ensureViewHas
                    [ attribute (Attributes.attribute "focusable" "false")
                    ]
                |> done
    , test "the right icon has the `aria-hidden` attribute set to `\"true\"`" <|
        \() ->
            programButton [ ClickableSvg.rightIcon UiIcon.arrowLeft ]
                |> ensureView
                    (Query.findAll [ tag "svg" ]
                        >> Query.index 1
                        >> Query.has
                            [ attribute (Aria.hidden True)
                            ]
                    )
                |> done
    , test "the right icon has the `role` attribute set to `\"img\"`" <|
        \() ->
            programButton [ ClickableSvg.rightIcon UiIcon.arrowLeft ]
                |> ensureView
                    (Query.findAll [ tag "svg" ]
                        >> Query.index 1
                        >> Query.has
                            [ attribute Role.img
                            ]
                    )
                |> done
    , test "the right icon has the `focusable` attribute set to `\"false\"`" <|
        \() ->
            programButton [ ClickableSvg.rightIcon UiIcon.arrowLeft ]
                |> ensureView
                    (Query.findAll [ tag "svg" ]
                        >> Query.index 1
                        >> Query.has
                            [ attribute (Attributes.attribute "focusable" "false")
                            ]
                    )
                |> done
    ]


disabledStateTests : List Test
disabledStateTests =
    [ test "the `aria-disabled` attribute is not present for an enabled ClickableSvg" <|
        \() ->
            programButton []
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "the `aria-disabled` attribute is present and set to `\"true\"` for a disabled ClickableSvg" <|
        \() ->
            programButton [ ClickableSvg.disabled True ]
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when enabled" <|
        \() ->
            programButton
                [ ClickableSvg.onClick NoOp
                ]
                |> clickOnButton
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            programButton
                [ ClickableSvg.disabled True
                ]
                |> clickOnButton
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    , test "the `type` attribute is present and set to `\"button\"` for a disabled ClickableSvg" <|
        \() ->
            programButton
                [ ClickableSvg.disabled True
                ]
                |> ensureViewHas [ attribute (Attributes.type_ "button") ]
                |> done
    , test "the `type` attribute is present and set to `\"button\"` for a disabled ClickableSvg with the `submit` type" <|
        \() ->
            programButton
                [ ClickableSvg.disabled True
                , ClickableSvg.submit
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
    MouseHelpers.click buttonSelectors


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


viewLink : List (ClickableSvg.Attribute Msg) -> Model -> Html Msg
viewLink attributes _ =
    div []
        [ ClickableSvg.link "Accessible name" UiIcon.arrowLeft attributes
        ]


viewButton : List (ClickableSvg.Attribute Msg) -> Model -> Html Msg
viewButton attributes _ =
    div []
        [ ClickableSvg.button "Accessible name" UiIcon.arrowLeft attributes
        ]


programLink : List (ClickableSvg.Attribute Msg) -> TestContext
programLink attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewLink attributes >> toUnstyled
        }
        |> ProgramTest.start ()


programButton : List (ClickableSvg.Attribute Msg) -> TestContext
programButton attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewButton attributes >> toUnstyled
        }
        |> ProgramTest.start ()
