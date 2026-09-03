module Nri.Ui.HintTooltip.V1 exposing
    ( view
    , Attribute
    , plaintext, paragraph, markdown, html
    , withoutTail
    , onTop, onBottom, onLeft, onRight
    , onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
    , alignStart, alignMiddle, alignEnd
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , open, onToggle, onTriggerKeyDown
    , css, testId, nriDescription
    , primaryLabel, auxiliaryDescription, helpfullyDisabled
    )

{-| Overflow-safe, non-interactive tooltip content for hover and focus triggers.

Use this module for short descriptive content that should escape clipped
ancestors such as scroll containers, table wrappers, and sidebars.

@docs view
@docs Attribute
@docs plaintext, paragraph, markdown, html
@docs withoutTail
@docs onTop, onBottom, onLeft, onRight
@docs onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
@docs alignStart, alignMiddle, alignEnd
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs open, onToggle, onTriggerKeyDown
@docs css, testId, nriDescription
@docs primaryLabel, auxiliaryDescription, helpfullyDisabled

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Css
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.AnchoredOverlay.Internal.V1 as Internal


{-| Configuration attributes for `HintTooltip.view`.
-}
type alias Attribute msg =
    Internal.Attribute msg


{-| Render a controlled, non-interactive tooltip.
-}
view :
    { id : String
    , trigger : List (Html.Attribute msg) -> Html msg
    }
    -> List (Attribute msg)
    -> Html msg
view { id, trigger } attributes =
    let
        overlay =
            Internal.buildAttributes attributes
    in
    Internal.view
        { id = id
        , anchorId = anchorId id
        , trigger = trigger
        , mode = Internal.Hint
        , wrapperDescription = "Nri-Ui-HintTooltip-V1"
        , wrapperEvents = maybeHoverEvents overlay
        , triggerAttributes =
            \config ->
                purposeAttributes id config
                    ++ maybeFocusEvents config
                    ++ keyDownAttributes config
        , hostAttributes =
            \config ->
                [ Attributes.attribute "role" "tooltip"
                , Attributes.attribute "aria-hidden"
                    (if config.hintPurpose == Internal.PrimaryLabel then
                        "true"

                     else
                        "false"
                    )
                ]
        }
        overlay


anchorId : String -> String
anchorId id =
    id ++ "-anchor"


maybeHoverEvents : Internal.Config msg -> List (Html.Attribute msg)
maybeHoverEvents config =
    case config.onToggle of
        Just toMsg ->
            [ Events.onMouseEnter (toMsg True)
            , Events.onMouseLeave (toMsg False)
            ]

        Nothing ->
            []


maybeFocusEvents : Internal.Config msg -> List (Html.Attribute msg)
maybeFocusEvents config =
    case config.onToggle of
        Just toMsg ->
            [ Events.onFocus (toMsg True)
            , Events.onBlur (toMsg False)
            ]

        Nothing ->
            []


purposeAttributes : String -> Internal.Config msg -> List (Html.Attribute msg)
purposeAttributes id config =
    case config.hintPurpose of
        Internal.PrimaryLabel ->
            []

        Internal.AuxiliaryDescription ->
            [ Aria.describedBy [ id ] ]

        Internal.HelpfullyDisabled ->
            [ Aria.describedBy [ id ] ]


keyDownAttributes : Internal.Config msg -> List (Html.Attribute msg)
keyDownAttributes config =
    case ( config.onToggle, config.triggerKeyDownEvents ) of
        ( Just toMsg, extraEvents ) ->
            [ Key.onKeyDown (Key.escape (toMsg False) :: extraEvents) ]

        ( Nothing, [] ) ->
            []

        ( Nothing, extraEvents ) ->
            [ Key.onKeyDown extraEvents ]


{-| Provide plain text content.
-}
plaintext : String -> Attribute msg
plaintext =
    Internal.plaintext


{-| Provide plain text content wrapped in a paragraph.
-}
paragraph : String -> Attribute msg
paragraph =
    Internal.paragraph


{-| Provide inline markdown content.
-}
markdown : String -> Attribute msg
markdown =
    Internal.markdown


{-| Provide custom HTML content.
-}
html : List (Html msg) -> Attribute msg
html =
    Internal.html


{-| Remove the tooltip tail.
-}
withoutTail : Attribute msg
withoutTail =
    Internal.withoutTail


{-| Prefer placing the tooltip above the trigger.
-}
onTop : Attribute msg
onTop =
    Internal.onTop


{-| Prefer placing the tooltip below the trigger.
-}
onBottom : Attribute msg
onBottom =
    Internal.onBottom


{-| Prefer placing the tooltip to the left of the trigger.
-}
onLeft : Attribute msg
onLeft =
    Internal.onLeft


{-| Prefer placing the tooltip to the right of the trigger.
-}
onRight : Attribute msg
onRight =
    Internal.onRight


{-| Use top placement on mobile.
-}
onTopForMobile : Attribute msg
onTopForMobile =
    Internal.onTopForMobile


{-| Use bottom placement on mobile.
-}
onBottomForMobile : Attribute msg
onBottomForMobile =
    Internal.onBottomForMobile


{-| Use left placement on mobile.
-}
onLeftForMobile : Attribute msg
onLeftForMobile =
    Internal.onLeftForMobile


{-| Use right placement on mobile.
-}
onRightForMobile : Attribute msg
onRightForMobile =
    Internal.onRightForMobile


{-| Align the tooltip toward the start of the trigger.
-}
alignStart : Attribute msg
alignStart =
    Internal.alignStart


{-| Center-align the tooltip against the trigger.
-}
alignMiddle : Attribute msg
alignMiddle =
    Internal.alignMiddle


{-| Align the tooltip toward the end of the trigger.
-}
alignEnd : Attribute msg
alignEnd =
    Internal.alignEnd


{-| Set an exact tooltip width in pixels.
-}
exactWidth : Int -> Attribute msg
exactWidth =
    Internal.exactWidth


{-| Size the tooltip to its content.
-}
fitToContent : Attribute msg
fitToContent =
    Internal.fitToContent


{-| Use the compact padding preset.
-}
smallPadding : Attribute msg
smallPadding =
    Internal.smallPadding


{-| Use the default padding preset.
-}
normalPadding : Attribute msg
normalPadding =
    Internal.normalPadding


{-| Set custom padding in pixels.
-}
customPadding : Float -> Attribute msg
customPadding =
    Internal.customPadding


{-| Control whether the tooltip is open.
-}
open : Bool -> Attribute msg
open =
    Internal.open


{-| Receive open and close requests from hover, focus, and Escape handling.
-}
onToggle : (Bool -> msg) -> Attribute msg
onToggle =
    Internal.onToggle


{-| Add additional trigger `keydown` handlers.
-}
onTriggerKeyDown : List (Key.Event msg) -> Attribute msg
onTriggerKeyDown =
    Internal.onTriggerKeyDown


{-| Extend the tooltip bubble styles.
-}
css : List Css.Style -> Attribute msg
css =
    Internal.css


{-| Add a `data-testid` to the tooltip bubble.
-}
testId : String -> Attribute msg
testId =
    Internal.testId


{-| Add a `data-nri-description` to the tooltip bubble.
-}
nriDescription : String -> Attribute msg
nriDescription =
    Internal.nriDescription


{-| Hide redundant tooltip text from assistive technology.
-}
primaryLabel : Attribute msg
primaryLabel =
    Internal.primaryLabel


{-| Associate the tooltip as an auxiliary description for the trigger.
-}
auxiliaryDescription : Attribute msg
auxiliaryDescription =
    Internal.auxiliaryDescription


{-| Associate the tooltip as explanatory text for a disabled trigger.
-}
helpfullyDisabled : Attribute msg
helpfullyDisabled =
    Internal.helpfullyDisabled
