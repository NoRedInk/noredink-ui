module Nri.Ui.RadioButton.V4 exposing
    ( view
    , premium, onLockedClick
    , disclosure
    , onSelect
    , Attribute
    , hiddenLabel, visibleLabel
    , containerCss, labelCss, custom, nriDescription, id, testId
    , disabled, enabled, errorIf, errorMessage, guidance
    )

{-| Changes from V3:

  - use PremiumDisplay instead of PremiumLevel
  - rename showPennant to onLockedClick since its display depends on premium now
  - make onLockedClick be triggers when clicking anywhere and not just pennant to match PremiumChecbox

@docs view


### Content

@docs premium, onLockedClick
@docs disclosure


### Event handlers

@docs onSelect


### Attributes

@docs Attribute
@docs hiddenLabel, visibleLabel
@docs containerCss, labelCss, custom, nriDescription, id, testId
@docs disabled, enabled, errorIf, errorMessage, guidance

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Css.Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (class, classList, css, for)
import Html.Styled.Events exposing (onClick)
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 exposing (Svg)
import String exposing (toLower)
import String.Extra exposing (dasherize)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


{-| This disables the input
-}
disabled : Attribute value msg
disabled =
    Attribute <| \config -> { config | isDisabled = True }


{-| This enables the input, this is the default behavior
-}
enabled : Attribute value msg
enabled =
    Attribute <| \config -> { config | isDisabled = False }


{-| Sets whether or not the field will be highlighted as having a validation error.
-}
errorIf : Bool -> Attribute value msg
errorIf =
    Attribute << InputErrorAndGuidanceInternal.setErrorIf


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.
-}
errorMessage : Maybe String -> Attribute value msg
errorMessage =
    Attribute << InputErrorAndGuidanceInternal.setErrorMessage


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute value msg
guidance =
    Attribute << InputErrorAndGuidanceInternal.setGuidance


{-| Fire a message parameterized by the value type when selecting a radio option
-}
onSelect : (value -> msg) -> Attribute value msg
onSelect onSelect_ =
    Attribute <| \config -> { config | onSelect = Just onSelect_ }


{-| Lock Premium content if the user does not have Premium.
-}
premium : PremiumDisplay -> Attribute value msg
premium premiumDisplay =
    Attribute <|
        \config ->
            { config | premiumDisplay = premiumDisplay }


{-| Makes the Premium pennant clickable.

When the pennant is clicked, the msg that's passed in will fire.

-}
onLockedClick : msg -> Attribute value msg
onLockedClick onLockedMsg =
    Attribute <| \config -> { config | onLockedMsg = Just onLockedMsg }


{-| Content that shows when this RadioButton is selected
-}
disclosure : List (Html msg) -> Attribute value msg
disclosure childNodes =
    Attribute <| \config -> { config | disclosedContent = childNodes }


{-| Adds CSS to the element containing the input.
-}
containerCss : List Css.Style -> Attribute value msg
containerCss styles =
    Attribute <| \config -> { config | containerCss = config.containerCss ++ styles }


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element, since it contains the icon SVG as well.

-}
labelCss : List Css.Style -> Attribute value msg
labelCss styles =
    Attribute <| \config -> { config | labelCss = config.labelCss ++ styles }


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute value msg
hiddenLabel =
    Attribute <| \config -> { config | hideLabel = True }


{-| Shows the visible label. This is the default behavior
-}
visibleLabel : Attribute value msg
visibleLabel =
    Attribute <| \config -> { config | hideLabel = False }


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one radio input with the same label on
the page. You might also use this helper if you're manually managing focus.
-}
id : String -> Attribute value msg
id id_ =
    Attribute <| \config -> { config | id = Just id_ }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute value msg
custom attributes =
    Attribute <| \config -> { config | custom = config.custom ++ attributes }


{-| -}
nriDescription : String -> Attribute value msg
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute value msg
testId id_ =
    custom [ Extra.testId id_ ]


{-| Customizations for the RadioButton.
-}
type Attribute value msg
    = Attribute (Config value msg -> Config value msg)


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config value msg =
    { name : Maybe String
    , id : Maybe String
    , premiumDisplay : PremiumDisplay
    , isDisabled : Bool
    , guidance : Guidance
    , error : ErrorState
    , hideLabel : Bool
    , containerCss : List Css.Style
    , labelCss : List Css.Style
    , custom : List (Html.Attribute Never)
    , onSelect : Maybe (value -> msg)
    , onLockedMsg : Maybe msg
    , disclosedContent : List (Html msg)
    }


emptyConfig : Config value msg
emptyConfig =
    { name = Nothing
    , id = Nothing
    , premiumDisplay = PremiumDisplay.Free
    , isDisabled = False
    , guidance = InputErrorAndGuidanceInternal.noGuidance
    , error = InputErrorAndGuidanceInternal.noError
    , hideLabel = False
    , containerCss = []
    , labelCss = []
    , custom = []
    , onSelect = Nothing
    , onLockedMsg = Nothing
    , disclosedContent = []
    }


applyConfig : List (Attribute value msg) -> Config value msg -> Config value msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


{-| View a single radio button.
-}
view :
    { label : String
    , name : String
    , value : value
    , valueToString : value -> String
    , selectedValue : Maybe value
    }
    -> List (Attribute value msg)
    -> Html msg
view { label, name, value, valueToString, selectedValue } attributes =
    let
        config =
            applyConfig attributes emptyConfig

        stringValue =
            valueToString value

        idValue =
            case config.id of
                Just specificId ->
                    specificId

                Nothing ->
                    name ++ "-" ++ dasherize (toLower stringValue)

        isChecked =
            selectedValue == Just value

        isPremium =
            config.premiumDisplay /= PremiumDisplay.Free

        isLocked =
            config.premiumDisplay == PremiumDisplay.PremiumLocked

        ( disclosureIds, disclosedElements ) =
            config.disclosedContent
                |> List.indexedMap
                    (\index element ->
                        let
                            id_ =
                                (idValue ++ "-disclosure-content-") ++ String.fromInt index
                        in
                        ( id_, span [ Attributes.id id_ ] [ element ] )
                    )
                |> List.unzip

        isInError =
            InputErrorAndGuidanceInternal.getIsInError config.error
    in
    if isLocked then
        viewLockedButton { idValue = idValue, label = label } config

    else
        Html.span
            [ Attributes.id (idValue ++ "-container")
            , css
                [ position relative
                , margin4 (px 2) (px 2) (px 2) (px -2)
                , Css.padding4 (px 4) (px 2) (px 2) (Css.px 38)
                , display inlineBlock
                , pseudoClass "focus-within"
                    [ Css.Global.descendants
                        [ Css.Global.class "Nri-RadioButton-RadioButtonIcon"
                            [ FocusRing.boxShadows []
                            ]
                        ]
                    ]
                , Css.batch config.containerCss
                ]
            ]
            ([ radio name
                stringValue
                isChecked
                ([ Attributes.id idValue
                 , Aria.disabled config.isDisabled
                 , InputErrorAndGuidanceInternal.describedBy idValue config
                 , case config.onSelect of
                    Just onSelect_ ->
                        onClick (onSelect_ value)

                    Nothing ->
                        Extra.none
                 , class "Nri-RadioButton-HiddenRadioInput"
                 , Aria.describedBy disclosureIds
                 , css
                    [ position absolute
                    , top (pct 50)
                    , left (px 4)
                    , opacity zero
                    ]
                 ]
                    ++ List.map (Attributes.map never) config.custom
                )
             , Html.label
                [ for idValue
                , classList
                    [ ( "Nri-RadioButton-RadioButton", True )
                    , ( "Nri-RadioButton-RadioButtonChecked", isChecked )
                    ]
                , css
                    [ outline Css.none
                    , Fonts.baseFont
                    , Css.batch
                        (if config.isDisabled then
                            [ color Colors.gray45
                            , cursor notAllowed
                            ]

                         else if isInError then
                            [ color Colors.purple
                            , cursor pointer
                            ]

                         else
                            [ color Colors.navy
                            , cursor pointer
                            ]
                        )
                    , margin zero
                    , padding zero
                    , fontSize (px 15)
                    , Css.property "font-weight" "600"
                    , display inlineBlock
                    , Css.property "transition" "all 0.4s ease"
                    ]
                ]
                [ radioInputIcon
                    { isLocked = isLocked
                    , isDisabled = config.isDisabled
                    , isChecked = isChecked
                    }
                , span
                    [ css
                        [ display inlineFlex
                        , alignItems center
                        , Css.height (px 20)
                        ]
                    ]
                    [ Html.span
                        [ css <|
                            if config.hideLabel then
                                [ Css.width (px 1)
                                , overflow Css.hidden
                                , margin (px -1)
                                , padding (px 0)
                                , border (px 0)
                                , display inlineBlock
                                , textIndent (px 1)
                                ]

                            else
                                config.labelCss
                        ]
                        [ Html.text label ]
                    , if isPremium then
                        premiumPennant

                      else
                        text ""
                    ]
                ]
             , InputErrorAndGuidanceInternal.view idValue config
             ]
                ++ (if isChecked then
                        disclosedElements

                    else
                        []
                   )
            )


viewLockedButton : { idValue : String, label : String } -> Config value msg -> Html msg
viewLockedButton { idValue, label } config =
    button
        [ Attributes.id (idValue ++ "-container")
        , css
            [ position relative
            , marginLeft (px -4)
            , Css.paddingLeft (Css.px 40)
            , Css.paddingTop (px 6)
            , Css.paddingBottom (px 4)
            , display inlineBlock
            , backgroundColor Css.transparent
            , border Css.zero
            , cursor pointer
            , Css.batch config.containerCss
            ]
        , case config.onLockedMsg of
            Just msg ->
                onClick msg

            Nothing ->
                Extra.none
        ]
        [ Html.div
            [ class "Nri-RadioButton-LockedPremiumButton"
            , css
                [ outline Css.none
                , Fonts.baseFont
                , color Colors.navy
                , margin zero
                , padding zero
                , fontSize (px 15)
                , Css.property "font-weight" "600"
                , displayFlex
                , Css.property "transition" "all 0.4s ease"
                ]
            ]
            [ radioInputIcon
                { isLocked = True
                , isDisabled = False
                , isChecked = False
                }
            , span
                [ css
                    [ display inlineFlex
                    , alignItems center
                    , Css.height (px 20)
                    ]
                ]
                [ Html.span
                    [ css <|
                        if config.hideLabel then
                            [ Css.width (px 1)
                            , overflow Css.hidden
                            , margin (px -1)
                            , padding (px 0)
                            , border (px 0)
                            , display inlineBlock
                            , textIndent (px 1)
                            ]

                        else
                            config.labelCss
                    ]
                    [ Html.text label ]
                , premiumPennant
                ]
            ]
        , InputErrorAndGuidanceInternal.view idValue config
        ]


premiumPennant : Html msg
premiumPennant =
    Pennant.premiumFlag
        |> Nri.Ui.Svg.V1.withWidth (Css.px 26)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 24)
        |> Nri.Ui.Svg.V1.withCss
            [ marginLeft (px 8)
            , verticalAlign middle
            ]
        |> Nri.Ui.Svg.V1.toHtml


radioInputIcon :
    { isChecked : Bool
    , isLocked : Bool
    , isDisabled : Bool
    }
    -> Html msg
radioInputIcon config =
    let
        image =
            case ( config.isDisabled, config.isLocked, config.isChecked ) of
                ( _, True, _ ) ->
                    lockedSvg

                ( True, _, _ ) ->
                    unselectedSvg

                ( _, False, True ) ->
                    selectedSvg

                ( _, False, False ) ->
                    unselectedSvg

        iconHeight =
            26
    in
    div
        [ classList
            [ ( "Nri-RadioButton-RadioButtonIcon", True )
            , ( "Nri-RadioButton-RadioButtonDisabled", config.isDisabled )
            ]
        , css
            [ Css.batch <|
                if config.isDisabled then
                    [ opacity (num 0.4) ]

                else
                    []
            , position absolute
            , left zero
            , top (calc (pct 50) Css.minus (Css.px (iconHeight / 2)))
            , Css.property "transition" ".3s all"
            , borderRadius (px 50)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ image
            |> Nri.Ui.Svg.V1.withHeight (Css.px iconHeight)
            |> Nri.Ui.Svg.V1.withWidth (Css.px 26)
            |> Nri.Ui.Svg.V1.toHtml
        ]


unselectedSvg : Svg
unselectedSvg =
    Nri.Ui.Svg.V1.init "0 0 27 27"
        [ Svg.defs []
            [ Svg.rect [ SvgAttributes.id "unselected-path-1", SvgAttributes.x "0", SvgAttributes.y "0", SvgAttributes.width "27", SvgAttributes.height "27", SvgAttributes.rx "13.5" ] []
            , Svg.filter [ SvgAttributes.id "unselected-filter-2", SvgAttributes.x "-3.7%", SvgAttributes.y "-3.7%", SvgAttributes.width "107.4%", SvgAttributes.height "107.4%", SvgAttributes.filterUnits "objectBoundingBox" ] [ Svg.feOffset [ SvgAttributes.dx "0", SvgAttributes.dy "2", SvgAttributes.in_ "SourceAlpha", SvgAttributes.result "shadowOffsetInner1" ] [], Svg.feComposite [ SvgAttributes.in_ "shadowOffsetInner1", SvgAttributes.in2 "SourceAlpha", SvgAttributes.operator "arithmetic", SvgAttributes.k2 "-1", SvgAttributes.k3 "1", SvgAttributes.result "shadowInnerInner1" ] [], Svg.feColorMatrix [ SvgAttributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0", SvgAttributes.in_ "shadowInnerInner1" ] [] ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ Svg.g []
                [ Svg.g []
                    [ Svg.use
                        [ SvgAttributes.fill "#EBEBEB"
                        , SvgAttributes.fillRule "evenodd"
                        , SvgAttributes.xlinkHref "#unselected-path-1"
                        ]
                        []
                    , Svg.use
                        [ SvgAttributes.fill "black"
                        , SvgAttributes.fillOpacity "1"
                        , SvgAttributes.filter "url(#unselected-filter-2)"
                        , SvgAttributes.xlinkHref "#unselected-path-1"
                        ]
                        []
                    ]
                ]
            ]
        ]
        |> withImageBorder Colors.gray75


selectedSvg : Svg
selectedSvg =
    Nri.Ui.Svg.V1.init "0 0 27 27"
        [ Svg.defs []
            [ Svg.rect [ SvgAttributes.id "selected-path-1", SvgAttributes.x "0", SvgAttributes.y "0", SvgAttributes.width "27", SvgAttributes.height "27", SvgAttributes.rx "13.5" ] []
            , Svg.filter
                [ SvgAttributes.id "selected-filter-2", SvgAttributes.x "-3.7%", SvgAttributes.y "-3.7%", SvgAttributes.width "107.4%", SvgAttributes.height "107.4%", SvgAttributes.filterUnits "objectBoundingBox" ]
                [ Svg.feOffset [ SvgAttributes.dx "0", SvgAttributes.dy "2", SvgAttributes.in_ "SourceAlpha", SvgAttributes.result "shadowOffsetInner1" ] [], Svg.feComposite [ SvgAttributes.in_ "shadowOffsetInner1", SvgAttributes.in2 "SourceAlpha", SvgAttributes.operator "arithmetic", SvgAttributes.k2 "-1", SvgAttributes.k3 "1", SvgAttributes.result "shadowInnerInner1" ] [], Svg.feColorMatrix [ SvgAttributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0", SvgAttributes.in_ "shadowInnerInner1" ] [] ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ Svg.g []
                [ Svg.g []
                    [ Svg.use
                        [ SvgAttributes.fill "#D4F0FF"
                        , SvgAttributes.fillRule "evenodd"
                        , SvgAttributes.xlinkHref "#selected-path-1"
                        ]
                        []
                    , Svg.use
                        [ SvgAttributes.fill "black"
                        , SvgAttributes.fillOpacity "1"
                        , SvgAttributes.filter "url(#selected-filter-2)"
                        , SvgAttributes.xlinkHref "#selected-path-1"
                        ]
                        []
                    ]
                , Svg.circle
                    [ SvgAttributes.fill "#146AFF"
                    , SvgAttributes.cx "13.5"
                    , SvgAttributes.cy "13.5"
                    , SvgAttributes.r "6.3"
                    ]
                    []
                ]
            ]
        ]
        |> withImageBorder Colors.azure


lockedSvg : Svg
lockedSvg =
    Nri.Ui.Svg.V1.init "0 0 30 30"
        [ Svg.defs []
            [ Svg.rect [ SvgAttributes.id "locked-path-1", SvgAttributes.x "0", SvgAttributes.y "0", SvgAttributes.width "30", SvgAttributes.height "30", SvgAttributes.rx "15" ] []
            , Svg.filter [ SvgAttributes.id "locked-filter-2", SvgAttributes.x "-3.3%", SvgAttributes.y "-3.3%", SvgAttributes.width "106.7%", SvgAttributes.height "106.7%", SvgAttributes.filterUnits "objectBoundingBox" ] [ Svg.feOffset [ SvgAttributes.dx "0", SvgAttributes.dy "2", SvgAttributes.in_ "SourceAlpha", SvgAttributes.result "shadowOffsetInner1" ] [], Svg.feComposite [ SvgAttributes.in_ "shadowOffsetInner1", SvgAttributes.in2 "SourceAlpha", SvgAttributes.operator "arithmetic", SvgAttributes.k2 "-1", SvgAttributes.k3 "1", SvgAttributes.result "shadowInnerInner1" ] [], Svg.feColorMatrix [ SvgAttributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0", SvgAttributes.in_ "shadowInnerInner1" ] [] ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ Svg.g []
                [ Svg.use
                    [ SvgAttributes.fill "#EBEBEB"
                    , SvgAttributes.fillRule "evenodd"
                    , SvgAttributes.xlinkHref "#locked-path-1"
                    ]
                    []
                , Svg.use
                    [ SvgAttributes.fill "black"
                    , SvgAttributes.fillOpacity "1"
                    , SvgAttributes.filter "url(#locked-filter-2)"
                    , SvgAttributes.xlinkHref "#locked-path-1"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttributes.transform "translate(8.000000, 5.000000)"
                ]
                [ Svg.path
                    [ SvgAttributes.d "M11.7991616,9.36211885 L11.7991616,5.99470414 C11.7991616,3.24052783 9.64616757,1 7.00011784,1 C4.35359674,1 2.20083837,3.24052783 2.20083837,5.99470414 L2.20083837,9.36211885 L1.51499133,9.36211885 C0.678540765,9.36211885 -6.21724894e-14,10.0675883 -6.21724894e-14,10.9381415 L-6.21724894e-14,18.9239773 C-6.21724894e-14,19.7945305 0.678540765,20.5 1.51499133,20.5 L12.48548,20.5 C13.3219306,20.5 14,19.7945305 14,18.9239773 L14,10.9383868 C14,10.0678336 13.3219306,9.36211885 12.48548,9.36211885 L11.7991616,9.36211885 Z M7.46324136,15.4263108 L7.46324136,17.2991408 C7.46324136,17.5657769 7.25560176,17.7816368 7.00011784,17.7816368 C6.74416256,17.7816368 6.53652295,17.5657769 6.53652295,17.2991408 L6.53652295,15.4263108 C6.01259238,15.228848 5.63761553,14.7080859 5.63761553,14.0943569 C5.63761553,13.3116195 6.24757159,12.6763045 7.00011784,12.6763045 C7.75195704,12.6763045 8.36285584,13.3116195 8.36285584,14.0943569 C8.36285584,14.7088218 7.98717193,15.2295839 7.46324136,15.4263108 L7.46324136,15.4263108 Z M9.98178482,9.36211885 L4.01821518,9.36211885 L4.01821518,5.99470414 C4.01821518,4.2835237 5.35597044,2.89122723 7.00011784,2.89122723 C8.64402956,2.89122723 9.98178482,4.2835237 9.98178482,5.99470414 L9.98178482,9.36211885 L9.98178482,9.36211885 Z"
                    , SvgAttributes.fill "#E68800"
                    ]
                    []
                , Svg.path
                    [ SvgAttributes.d "M11.7991616,8.14770554 L11.7991616,4.8666348 C11.7991616,2.18307839 9.64616757,-7.10542736e-15 7.00011784,-7.10542736e-15 C4.35359674,-7.10542736e-15 2.20083837,2.18307839 2.20083837,4.8666348 L2.20083837,8.14770554 L1.51499133,8.14770554 C0.678540765,8.14770554 -6.21724894e-14,8.83508604 -6.21724894e-14,9.6833174 L-6.21724894e-14,17.4643881 C-6.21724894e-14,18.3126195 0.678540765,19 1.51499133,19 L12.48548,19 C13.3219306,19 14,18.3126195 14,17.4643881 L14,9.68355641 C14,8.83532505 13.3219306,8.14770554 12.48548,8.14770554 L11.7991616,8.14770554 Z M7.46324136,14.0564054 L7.46324136,15.8812141 C7.46324136,16.1410134 7.25560176,16.3513384 7.00011784,16.3513384 C6.74416256,16.3513384 6.53652295,16.1410134 6.53652295,15.8812141 L6.53652295,14.0564054 C6.01259238,13.8640057 5.63761553,13.3565966 5.63761553,12.7586042 C5.63761553,11.9959369 6.24757159,11.376912 7.00011784,11.376912 C7.75195704,11.376912 8.36285584,11.9959369 8.36285584,12.7586042 C8.36285584,13.3573136 7.98717193,13.8647228 7.46324136,14.0564054 L7.46324136,14.0564054 Z M9.98178482,8.14770554 L4.01821518,8.14770554 L4.01821518,4.8666348 C4.01821518,3.19933078 5.35597044,1.84273423 7.00011784,1.84273423 C8.64402956,1.84273423 9.98178482,3.19933078 9.98178482,4.8666348 L9.98178482,8.14770554 L9.98178482,8.14770554 Z"
                    , SvgAttributes.fill "#FEC709"
                    ]
                    []
                ]
            ]
        ]
        |> withImageBorder Colors.gray75


withImageBorder : Color -> Svg -> Svg
withImageBorder color =
    Nri.Ui.Svg.V1.withCss
        [ Css.border3 (px 1) solid color
        , Css.borderRadius (Css.pct 50)
        ]
