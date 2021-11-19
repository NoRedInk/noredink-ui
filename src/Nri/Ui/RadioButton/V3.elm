module Nri.Ui.RadioButton.V3 exposing
    ( view
    , disabled, enabled
    , value, selectedValue, valueToString
    , onSelect
    , name
    , premium, showPennant
    , disclosure, block, inline
    , describedBy
    , none
    )

{-| Changes from V2:

  - list based API instead of record based
  - add disclosure to show rich content when the radio is selected

@docs view
@docs disabled, enabled
@docs value, selectedValue, valueToString
@docs onSelect
@docs name
@docs premium, showPennant
@docs disclosure, block, inline
@docs describedBy
@docs none

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style as Style
import Accessibility.Styled.Widget as Widget
import Css as Css exposing (..)
import Css.Global
import Html.Styled as Html
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Attributes
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 exposing (Svg, fromHtml)
import String exposing (toLower)
import String.Extra exposing (dasherize)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


{-| This disables the input
-}
disabled : Attribute value msg
disabled =
    Attribute emptyEventsAndValues <|
        \config -> { config | isDisabled = True }


{-| This enables the input, this is the default behavior
-}
enabled : Attribute value msg
enabled =
    Attribute emptyEventsAndValues <|
        \config -> { config | isDisabled = False }


{-| Every radio button in the same group should have the same name
-}
name : String -> Attribute value msg
name name_ =
    Attribute emptyEventsAndValues <|
        \config -> { config | name = Just name_ }


{-| Sets the value of one radio button
-}
value : value -> Attribute value msg
value value_ =
    Attribute { emptyEventsAndValues | value = Just value_ } identity


{-| Specifies what the current value of a group of radio buttons should be
-}
selectedValue : Maybe value -> Attribute value msg
selectedValue value_ =
    Attribute { emptyEventsAndValues | selectedValue = value_ } identity


{-| Fire a message parameterized by the value type when selecting a radio option
-}
onSelect : (value -> msg) -> Attribute value msg
onSelect onSelect_ =
    Attribute { emptyEventsAndValues | onSelect = Just onSelect_ } identity


{-| Since <input>s transact in strings we need to be able to give every radio button in a group
a unique string value. This function should be the same for every RadioButton in a group.
-}
valueToString : (value -> String) -> Attribute value msg
valueToString valueToString_ =
    Attribute { emptyEventsAndValues | valueToString = Just valueToString_ } identity


{-| Lock the content if the teacher does not have premium access
-}
premium : { teacherPremiumLevel : PremiumLevel, contentPremiumLevel : PremiumLevel } -> Attribute value msg
premium { teacherPremiumLevel, contentPremiumLevel } =
    Attribute emptyEventsAndValues <|
        \config ->
            { config
                | teacherPremiumLevel = Just teacherPremiumLevel
                , contentPremiumLevel = Just contentPremiumLevel
            }


{-| Show the pennant and attach this onClick handler
-}
showPennant : msg -> Attribute value msg
showPennant premiumMsg =
    Attribute { emptyEventsAndValues | premiumMsg = Just premiumMsg } identity


{-| Content that shows when this RadioButton is selected
-}
disclosure : List (Html msg) -> Attribute value msg
disclosure childNodes =
    Attribute { emptyEventsAndValues | disclosedContent = childNodes } identity


{-| Set the Aria describedby attribute if given a non-empty list of IDs
-}
describedBy : List String -> Attribute value msg
describedBy ids =
    Attribute emptyEventsAndValues <|
        \config -> { config | describedByIds = ids }


{-| Has no effect; useful for conditionals and cases
-}
none : Attribute value msg
none =
    Attribute emptyEventsAndValues identity


{-| Displays the radio button as a gray block with a small black label.
Designed to have the disclosure content be more prominent
-}
block : Attribute value msg
block =
    Attribute emptyEventsAndValues <|
        \config -> { config | display = GrayBlock }


{-| Displays the radio button as an inline span with a large blue label.
Designed to have the disclosure content be inline if present at all
-}
inline : Attribute value msg
inline =
    Attribute emptyEventsAndValues <|
        \config -> { config | display = Inline }


type Display
    = GrayBlock
    | Inline


{-| Customizations for the RadioButton.
-}
type Attribute value msg
    = Attribute (EventsAndValues value msg) (Config -> Config)


type alias EventsAndValues value msg =
    { value : Maybe value
    , selectedValue : Maybe value
    , onSelect : Maybe (value -> msg)
    , valueToString : Maybe (value -> String)
    , premiumMsg : Maybe msg
    , disclosedContent : List (Html msg)
    }


emptyEventsAndValues : EventsAndValues value msg
emptyEventsAndValues =
    { value = Nothing
    , selectedValue = Nothing
    , onSelect = Nothing
    , valueToString = Nothing
    , premiumMsg = Nothing
    , disclosedContent = []
    }


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config =
    { name : Maybe String
    , teacherPremiumLevel : Maybe PremiumLevel
    , contentPremiumLevel : Maybe PremiumLevel
    , isDisabled : Bool
    , showPennant : Bool
    , describedByIds : List String
    , display : Display
    }


emptyConfig : Config
emptyConfig =
    { name = Nothing
    , teacherPremiumLevel = Nothing
    , contentPremiumLevel = Nothing
    , isDisabled = False
    , showPennant = False
    , describedByIds = []
    , display = Inline
    }


applyConfig : List (Attribute value msg) -> Config
applyConfig attributes =
    List.foldl (\(Attribute _ update) config -> update config)
        emptyConfig
        attributes


orExisting : (acc -> Maybe a) -> acc -> acc -> Maybe a
orExisting f new previous =
    case f previous of
        Just just ->
            Just just

        Nothing ->
            f new


applyEvents : List (Attribute value msg) -> EventsAndValues value msg
applyEvents =
    List.foldl
        (\(Attribute eventsAndValues _) existing ->
            { value = orExisting .value eventsAndValues existing
            , selectedValue = orExisting .selectedValue eventsAndValues existing
            , onSelect = orExisting .onSelect eventsAndValues existing
            , valueToString = orExisting .valueToString eventsAndValues existing
            , premiumMsg = orExisting .premiumMsg eventsAndValues existing
            , disclosedContent = eventsAndValues.disclosedContent ++ existing.disclosedContent
            }
        )
        emptyEventsAndValues


maybeAttr : (a -> Html.Attribute msg) -> Maybe a -> Html.Attribute msg
maybeAttr attr maybeValue =
    maybeValue
        |> Maybe.map attr
        |> Maybe.withDefault Attributes.none


{-| View a single radio button.
Renders nothing if the attributes list does not contain value, name, and valueToString.
-}
view : String -> List (Attribute value msg) -> Html msg
view label attributes =
    let
        eventsAndValues : EventsAndValues value msg
        eventsAndValues =
            applyEvents attributes

        config : Config
        config =
            applyConfig attributes
    in
    case makeInternalConfig label config eventsAndValues of
        Just internalConfig ->
            case config.display of
                Inline ->
                    viewInline internalConfig

                GrayBlock ->
                    viewBlock internalConfig

        _ ->
            text "no radio button here"


type alias InternalConfig value msg =
    { -- user specified values
      value : value
    , name : String
    , valueToString : value -> String
    , eventsAndValues : EventsAndValues value msg
    , config : Config
    , label : String

    -- TODO: computed values that both view helpers need
    }


makeInternalConfig : String -> Config -> EventsAndValues value msg -> Maybe (InternalConfig value msg)
makeInternalConfig label config eventsAndValues =
    case ( eventsAndValues.value, config.name, eventsAndValues.valueToString ) of
        ( Just value_, Just name_, Just valueToString_ ) ->
            Just
                { value = value_
                , name = name_
                , valueToString = valueToString_
                , eventsAndValues = eventsAndValues
                , config = config
                , label = label
                }

        _ ->
            Nothing


viewBlock : InternalConfig value msg -> Html msg
viewBlock internalConfig =
    let
        isChecked =
            -- why not guard and make sure neither is Nothing?
            -- Because if value is Nothing we do not render a radio
            internalConfig.eventsAndValues.selectedValue
                == internalConfig.eventsAndValues.value

        isLocked : Bool
        isLocked =
            case ( internalConfig.config.contentPremiumLevel, internalConfig.config.teacherPremiumLevel ) of
                ( Just contentPremiumLevel, Just teacherPremiumLevel ) ->
                    not <|
                        PremiumLevel.allowedFor
                            contentPremiumLevel
                            teacherPremiumLevel

                _ ->
                    False

        showPennant_ =
            case internalConfig.eventsAndValues.premiumMsg of
                Just _ ->
                    True

                _ ->
                    False

        id_ =
            internalConfig.name ++ "-" ++ (dasherize <| toLower <| internalConfig.valueToString internalConfig.value)

        disclosureIdAndElement : Maybe ( String, Html msg )
        disclosureIdAndElement =
            case ( internalConfig.eventsAndValues.disclosedContent, isChecked ) of
                ( [], _ ) ->
                    Nothing

                ( _, False ) ->
                    Nothing

                ( (_ :: _) as childNodes, True ) ->
                    let
                        disclosureId =
                            id_ ++ "-disclosure-content"
                    in
                    Just <| ( disclosureId, div [ id disclosureId ] childNodes )
    in
    Html.div
        [ id (id_ ++ "-container")
        , classList [ ( "Nri-RadioButton-PremiumClass", showPennant_ ) ]
        , css
            [ position relative
            , display Css.block
            , Css.height (px 34)
            , Css.width <| pct 100
            , Css.backgroundColor Colors.gray96
            , padding <| Css.px 20
            , marginBottom <| Css.px 10
            , borderRadius <| Css.px 8
            ]
        ]
        [ radio internalConfig.name
            (internalConfig.valueToString internalConfig.value)
            isChecked
            [ id id_
            , Widget.disabled (isLocked || internalConfig.config.isDisabled)
            , case ( internalConfig.eventsAndValues.onSelect, internalConfig.config.isDisabled ) of
                ( Just onSelect_, False ) ->
                    onClick (onSelect_ internalConfig.value)

                _ ->
                    Attributes.none
            , class "Nri-RadioButton-HiddenRadioInput"
            , maybeAttr (Tuple.first >> Aria.controls) disclosureIdAndElement
            , case internalConfig.config.describedByIds of
                (_ :: _) as describedByIds ->
                    Aria.describedBy describedByIds

                [] ->
                    Attributes.none
            , css
                [ position absolute
                , top (px 4)
                , left (px 4)
                , opacity zero
                , pseudoClass "focus"
                    [ Css.Global.adjacentSiblings
                        [ Css.Global.everything
                            [ Css.Global.descendants
                                [ Css.Global.class "Nri-RadioButton-RadioButtonIcon"
                                    [ borderColor (rgb 0 95 204)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Html.label
            [ for id_
            , classList
                [ ( "Nri-RadioButton-RadioButton", True )
                , ( "Nri-RadioButton-RadioButtonChecked", isChecked )
                ]
            , css <|
                [ position relative
                , outline Css.none
                , margin zero
                , Fonts.baseFont
                , if internalConfig.config.isDisabled then
                    Css.batch
                        [ color Colors.gray45
                        , cursor notAllowed
                        ]

                  else
                    cursor pointer
                , padding4 zero zero zero (px 40)
                ]
            ]
            [ radioInputIcon
                { isLocked = isLocked
                , isDisabled = internalConfig.config.isDisabled
                , isChecked = isChecked
                }
            , span
                (if showPennant_ then
                    [ css
                        [ display inlineFlex
                        , alignItems center
                        , Css.height (px 20)
                        ]
                    ]

                 else
                    [ css [ verticalAlign middle ] ]
                )
                [ Html.text internalConfig.label
                , viewJust
                    (\premiumMsg ->
                        ClickableSvg.button "Premium"
                            Pennant.premiumFlag
                            [ ClickableSvg.onClick premiumMsg
                            , ClickableSvg.exactWidth 26
                            , ClickableSvg.exactHeight 24
                            , ClickableSvg.css [ marginLeft (px 8) ]
                            ]
                    )
                    internalConfig.eventsAndValues.premiumMsg
                ]
            ]
        , viewJust
            Tuple.second
            disclosureIdAndElement
        ]


viewInline : InternalConfig value msg -> Html msg
viewInline internalConfig =
    let
        isChecked =
            -- why not guard and make sure neither is Nothing?
            -- Because if value is Nothing we do not render a radio
            internalConfig.eventsAndValues.selectedValue
                == internalConfig.eventsAndValues.value

        isLocked : Bool
        isLocked =
            case ( internalConfig.config.contentPremiumLevel, internalConfig.config.teacherPremiumLevel ) of
                ( Just contentPremiumLevel, Just teacherPremiumLevel ) ->
                    not <|
                        PremiumLevel.allowedFor
                            contentPremiumLevel
                            teacherPremiumLevel

                _ ->
                    False

        showPennant_ =
            case internalConfig.eventsAndValues.premiumMsg of
                Just _ ->
                    True

                _ ->
                    False

        id_ =
            internalConfig.name ++ "-" ++ (dasherize <| toLower <| internalConfig.valueToString internalConfig.value)

        disclosureIdAndElement : Maybe ( String, Html msg )
        disclosureIdAndElement =
            case ( internalConfig.eventsAndValues.disclosedContent, isChecked ) of
                ( [], _ ) ->
                    Nothing

                ( _, False ) ->
                    Nothing

                ( (_ :: _) as childNodes, True ) ->
                    let
                        disclosureId =
                            id_ ++ "-disclosure-content"
                    in
                    Just <| ( disclosureId, div [ id disclosureId ] childNodes )
    in
    Html.span
        [ id (id_ ++ "-container")
        , classList [ ( "Nri-RadioButton-PremiumClass", showPennant_ ) ]
        , css
            [ position relative
            , marginLeft (px -4)
            , display inlineBlock
            , Css.height (px 34)
            , pseudoClass "focus-within"
                [ Css.Global.descendants
                    [ Css.Global.class "Nri-RadioButton-RadioButtonIcon"
                        [ borderColor (rgb 0 95 204)
                        ]
                    ]
                ]
            ]
        ]
        [ radio internalConfig.name
            (internalConfig.valueToString internalConfig.value)
            isChecked
            [ id id_
            , Widget.disabled (isLocked || internalConfig.config.isDisabled)
            , case ( internalConfig.eventsAndValues.onSelect, internalConfig.config.isDisabled ) of
                ( Just onSelect_, False ) ->
                    onClick (onSelect_ internalConfig.value)

                _ ->
                    Attributes.none
            , class "Nri-RadioButton-HiddenRadioInput"
            , maybeAttr (Tuple.first >> Aria.controls) disclosureIdAndElement
            , case internalConfig.config.describedByIds of
                (_ :: _) as describedByIds ->
                    Aria.describedBy describedByIds

                [] ->
                    Attributes.none
            , css
                [ position absolute
                , top (px 4)
                , left (px 4)
                , opacity zero
                , pseudoClass "focus"
                    [ Css.Global.adjacentSiblings
                        [ Css.Global.everything
                            [ Css.Global.descendants
                                [ Css.Global.class "Nri-RadioButton-RadioButtonIcon"
                                    [ borderColor (rgb 0 95 204)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Html.label
            [ for id_
            , classList
                [ ( "Nri-RadioButton-RadioButton", True )
                , ( "Nri-RadioButton-RadioButtonChecked", isChecked )
                ]
            , css <|
                [ position relative
                , outline Css.none
                , margin zero
                , Fonts.baseFont
                , if internalConfig.config.isDisabled then
                    Css.batch
                        [ color Colors.gray45
                        , cursor notAllowed
                        ]

                  else
                    cursor pointer
                , padding4 (px 6) zero (px 4) (px 40)
                , fontSize (px 15)
                , Css.property "font-weight" "600"
                , display inlineBlock
                , color Colors.navy
                ]
            ]
            [ radioInputIcon
                { isLocked = isLocked
                , isDisabled = internalConfig.config.isDisabled
                , isChecked = isChecked
                }
            , span
                (if showPennant_ then
                    [ css
                        [ display inlineFlex
                        , alignItems center
                        , Css.height (px 20)
                        ]
                    ]

                 else
                    [ css [ verticalAlign middle ] ]
                )
                [ Html.text internalConfig.label
                , viewJust
                    (\premiumMsg ->
                        ClickableSvg.button "Premium"
                            Pennant.premiumFlag
                            [ ClickableSvg.onClick premiumMsg
                            , ClickableSvg.exactWidth 26
                            , ClickableSvg.exactHeight 24
                            , ClickableSvg.css [ marginLeft (px 8) ]
                            ]
                    )
                    internalConfig.eventsAndValues.premiumMsg
                ]
            ]
        , viewJust
            Tuple.second
            disclosureIdAndElement
        ]


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
            , top zero
            , Css.property "transition" ".3s all"
            , border3 (px 2) solid transparent
            , borderRadius (px 50)
            , padding (px 2)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ image
            |> Nri.Ui.Svg.V1.withHeight (Css.px 26)
            |> Nri.Ui.Svg.V1.withWidth (Css.px 26)
            |> Nri.Ui.Svg.V1.toHtml
        ]


unselectedSvg : Svg
unselectedSvg =
    Svg.svg [ SvgAttributes.viewBox "0 0 27 27" ]
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
        |> Nri.Ui.Svg.V1.fromHtml


selectedSvg : Svg
selectedSvg =
    Svg.svg [ SvgAttributes.viewBox "0 0 27 27" ]
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
        |> Nri.Ui.Svg.V1.fromHtml


lockedSvg : Svg
lockedSvg =
    Svg.svg [ SvgAttributes.viewBox "0 0 30 30" ]
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
        |> Nri.Ui.Svg.V1.fromHtml
