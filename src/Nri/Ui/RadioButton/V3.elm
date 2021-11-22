module Nri.Ui.RadioButton.V3 exposing
    ( view, Attribute
    , disabled, enabled
    , value, selectedValue, valueToString
    , onSelect
    , name
    , premium, showPennant
    , disclosure
    , hiddenLabel, visibleLabel
    , describedBy
    , none, batch
    , containerCss
    )

{-| Changes from V2:

  - list based API instead of record based
  - add disclosure to show rich content when the radio is selected
  - when the radio button is disabled don't attach onSelect listener

@docs view, Attribute
@docs disabled, enabled
@docs value, selectedValue, valueToString
@docs onSelect
@docs name
@docs premium, showPennant
@docs disclosure
@docs hiddenLabel, visibleLabel
@docs describedBy
@docs none, batch

@docs containerCss

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


{-| Adds CSS to the element containing the input.
-}
containerCss : List Css.Style -> Attribute value msg
containerCss styles =
    Attribute emptyEventsAndValues <|
        \config -> { config | containerCss = config.containerCss ++ styles }


{-| Has no effect; useful for conditionals and cases
-}
none : Attribute value msg
none =
    Attribute emptyEventsAndValues identity


{-| Apply a list of attributes as one. Again useful for conditionals
-}
batch : List (Attribute value msg) -> Attribute value msg
batch attributes =
    Attribute (applyEvents attributes) (applyConfig attributes)


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute value msg
hiddenLabel =
    Attribute emptyEventsAndValues <|
        \config -> { config | hideLabel = True }


{-| Shows the visible label. This is the default behavior
-}
visibleLabel : Attribute value msg
visibleLabel =
    Attribute emptyEventsAndValues <|
        \config -> { config | hideLabel = False }


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
    , hideLabel : Bool
    , containerCss : List Css.Style
    }


emptyConfig : Config
emptyConfig =
    { name = Nothing
    , teacherPremiumLevel = Nothing
    , contentPremiumLevel = Nothing
    , isDisabled = False
    , showPennant = False
    , describedByIds = []
    , hideLabel = False
    , containerCss = []
    }


applyConfig : List (Attribute value msg) -> Config -> Config
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute _ update) config -> update config)
        beginningConfig
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
        config_ =
            applyConfig attributes emptyConfig
    in
    case internalConfig label config_ (applyEvents attributes) of
        Just config ->
            view_ config

        _ ->
            text "no radio button here"


type alias InternalConfig value msg =
    { -- user specified Attributes
      name : String
    , label : String
    , teacherPremiumLevel : Maybe PremiumLevel
    , contentPremiumLevel : Maybe PremiumLevel
    , isDisabled : Bool
    , describedByIds : List String
    , hideLabel : Bool

    -- user specified messages and values TODO unpack eventsAndValues
    , value : value
    , selectedValue : Maybe value
    , onSelect : Maybe (value -> msg)
    , valueToString : value -> String
    , premiumMsg : Maybe msg
    , disclosedContent : List (Html msg)
    , containerCss : List Css.Style

    -- computed values that both view helpers need
    , isChecked : Bool
    , isLocked : Bool
    , showPennant : Bool
    , id : String
    , disclosureIdAndElement : Maybe ( String, List (Html msg) )
    }


internalConfig : String -> Config -> EventsAndValues value msg -> Maybe (InternalConfig value msg)
internalConfig label config eventsAndValues =
    case ( eventsAndValues.value, config.name, eventsAndValues.valueToString ) of
        ( Just value_, Just name_, Just valueToString_ ) ->
            let
                isChecked =
                    -- why not guard and make sure neither is Nothing?
                    -- Because if value is Nothing we do not render a radio
                    eventsAndValues.selectedValue
                        == eventsAndValues.value

                id_ =
                    name_ ++ "-" ++ (dasherize <| toLower <| valueToString_ value_)

                disclosureId =
                    id_ ++ "-disclosure-content"
            in
            Just
                { name = name_
                , label = label
                , teacherPremiumLevel = config.teacherPremiumLevel
                , contentPremiumLevel = config.contentPremiumLevel
                , isDisabled = config.isDisabled
                , describedByIds = config.describedByIds
                , hideLabel = config.hideLabel
                , value = value_
                , selectedValue = eventsAndValues.selectedValue
                , onSelect = eventsAndValues.onSelect
                , valueToString = valueToString_
                , premiumMsg = eventsAndValues.premiumMsg
                , disclosedContent = eventsAndValues.disclosedContent
                , isChecked = isChecked
                , containerCss = config.containerCss
                , isLocked =
                    case ( config.contentPremiumLevel, config.teacherPremiumLevel ) of
                        ( Just contentPremiumLevel, Just teacherPremiumLevel ) ->
                            not <|
                                PremiumLevel.allowedFor
                                    contentPremiumLevel
                                    teacherPremiumLevel

                        _ ->
                            False
                , showPennant =
                    case eventsAndValues.premiumMsg of
                        Just _ ->
                            True

                        _ ->
                            False
                , id = id_
                , disclosureIdAndElement =
                    case ( eventsAndValues.disclosedContent, isChecked ) of
                        ( [], _ ) ->
                            Nothing

                        ( _, False ) ->
                            Nothing

                        ( (_ :: _) as childNodes, True ) ->
                            Just <| ( disclosureId, childNodes )
                }

        _ ->
            Nothing


view_ : InternalConfig value msg -> Html msg
view_ config =
    Html.span
        [ id (config.id ++ "-container")
        , classList [ ( "Nri-RadioButton-PremiumClass", config.showPennant ) ]
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
            , Css.batch config.containerCss
            ]
        ]
        [ radio config.name
            (config.valueToString config.value)
            config.isChecked
            [ id config.id
            , Widget.disabled (config.isLocked || config.isDisabled)
            , case ( config.onSelect, config.isDisabled ) of
                ( Just onSelect_, False ) ->
                    onClick (onSelect_ config.value)

                _ ->
                    Attributes.none
            , class "Nri-RadioButton-HiddenRadioInput"
            , maybeAttr (Tuple.first >> Aria.controls) config.disclosureIdAndElement
            , case config.describedByIds of
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
            [ for config.id
            , classList
                [ ( "Nri-RadioButton-RadioButton", True )
                , ( "Nri-RadioButton-RadioButtonChecked", config.isChecked )
                ]
            , css <|
                [ position relative
                , outline Css.none
                , margin zero
                , Fonts.baseFont
                , if config.isDisabled then
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
                { isLocked = config.isLocked
                , isDisabled = config.isDisabled
                , isChecked = config.isChecked
                }
            , span
                (if config.showPennant then
                    [ css
                        [ display inlineFlex
                        , alignItems center
                        , Css.height (px 20)
                        ]
                    ]

                 else
                    [ css [ verticalAlign middle ] ]
                )
                [ Html.span
                    [ css <|
                        if config.hideLabel then
                            [ Css.height <| px 1
                            , Css.width <| px 1
                            , overflow Css.hidden
                            , margin <| px -1
                            , padding <| px 0
                            , border <| px 0
                            , display inlineBlock
                            , textIndent <| px 1
                            ]

                        else
                            []
                    ]
                    [ Html.text config.label ]
                , case ( config.hideLabel, config.premiumMsg ) of
                    ( False, Just premiumMsg ) ->
                        ClickableSvg.button "Premium"
                            Pennant.premiumFlag
                            [ ClickableSvg.onClick premiumMsg
                            , ClickableSvg.exactWidth 26
                            , ClickableSvg.exactHeight 24
                            , ClickableSvg.css [ marginLeft (px 8) ]
                            ]

                    _ ->
                        text ""
                ]
            ]
        , case config.disclosureIdAndElement of
            Just ( id_, childNodes ) ->
                span [ id id_ ] childNodes

            Nothing ->
                text ""
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
