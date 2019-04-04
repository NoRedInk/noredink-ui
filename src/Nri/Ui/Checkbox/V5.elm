module Nri.Ui.Checkbox.V5 exposing
    ( Model, Theme(..), IsSelected(..)
    , view, viewWithLabel, Assets
    , selectedFromBool
    )

{-|


# Changes from V5:

  - Removes `noOpMsg` from Model

@docs Model, Theme, IsSelected

@docs view, viewWithLabel, Assets

@docs selectedFromBool

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Global exposing (Snippet, children, descendants, everything, selector)
import Html.Events
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.AssetPath.Css
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V3 as HtmlExtra exposing (defaultOptions)
import Svg
import Svg.Attributes


{-| -}
type alias Model msg =
    { identifier : String
    , label : String
    , setterMsg : Bool -> msg
    , selected : IsSelected
    , disabled : Bool
    , theme : Theme
    }


{-|

    = Selected --  Checked (rendered with a checkmark)
    | NotSelected -- Not Checked (rendered blank)
    | PartiallySelected -- Indeterminate (rendered dash)

-}
type IsSelected
    = Selected
    | NotSelected
    | PartiallySelected


{-| -}
type Theme
    = Square
    | Locked


{-| If your selectedness is always selected or not selected,
you will likely store that state as a `Bool` in your model.
`selectedFromBool` lets you easily convert that into an `IsSelected` value
for use with `Nri.Ui.Checkbox`.
-}
selectedFromBool : Bool -> IsSelected
selectedFromBool isSelected =
    case isSelected of
        True ->
            Selected

        False ->
            NotSelected


selectedToMaybe : IsSelected -> Maybe Bool
selectedToMaybe selected =
    case selected of
        Selected ->
            Just True

        NotSelected ->
            Just False

        PartiallySelected ->
            Nothing


{-| Shows a checkbox (the label is only used for accessibility hints)
-}
view : Assets a -> Model msg -> Html.Html msg
view assets model =
    buildCheckbox assets model <|
        Html.span Accessibility.Styled.Style.invisible
            [ Html.text model.label ]


{-| Shows a checkbox and its label text
-}
viewWithLabel : Assets a -> Model msg -> Html.Html msg
viewWithLabel assets model =
    buildCheckbox assets model <|
        Html.span [] [ Html.text model.label ]


buildCheckbox : Assets a -> Model msg -> Html.Html msg -> Html.Html msg
buildCheckbox assets model labelContent =
    viewCheckbox model <|
        case model.theme of
            Square ->
                { containerClasses = toClassList [ "SquareClass" ]
                , labelStyles =
                    squareLabelStyles model <|
                        case model.selected of
                            Selected ->
                                checkboxChecked

                            NotSelected ->
                                checkboxUnchecked

                            PartiallySelected ->
                                checkboxCheckedPartially
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }

            Locked ->
                { containerClasses = toClassList [ "Locked" ]
                , labelStyles = lockLabelStyles model assets.checkboxLockOnInside_svg
                , labelClasses = labelClass model.selected
                , labelContent = labelContent
                }


squareLabelStyles : { b | disabled : Bool } -> Icon -> Html.Styled.Attribute msg
squareLabelStyles model image =
    let
        baseStyles =
            [ positioning
            , textStyle
            , outline none
            , addIcon image
            ]
    in
    css
        (baseStyles
            ++ (if model.disabled then
                    [ cursor auto, checkboxImageSelector [ opacity (num 0.4) ] ]

                else
                    [ cursor pointer ]
               )
        )


lockLabelStyles : { b | disabled : Bool } -> Icon -> Html.Styled.Attribute msg
lockLabelStyles model image =
    let
        baseStyles =
            [ positioning
            , textStyle
            , outline none
            , addIcon image
            ]
    in
    css
        (baseStyles
            ++ (if model.disabled then
                    [ cursor auto
                    , checkboxImageSelector [ opacity (num 0.4) ]
                    ]

                else
                    [ cursor pointer ]
               )
        )


positioning : Style
positioning =
    batch
        [ display inlineBlock
        , padding4 (px 13) zero (px 13) (px 35)
        ]


textStyle : Style
textStyle =
    batch
        [ Fonts.baseFont
        , fontSize (px 16)
        , fontWeight (int 600)
        , color Colors.navy
        ]


addIcon : Icon -> Style
addIcon icon =
    batch
        [ position relative
        , checkboxImageSelector
            [ backgroundImage icon
            , backgroundRepeat noRepeat
            , backgroundSize (px 24)
            , property "content" "''"
            , position absolute
            , left zero
            , top (calc (pct 50) minus (px 12))
            , width (px 24)
            , height (px 24)
            ]
        ]


checkboxImageSelector : List Style -> Style
checkboxImageSelector =
    before


labelClass : IsSelected -> Html.Styled.Attribute msg
labelClass isSelected =
    case isSelected of
        Selected ->
            toClassList [ "Label", "Checked" ]

        NotSelected ->
            toClassList [ "Label", "Unchecked" ]

        PartiallySelected ->
            toClassList [ "Label", "Indeterminate" ]


toClassList : List String -> Html.Styled.Attribute msg
toClassList =
    List.map (\a -> ( "checkbox-V3__" ++ a, True )) >> Attributes.classList


viewCheckbox :
    Model msg
    ->
        { containerClasses : Html.Attribute msg
        , labelStyles : Html.Attribute msg
        , labelClasses : Html.Attribute msg
        , labelContent : Html.Html msg
        }
    -> Html.Html msg
viewCheckbox model config =
    let
        toggledValue =
            selectedToMaybe model.selected
                |> Maybe.withDefault False
                |> not
    in
    Html.Styled.span
        [ css
            [ display block
            , height inherit
            , descendants [ Css.Global.input [ display none ] ]
            ]
        , config.containerClasses
        , Attributes.id <| model.identifier ++ "-container"
        , Events.stopPropagationOn "click"
            (Json.Decode.fail "stop click propagation")
        ]
        [ Html.checkbox model.identifier
            (selectedToMaybe model.selected)
            [ Widget.label model.label
            , Events.onCheck (\_ -> model.setterMsg toggledValue)
            , Attributes.id model.identifier
            , Attributes.disabled model.disabled
            ]
        , viewLabel model config.labelContent config.labelClasses config.labelStyles
        ]


viewLabel : Model msg -> Html.Html msg -> Html.Attribute msg -> Html.Attribute msg -> Html.Html msg
viewLabel model content class theme =
    Html.Styled.label
        [ Attributes.for model.identifier
        , Aria.controls model.identifier
        , Widget.disabled model.disabled
        , Widget.checked (selectedToMaybe model.selected)
        , if not model.disabled then
            Attributes.tabindex 0

          else
            ExtraAttributes.none
        , if not model.disabled then
            --TODO: the accessibility keyboard module might make this a tad more readable.
            HtmlExtra.onKeyUp
                { defaultOptions | preventDefault = True }
                (\keyCode ->
                    -- 32 is the space bar, 13 is enter
                    if (keyCode == 32 || keyCode == 13) && not model.disabled then
                        Just <| model.setterMsg (Maybe.map not (selectedToMaybe model.selected) |> Maybe.withDefault True)

                    else
                        Nothing
                )

          else
            ExtraAttributes.none
        , class
        , theme
        ]
        [ content ]


{-| The assets used in this module.
-}
type alias Assets r =
    { r
        | checkboxUnchecked_svg : Asset
        , checkboxChecked_svg : Asset
        , checkboxCheckedPartially_svg : Asset
        , checkboxLockOnInside_svg : Asset
    }


backgroundImage : Icon -> Style
backgroundImage (Icon icon) =
    -- Nri.Ui.AssetPath.Css.url
    --     >> property "background-image"
    property "background-image" "url(todo)"


type Icon
    = Icon (Html.Html Never)


checkboxUnchecked : Icon
checkboxUnchecked =
    Svg.svg
        [ Svg.Attributes.width "27px"
        , Svg.Attributes.height "27px"
        , Svg.Attributes.viewBox "0 0 27 27"
        ]
        [ Svg.defs []
            [ Svg.rect
                [ Svg.Attributes.id "path-1"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "27"
                , Svg.Attributes.height "27"
                , Svg.Attributes.rx "4"
                ]
                []
            , Svg.filter
                [ Svg.Attributes.x "-3.7%"
                , Svg.Attributes.y "-3.7%"
                , Svg.Attributes.width "107.4%"
                , Svg.Attributes.height "107.4%"
                , Svg.Attributes.filterUnits "objectBoundingBox"
                , Svg.Attributes.id "filter-2"
                ]
                [ Svg.feOffset
                    [ Svg.Attributes.dx "0"
                    , Svg.Attributes.dy "2"
                    , Svg.Attributes.in_ "SourceAlpha"
                    , Svg.Attributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ Svg.Attributes.in_ "shadowOffsetInner1"
                    , Svg.Attributes.in2 "SourceAlpha"
                    , Svg.Attributes.operator "arithmetic"
                    , Svg.Attributes.k2 "-1"
                    , Svg.Attributes.k3 "1"
                    , Svg.Attributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ Svg.Attributes.values "0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0.1 0"
                    , Svg.Attributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ Svg.Attributes.id "Page-1"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Svg.Attributes.id "checkbox_unchecked"
                ]
                [ Svg.use [ Svg.Attributes.fill "#EBEBEB", Svg.Attributes.fillRule "evenodd" ] []
                , Svg.use
                    [ Svg.Attributes.fill "black"
                    , Svg.Attributes.fillOpacity "1"
                    , Svg.Attributes.filter "url(#filter-2)"
                    ]
                    []
                ]
            ]
        ]
        |> Html.Styled.fromUnstyled
        |> Icon


checkboxChecked : Icon
checkboxChecked =
    Svg.svg
        [ Svg.Attributes.width "27px"
        , Svg.Attributes.height "27px"
        , Svg.Attributes.viewBox "0 0 27 27"
        ]
        [ Svg.defs []
            [ Svg.rect
                [ Svg.Attributes.id "path-1"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "27"
                , Svg.Attributes.height "27"
                , Svg.Attributes.rx "4"
                ]
                []
            , Svg.filter
                [ Svg.Attributes.x "-3.7%"
                , Svg.Attributes.y "-3.7%"
                , Svg.Attributes.width "107.4%"
                , Svg.Attributes.height "107.4%"
                , Svg.Attributes.filterUnits "objectBoundingBox"
                , Svg.Attributes.id "filter-2"
                ]
                [ Svg.feOffset
                    [ Svg.Attributes.dx "0"
                    , Svg.Attributes.dy "2"
                    , Svg.Attributes.in_ "SourceAlpha"
                    , Svg.Attributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ Svg.Attributes.in_ "shadowOffsetInner1"
                    , Svg.Attributes.in2 "SourceAlpha"
                    , Svg.Attributes.operator "arithmetic"
                    , Svg.Attributes.k2 "-1"
                    , Svg.Attributes.k3 "1"
                    , Svg.Attributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ Svg.Attributes.values "0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0.1 0"
                    , Svg.Attributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ Svg.Attributes.id "Page-1"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Svg.Attributes.id "checkbox_checked"
                ]
                [ Svg.g []
                    [ Svg.use
                        [ Svg.Attributes.fill "#D4F0FF"
                        , Svg.Attributes.fillRule "evenodd"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.fill "black"
                        , Svg.Attributes.fillOpacity "1"
                        , Svg.Attributes.filter "url(#filter-2)"
                        ]
                        []
                    ]
                , Svg.g
                    [ Svg.Attributes.id "icon/check-blue"
                    , Svg.Attributes.transform "translate(3.600000, 3.600000)"
                    , Svg.Attributes.fill "#146AFF"
                    ]
                    [ Svg.path
                        [ Svg.Attributes.d "M7.04980639,17.8647896 C6.57427586,17.8647896 6.11539815,17.6816086 5.77123987,17.3513276 L0.571859358,12.3786105 C-0.167340825,11.672716 -0.193245212,10.5014676 0.513574487,9.7631926 C1.21761872,9.02491757 2.38979222,8.99808803 3.12899241,9.70490773 L6.96746745,13.3750043 L16.7917062,2.73292703 C17.4855737,1.98077465 18.6558969,1.93451682 19.4061989,2.62745917 C20.1574262,3.32132667 20.2046091,4.49164987 19.5116668,5.24195193 L8.4097867,17.2689887 C8.07210452,17.6344256 7.60397524,17.8481368 7.10716611,17.8638644 C7.08866297,17.8647896 7.06923468,17.8647896 7.04980639,17.8647896"
                        ]
                        []
                    ]
                ]
            ]
        ]
        |> Html.Styled.fromUnstyled
        |> Icon


checkboxCheckedPartially : Icon
checkboxCheckedPartially =
    Svg.svg
        [ Svg.Attributes.width "27px"
        , Svg.Attributes.height "27px"
        , Svg.Attributes.viewBox "0 0 27 27"
        ]
        [ Svg.defs []
            [ Svg.rect
                [ Svg.Attributes.id "path-1"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "27"
                , Svg.Attributes.height "27"
                , Svg.Attributes.rx "4"
                ]
                []
            , Svg.filter
                [ Svg.Attributes.x "-3.7%"
                , Svg.Attributes.y "-3.7%"
                , Svg.Attributes.width "107.4%"
                , Svg.Attributes.height "107.4%"
                , Svg.Attributes.filterUnits "objectBoundingBox"
                , Svg.Attributes.id "filter-2"
                ]
                [ Svg.feOffset
                    [ Svg.Attributes.dx "0"
                    , Svg.Attributes.dy "2"
                    , Svg.Attributes.in_ "SourceAlpha"
                    , Svg.Attributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ Svg.Attributes.in_ "shadowOffsetInner1"
                    , Svg.Attributes.in2 "SourceAlpha"
                    , Svg.Attributes.operator "arithmetic"
                    , Svg.Attributes.k2 "-1"
                    , Svg.Attributes.k3 "1"
                    , Svg.Attributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix [ Svg.Attributes.values "0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0.1 0", Svg.Attributes.in_ "shadowInnerInner1" ] []
                ]
            ]
        , Svg.g
            [ Svg.Attributes.id "Page-1"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Svg.Attributes.id "checkbox_checkedPartially"
                ]
                [ Svg.g
                    [ Svg.Attributes.id "checkbox_checked"
                    ]
                    [ Svg.use
                        [ Svg.Attributes.fill "#EBEBEB"
                        , Svg.Attributes.fillRule "evenodd"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.fill "black"
                        , Svg.Attributes.fillOpacity "1"
                        , Svg.Attributes.filter "url(#filter-2)"
                        ]
                        []
                    ]
                , Svg.path
                    [ Svg.Attributes.d "M22.2879231,10.8937777 C22.4823276,11.0881822 22.5430781,11.3676344 22.4701764,11.7321429 C22.1785697,13.2630784 21.6196651,14.4294879 20.793446,15.2314064 C19.9672268,16.033325 18.9830688,16.4342783 17.8409423,16.4342783 C16.9175209,16.4342783 16.073089,16.3006272 15.3076213,16.033321 C14.5421536,15.7660148 13.612671,15.3772116 12.5191457,14.8668998 C11.668626,14.4537903 10.9821454,14.1500378 10.4596833,13.9556333 C9.93722115,13.7612288 9.40869184,13.664028 8.87407945,13.664028 C7.53754849,13.664028 6.68704155,14.3201333 6.32253311,15.6323637 C6.27393198,15.8267682 6.17065614,15.9907946 6.01270248,16.1244477 C5.85474882,16.2581008 5.66642228,16.3249263 5.44771721,16.3249263 C5.20471159,16.3249263 4.9860098,16.2277255 4.7916053,16.033321 C4.59720079,15.8389165 4.5,15.6202147 4.5,15.3772091 C4.5,14.6238916 4.71262674,13.8705855 5.13788659,13.117268 C5.56314644,12.3639506 6.1342011,11.7503706 6.85106771,11.2765096 C7.56793431,10.8026486 8.32731551,10.5657217 9.12923409,10.5657217 C10.076956,10.5657217 10.933538,10.6993728 11.6990058,10.966679 C12.4644735,11.2339852 13.3939561,11.6227884 14.4874814,12.1331002 C15.3380011,12.5462097 16.0244817,12.8499622 16.5469438,13.0443667 C17.0694059,13.2387712 17.5979352,13.335972 18.1325476,13.335972 C18.8129634,13.335972 19.2868173,13.2266211 19.5541234,13.0079161 C19.8214296,12.789211 20.1008819,12.4004078 20.3924887,11.8414949 C20.4653904,11.6470904 20.5747413,11.4162385 20.7205446,11.1489323 C20.9149491,10.7844239 21.2065515,10.6021724 21.5953605,10.6021724 C21.8626667,10.6021724 22.0935186,10.6993732 22.2879231,10.8937777 Z"
                    , Svg.Attributes.id "~"
                    , Svg.Attributes.fill "#146AFF"
                    ]
                    []
                ]
            ]
        ]
        |> Html.Styled.fromUnstyled
        |> Icon


checkboxLockOnInside : Icon
checkboxLockOnInside =
    Svg.svg
        [ Svg.Attributes.width "27px"
        , Svg.Attributes.height "27px"
        , Svg.Attributes.viewBox "0 0 27 27"
        ]
        [ Svg.defs []
            [ Svg.rect
                [ Svg.Attributes.id "path-1"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "27"
                , Svg.Attributes.height "27"
                , Svg.Attributes.rx "4"
                ]
                []
            , Svg.filter
                [ Svg.Attributes.x "-3.7%"
                , Svg.Attributes.y "-3.7%"
                , Svg.Attributes.width "107.4%"
                , Svg.Attributes.height "107.4%"
                , Svg.Attributes.filterUnits "objectBoundingBox"
                , Svg.Attributes.id "filter-2"
                ]
                [ Svg.feOffset
                    [ Svg.Attributes.dx "0"
                    , Svg.Attributes.dy "2"
                    , Svg.Attributes.in_ "SourceAlpha"
                    , Svg.Attributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ Svg.Attributes.in_ "shadowOffsetInner1"
                    , Svg.Attributes.in2 "SourceAlpha"
                    , Svg.Attributes.operator "arithmetic"
                    , Svg.Attributes.k2 "-1"
                    , Svg.Attributes.k3 "1"
                    , Svg.Attributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ Svg.Attributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0"
                    , Svg.Attributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ Svg.Attributes.id "Page-1"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Svg.Attributes.id "checkbox_lock_on_inside"
                ]
                [ Svg.g
                    [ Svg.Attributes.id "checkbox_checked"
                    ]
                    [ Svg.use
                        [ Svg.Attributes.fill "#EBEBEB"
                        , Svg.Attributes.fillRule "evenodd"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.fill "black"
                        , Svg.Attributes.fillOpacity "1"
                        , Svg.Attributes.filter "url(#filter-2)"
                        ]
                        []
                    ]
                , Svg.g
                    [ Svg.Attributes.id "icon/premium-lock-shadow"
                    , Svg.Attributes.transform "translate(4.050000, 4.050000)"
                    ]
                    [ Svg.g
                        [ Svg.Attributes.id "icon_premium_locked"
                        , Svg.Attributes.transform "translate(3.040000, 0.271429)"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.d "M10.8889406,8.4420254 L10.8889406,5.41406583 C10.8889406,2.93752663 8.90203465,0.922857143 6.46010875,0.922857143 C4.01774785,0.922857143 2.03105941,2.93752663 2.03105941,5.41406583 L2.03105941,8.4420254 L1.39812057,8.4420254 C0.626196192,8.4420254 0,9.0763794 0,9.85917577 L0,17.0399925 C0,17.8227889 0.626196192,18.4571429 1.39812057,18.4571429 L11.5223144,18.4571429 C12.2942388,18.4571429 12.92,17.8227889 12.92,17.0399925 L12.92,9.85939634 C12.92,9.07659997 12.2942388,8.4420254 11.5223144,8.4420254 L10.8889406,8.4420254 Z M6.8875056,13.8949112 L6.8875056,15.5789491 C6.8875056,15.8187066 6.69588391,16.0128066 6.46010875,16.0128066 C6.22389859,16.0128066 6.0322769,15.8187066 6.0322769,15.5789491 L6.0322769,13.8949112 C5.54876383,13.7173539 5.20271376,13.2490877 5.20271376,12.6972262 C5.20271376,11.9933932 5.76561607,11.4221217 6.46010875,11.4221217 C7.15394892,11.4221217 7.71772125,11.9933932 7.71772125,12.6972262 C7.71772125,13.2497494 7.37101867,13.7180156 6.8875056,13.8949112 L6.8875056,13.8949112 Z M9.21176142,8.4420254 L3.70823858,8.4420254 L3.70823858,5.41406583 C3.70823858,3.87538241 4.94279558,2.62343759 6.46010875,2.62343759 C7.97720442,2.62343759 9.21176142,3.87538241 9.21176142,5.41406583 L9.21176142,8.4420254 L9.21176142,8.4420254 Z"
                            , Svg.Attributes.id "icon_premium_locked-copy"
                            , Svg.Attributes.fill "#E68900"
                            ]
                            []
                        , Svg.rect
                            [ Svg.Attributes.id "Rectangle-9"
                            , Svg.Attributes.fill "#FFFFFF"
                            , Svg.Attributes.x "0.922857143"
                            , Svg.Attributes.y "10.1514286"
                            , Svg.Attributes.width "10.1514286"
                            , Svg.Attributes.height "5.53714286"
                            ]
                            []
                        , Svg.path [ Svg.Attributes.d "M10.8889406,7.51916826 L10.8889406,4.49120869 C10.8889406,2.01466949 8.90203465,0 6.46010875,0 C4.01774785,0 2.03105941,2.01466949 2.03105941,4.49120869 L2.03105941,7.51916826 L1.39812057,7.51916826 C0.626196192,7.51916826 0,8.15352226 0,8.93631863 L0,16.1171353 C0,16.8999317 0.626196192,17.5342857 1.39812057,17.5342857 L11.5223144,17.5342857 C12.2942388,17.5342857 12.92,16.8999317 12.92,16.1171353 L12.92,8.9365392 C12.92,8.15374283 12.2942388,7.51916826 11.5223144,7.51916826 L10.8889406,7.51916826 Z M6.8875056,12.9720541 L6.8875056,14.6560919 C6.8875056,14.8958495 6.69588391,15.0899495 6.46010875,15.0899495 C6.22389859,15.0899495 6.0322769,14.8958495 6.0322769,14.6560919 L6.0322769,12.9720541 C5.54876383,12.7944967 5.20271376,12.3262305 5.20271376,11.774369 C5.20271376,11.0705361 5.76561607,10.4992645 6.46010875,10.4992645 C7.15394892,10.4992645 7.71772125,11.0705361 7.71772125,11.774369 C7.71772125,12.3268922 7.37101867,12.7951584 6.8875056,12.9720541 L6.8875056,12.9720541 Z M9.21176142,7.51916826 L3.70823858,7.51916826 L3.70823858,4.49120869 C3.70823858,2.95252527 4.94279558,1.70058044 6.46010875,1.70058044 C7.97720442,1.70058044 9.21176142,2.95252527 9.21176142,4.49120869 L9.21176142,7.51916826 L9.21176142,7.51916826 Z", Svg.Attributes.fill "#FEC900" ] []
                        ]
                    ]
                ]
            ]
        ]
        |> Html.Styled.fromUnstyled
        |> Icon
