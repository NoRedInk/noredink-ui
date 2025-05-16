module CheckboxIcons exposing
    ( checked
    , checkedDisabled
    , checkedPartially
    , checkedPartiallyDisabled
    , lockOnInside
    , unchecked
    , uncheckedDisabled
    )

import Css
import Css.Global
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


unchecked : String -> Svg
unchecked idSuffix =
    let
        filterId =
            "filter-2" ++ idSuffix

        filterUrl =
            "url(#" ++ filterId ++ ")"
    in
    Nri.Ui.Svg.V1.init viewBox
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.x "-3.7%"
                , SvgAttributes.y "-3.7%"
                , SvgAttributes.width "107.4%"
                , SvgAttributes.height "107.4%"
                , SvgAttributes.filterUnits "objectBoundingBox"
                , SvgAttributes.id filterId
                ]
                [ Svg.feOffset
                    [ SvgAttributes.dx "0"
                    , SvgAttributes.dy "2"
                    , SvgAttributes.in_ "SourceAlpha"
                    , SvgAttributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ SvgAttributes.in_ "shadowOffsetInner1"
                    , SvgAttributes.in2 "SourceAlpha"
                    , SvgAttributes.operator "arithmetic"
                    , SvgAttributes.k2 "-1"
                    , SvgAttributes.k3 "1"
                    , SvgAttributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ SvgAttributes.values "0 0 0 0 0.1 0 0 0 0 0.1 0 0 0 0 0.1 0 0 0 0.1 0"
                    , SvgAttributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            , SvgAttributes.css
                [ Css.hover
                    [ Css.Global.descendants
                        [ Css.Global.rect
                            [ Css.fill Colors.frost
                            , Css.property "stroke" (toCssString Colors.azure)
                            ]
                        ]
                    ]
                ]
            ]
            [ Svg.g
                []
                [ checkboxBackground
                    [ SvgAttributes.fill "#F5F5F5"
                    , SvgAttributes.fillRule "evenodd"
                    , SvgAttributes.stroke (toCssString Colors.gray75)
                    ]
                , checkboxBackground
                    [ SvgAttributes.fill "black"
                    , SvgAttributes.fillOpacity "1"
                    , SvgAttributes.filter filterUrl
                    ]
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


uncheckedDisabled : Svg
uncheckedDisabled =
    Nri.Ui.Svg.V1.init viewBox
        [ Svg.g
            []
            [ checkboxBackground
                [ SvgAttributes.fill (toCssString Colors.gray85)
                , SvgAttributes.fillRule "evenodd"
                , SvgAttributes.stroke (toCssString Colors.gray85)
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


checked : String -> Svg
checked idSuffix =
    let
        filterId =
            "filter-2" ++ idSuffix

        filterUrl =
            "url(#" ++ filterId ++ ")"
    in
    Nri.Ui.Svg.V1.init viewBox
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.x "-3.7%"
                , SvgAttributes.y "-3.7%"
                , SvgAttributes.width "107.4%"
                , SvgAttributes.height "107.4%"
                , SvgAttributes.filterUnits "objectBoundingBox"
                , SvgAttributes.id filterId
                ]
                [ Svg.feOffset
                    [ SvgAttributes.dx "0"
                    , SvgAttributes.dy "2"
                    , SvgAttributes.in_ "SourceAlpha"
                    , SvgAttributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ SvgAttributes.in_ "shadowOffsetInner1"
                    , SvgAttributes.in2 "SourceAlpha"
                    , SvgAttributes.operator "arithmetic"
                    , SvgAttributes.k2 "-1"
                    , SvgAttributes.k3 "1"
                    , SvgAttributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ SvgAttributes.values "0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0.1 0"
                    , SvgAttributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ -- Blue background
              checkboxBackground
                [ SvgAttributes.fill (toCssString Colors.frost)
                , SvgAttributes.fillRule "evenodd"
                , SvgAttributes.stroke (toCssString Colors.azure)
                ]
            , -- the filter (looks like a box shadow inset on the top)
              checkboxBackground
                [ SvgAttributes.fill "black"
                , SvgAttributes.fillOpacity "1"
                , SvgAttributes.filter filterUrl
                ]
            , checkmark Colors.azure
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


checkedDisabled : Svg
checkedDisabled =
    Nri.Ui.Svg.V1.init viewBox
        [ -- Light gray background
          checkboxBackground
            [ SvgAttributes.fill (toCssString Colors.gray85)
            , SvgAttributes.stroke (toCssString Colors.gray85)
            ]
        , checkmark Colors.gray45
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


checkmark : Css.Color -> Svg.Svg msg
checkmark fillColor =
    Svg.g
        [ SvgAttributes.transform "translate(2, 2)"
        , SvgAttributes.fill (toCssString fillColor)
        ]
        [ Svg.path
            [ SvgAttributes.d "M17.2842 5.30178C17.6697 4.9065 18.3028 4.8987 18.6982 5.2842C19.0935 5.66977 19.1013 6.30288 18.7158 6.69826L8.96581 16.6983C8.78045 16.8883 8.52715 16.9969 8.26171 17C8.02945 17.0027 7.80468 16.9242 7.62499 16.7803L7.55077 16.7149L4.30077 13.5332L4.23143 13.458C3.90649 13.0687 3.92277 12.4883 4.28514 12.1182C4.64744 11.7482 5.22691 11.7198 5.62303 12.0362L5.69921 12.1035L8.23339 14.584L17.2842 5.30178Z" ]
            []
        ]


checkedPartially : String -> Svg
checkedPartially idSuffix =
    let
        filterId =
            "filter-2" ++ idSuffix

        filterUrl =
            "url(#" ++ filterId ++ ")"
    in
    Nri.Ui.Svg.V1.init viewBox
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.x "-3.7%"
                , SvgAttributes.y "-3.7%"
                , SvgAttributes.width "107.4%"
                , SvgAttributes.height "107.4%"
                , SvgAttributes.filterUnits "objectBoundingBox"
                , SvgAttributes.id filterId
                ]
                [ Svg.feOffset
                    [ SvgAttributes.dx "0"
                    , SvgAttributes.dy "2"
                    , SvgAttributes.in_ "SourceAlpha"
                    , SvgAttributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ SvgAttributes.in_ "shadowOffsetInner1"
                    , SvgAttributes.in2 "SourceAlpha"
                    , SvgAttributes.operator "arithmetic"
                    , SvgAttributes.k2 "-1"
                    , SvgAttributes.k3 "1"
                    , SvgAttributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ SvgAttributes.values "0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0 0.2 0 0 0 0.1 0"
                    , SvgAttributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ Svg.g
                []
                [ Svg.g
                    []
                    [ checkboxBackground
                        [ SvgAttributes.fill (toCssString Colors.frost)
                        , SvgAttributes.fillRule "evenodd"
                        , SvgAttributes.stroke (toCssString Colors.azure)
                        ]
                    , checkboxBackground
                        [ SvgAttributes.fill "black"
                        , SvgAttributes.fillOpacity "1"
                        , SvgAttributes.filter filterUrl
                        ]
                    ]
                , tilde Colors.azure
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


checkedPartiallyDisabled : Svg
checkedPartiallyDisabled =
    Nri.Ui.Svg.V1.init viewBox
        [ -- Dark gray background
          checkboxBackground
            [ SvgAttributes.fill (toCssString Colors.gray85)
            , SvgAttributes.stroke (toCssString Colors.gray85)
            ]
        , tilde Colors.gray45
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)


tilde : Css.Color -> Svg.Svg msg
tilde fillColor =
    Svg.g
        [ SvgAttributes.transform "translate(2, 2)"
        , SvgAttributes.fill (toCssString fillColor)
        ]
        [ Svg.path
            [ SvgAttributes.d "M17.082 9.60155C17.0745 9.61727 17.0621 9.64281 17.0449 9.67675C17.0103 9.74507 16.9567 9.84723 16.8857 9.97069C16.7425 10.2198 16.5343 10.5466 16.2724 10.8682C15.7091 11.5599 15.0865 11.9998 14.4795 12C14.1754 12 13.8309 11.8939 13.4092 11.6582C12.9816 11.4192 12.5494 11.0909 12.0556 10.709C11.5871 10.3466 11.0502 9.92588 10.497 9.60448C9.93942 9.28056 9.27131 8.99999 8.52048 8.99999C7.039 9.00015 5.89931 10.0758 5.24314 10.8574C4.88856 11.2798 4.60774 11.6966 4.41697 12.0058C4.32106 12.1613 4.24633 12.2927 4.19431 12.3867C4.16826 12.4338 4.14727 12.4726 4.13279 12.5C4.12581 12.5132 4.12039 12.5241 4.11619 12.5322C4.11405 12.5364 4.11171 12.5402 4.11033 12.543C4.10965 12.5443 4.10887 12.5459 4.10838 12.5469V12.5478L4.1074 12.5488L4.06638 12.6426C3.88459 13.1157 4.08671 13.659 4.54881 13.8926C5.0417 14.1417 5.64342 13.944 5.89256 13.4512V13.4502L5.90232 13.4326C5.91104 13.4161 5.92522 13.39 5.94431 13.3555C5.98299 13.2855 6.04182 13.182 6.11912 13.0566C6.27527 12.8035 6.49946 12.4701 6.77439 12.1426C7.37728 11.4245 7.99764 11.0002 8.52048 11C8.77193 11 9.08151 11.0945 9.49216 11.333C9.90716 11.5741 10.3311 11.9035 10.832 12.291C11.3077 12.659 11.8542 13.0809 12.4326 13.4043C13.0168 13.731 13.7059 14 14.4795 14C16.0273 13.9998 17.1651 12.9399 17.8232 12.1318C18.1719 11.7036 18.44 11.2801 18.6201 10.9668C18.7106 10.8093 18.7806 10.6767 18.8291 10.581C18.8533 10.5332 18.8722 10.494 18.8857 10.4658C18.8925 10.4517 18.8983 10.4403 18.9023 10.4316C18.9043 10.4274 18.9059 10.4238 18.9072 10.4209C18.9079 10.4194 18.9087 10.4181 18.9092 10.417V10.415H18.9101C18.9103 10.4147 18.9101 10.4143 18 9.99999L18.9101 10.4141C19.1388 9.91142 18.9166 9.3186 18.414 9.08983C17.9115 8.8612 17.3196 9.08346 17.0908 9.58593L17.082 9.60155Z"
            ]
            []
        ]


viewBox : String
viewBox =
    "0 -1 27 29"


checkboxBackground attrs =
    Svg.rect
        ([ SvgAttributes.x "0"
         , SvgAttributes.y "0"
         , SvgAttributes.width "27"
         , SvgAttributes.height "27"
         , SvgAttributes.strokeWidth "1px"
         , SvgAttributes.strokeLinejoin "round"
         , SvgAttributes.stroke "none"
         , SvgAttributes.rx "4"
         ]
            ++ attrs
        )
        []


{-| -}
lockOnInside : String -> Svg
lockOnInside idSuffix =
    let
        filterId =
            "filter-2" ++ idSuffix

        filterUrl =
            "url(#" ++ filterId ++ ")"
    in
    Nri.Ui.Svg.V1.init viewBox
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.x "-3.7%"
                , SvgAttributes.y "-3.7%"
                , SvgAttributes.width "107.4%"
                , SvgAttributes.height "107.4%"
                , SvgAttributes.filterUnits "objectBoundingBox"
                , SvgAttributes.id filterId
                ]
                [ Svg.feOffset
                    [ SvgAttributes.dx "0"
                    , SvgAttributes.dy "2"
                    , SvgAttributes.in_ "SourceAlpha"
                    , SvgAttributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ SvgAttributes.in_ "shadowOffsetInner1"
                    , SvgAttributes.in2 "SourceAlpha"
                    , SvgAttributes.operator "arithmetic"
                    , SvgAttributes.k2 "-1"
                    , SvgAttributes.k3 "1"
                    , SvgAttributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ SvgAttributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0"
                    , SvgAttributes.in_ "shadowInnerInner1"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttributes.stroke "none"
            , SvgAttributes.strokeWidth "1"
            , SvgAttributes.fill "none"
            , SvgAttributes.fillRule "evenodd"
            ]
            [ Svg.g
                []
                [ Svg.g
                    []
                    [ checkboxBackground
                        [ SvgAttributes.fill "#EBEBEB"
                        , SvgAttributes.fillRule "evenodd"
                        , SvgAttributes.stroke (toCssString Colors.gray75)
                        ]
                    , checkboxBackground
                        [ SvgAttributes.fill "black"
                        , SvgAttributes.fillOpacity "1"
                        , SvgAttributes.filter filterUrl
                        ]
                    ]
                , Svg.g
                    [ SvgAttributes.transform "translate(4.050000, 4.050000)"
                    ]
                    [ Svg.g
                        [ SvgAttributes.transform "translate(3.040000, 0.271429)"
                        ]
                        [ Svg.path
                            [ SvgAttributes.d "M10.8889406,8.4420254 L10.8889406,5.41406583 C10.8889406,2.93752663 8.90203465,0.922857143 6.46010875,0.922857143 C4.01774785,0.922857143 2.03105941,2.93752663 2.03105941,5.41406583 L2.03105941,8.4420254 L1.39812057,8.4420254 C0.626196192,8.4420254 0,9.0763794 0,9.85917577 L0,17.0399925 C0,17.8227889 0.626196192,18.4571429 1.39812057,18.4571429 L11.5223144,18.4571429 C12.2942388,18.4571429 12.92,17.8227889 12.92,17.0399925 L12.92,9.85939634 C12.92,9.07659997 12.2942388,8.4420254 11.5223144,8.4420254 L10.8889406,8.4420254 Z M6.8875056,13.8949112 L6.8875056,15.5789491 C6.8875056,15.8187066 6.69588391,16.0128066 6.46010875,16.0128066 C6.22389859,16.0128066 6.0322769,15.8187066 6.0322769,15.5789491 L6.0322769,13.8949112 C5.54876383,13.7173539 5.20271376,13.2490877 5.20271376,12.6972262 C5.20271376,11.9933932 5.76561607,11.4221217 6.46010875,11.4221217 C7.15394892,11.4221217 7.71772125,11.9933932 7.71772125,12.6972262 C7.71772125,13.2497494 7.37101867,13.7180156 6.8875056,13.8949112 L6.8875056,13.8949112 Z M9.21176142,8.4420254 L3.70823858,8.4420254 L3.70823858,5.41406583 C3.70823858,3.87538241 4.94279558,2.62343759 6.46010875,2.62343759 C7.97720442,2.62343759 9.21176142,3.87538241 9.21176142,5.41406583 L9.21176142,8.4420254 L9.21176142,8.4420254 Z"
                            , SvgAttributes.fill "#E68900"
                            ]
                            []
                        , Svg.rect
                            [ SvgAttributes.fill "#FFFFFF"
                            , SvgAttributes.x "0.922857143"
                            , SvgAttributes.y "10.1514286"
                            , SvgAttributes.width "10.1514286"
                            , SvgAttributes.height "5.53714286"
                            ]
                            []
                        , Svg.path
                            [ SvgAttributes.d "M10.8889406,7.51916826 L10.8889406,4.49120869 C10.8889406,2.01466949 8.90203465,0 6.46010875,0 C4.01774785,0 2.03105941,2.01466949 2.03105941,4.49120869 L2.03105941,7.51916826 L1.39812057,7.51916826 C0.626196192,7.51916826 0,8.15352226 0,8.93631863 L0,16.1171353 C0,16.8999317 0.626196192,17.5342857 1.39812057,17.5342857 L11.5223144,17.5342857 C12.2942388,17.5342857 12.92,16.8999317 12.92,16.1171353 L12.92,8.9365392 C12.92,8.15374283 12.2942388,7.51916826 11.5223144,7.51916826 L10.8889406,7.51916826 Z M6.8875056,12.9720541 L6.8875056,14.6560919 C6.8875056,14.8958495 6.69588391,15.0899495 6.46010875,15.0899495 C6.22389859,15.0899495 6.0322769,14.8958495 6.0322769,14.6560919 L6.0322769,12.9720541 C5.54876383,12.7944967 5.20271376,12.3262305 5.20271376,11.774369 C5.20271376,11.0705361 5.76561607,10.4992645 6.46010875,10.4992645 C7.15394892,10.4992645 7.71772125,11.0705361 7.71772125,11.774369 C7.71772125,12.3268922 7.37101867,12.7951584 6.8875056,12.9720541 L6.8875056,12.9720541 Z M9.21176142,7.51916826 L3.70823858,7.51916826 L3.70823858,4.49120869 C3.70823858,2.95252527 4.94279558,1.70058044 6.46010875,1.70058044 C7.97720442,1.70058044 9.21176142,2.95252527 9.21176142,4.49120869 L9.21176142,7.51916826 L9.21176142,7.51916826 Z"
                            , SvgAttributes.fill "#FEC900"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 27)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 27)
