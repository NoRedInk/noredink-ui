module Nri.Ui.AssignmentIcon.V2 exposing
    ( diagnostic, diagnosticCircled, planningDiagnosticCircled, unitDiagnosticCircled
    , practice, practiceCircled
    , quiz, quizCircled, passageQuizCircled
    , quickWrite, guidedDraft, guidedEssay, guidedShortResponse, peerReview, selfReview, dailyWriting, novels, texts, genres, genresCircled
    , quickWriteCircled, guidedDraftCircled, guidedEssayCircled, guidedShortResponseCircled, peerReviewCircled, selfReviewCircled, dailyWritingCircled, novelsCircled, gradingAssistantCircled, textsCircled
    , submitting, rating, revising
    , startPrimary, startSecondary
    , assessment, standards, writing, modules
    )

{-|


### Patch changes

  - change circular path elements to circle elements using encircle helper


# Quiz Engine

@docs diagnostic, diagnosticCircled, planningDiagnosticCircled, unitDiagnosticCircled
@docs practice, practiceCircled
@docs quiz, quizCircled, passageQuizCircled


# Writing

@docs quickWrite, guidedDraft, guidedEssay, guidedShortResponse, peerReview, selfReview, dailyWriting, novels, texts, genres, genresCircled
@docs quickWriteCircled, guidedDraftCircled, guidedEssayCircled, guidedShortResponseCircled, peerReviewCircled, selfReviewCircled, dailyWritingCircled, novelsCircled, gradingAssistantCircled, textsCircled


# Stages

@docs submitting, rating, revising


# Start

@docs startPrimary, startSecondary


# Activities

@docs assessment, standards, writing, modules


# Example usage

    import Html.Styled exposing (Html)
    import Nri.Ui.AssignmentIcon.V2 as AssignmentIcon
    import Nri.Ui.Colors.V1 as Colors
    import Nri.Ui.Svg.V1 as Svg

    view : Html msg
    view =
        AssignmentIcon.diagnostic
            |> Svg.withColor Colors.lichen
            |> Svg.toHtml

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attributes


encircle : Float -> (Svg Never -> List (Svg Never)) -> Nri.Ui.Svg.V1.Svg
encircle diameter circleToIcon =
    let
        diameterStr =
            String.fromFloat diameter

        radius =
            diameter / 2

        radiusStr =
            String.fromFloat radius
    in
    Svg.circle
        [ Attributes.cx radiusStr
        , Attributes.cy radiusStr
        , Attributes.r radiusStr
        ]
        []
        |> circleToIcon
        |> Nri.Ui.Svg.V1.init
            ("0 0 "
                ++ diameterStr
                ++ " "
                ++ diameterStr
            )


{-| -}
diagnostic : Nri.Ui.Svg.V1.Svg
diagnostic =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M7.99203 14.3635V16.4163H2.70103C1.18933 16.4163 0.00183105 17.6038 0.00183105 19.0081V23.0042C0.00183105 23.76 0.540881 24.2991 1.29673 24.2991C6.58773 24.2991 6.80473 30.9946 1.29673 30.9946C0.540881 30.9946 0.00183105 31.5336 0.00183105 32.2895V36.2856C0.00183105 37.6899 1.18933 38.8774 2.70103 38.8774H6.58773C7.34358 38.8774 7.99203 38.3383 7.99203 37.5825C7.99203 32.2915 14.58 32.182 14.58 37.5825C14.58 38.3383 15.1191 38.8774 15.8749 38.8774H19.871C21.3827 38.8774 22.5702 37.6899 22.5702 36.2856V30.9946H24.5136C28.8339 30.9946 28.8339 24.2991 24.5136 24.2991H22.5702V19.0081C22.5702 17.6038 21.3827 16.4163 19.871 16.4163H14.58V14.3635C14.58 10.0432 7.99203 10.0432 7.99203 14.3635Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M30.129 14.3635V16.4163H24.838C24.5138 16.4163 24.1896 16.4163 23.8654 16.5237C24.297 17.2795 24.6212 18.1428 24.6212 19.0081V22.2483C31.5332 22.3557 31.5332 32.9398 24.6212 33.0473V36.2876C24.6212 37.1509 24.297 38.0161 23.8654 38.6626C24.1896 38.8794 24.5138 38.8794 24.838 38.8794H28.8341C29.59 38.8794 30.129 38.3403 30.129 37.5845C30.129 32.2935 36.717 32.184 36.717 37.5845C36.717 38.3403 37.3655 38.8794 38.1213 38.8794H42.1174C43.5217 38.8794 44.7092 37.6919 44.7092 36.2876V30.9966H46.6526C51.0803 30.9966 51.0803 24.3011 46.6526 24.3011H44.7092V19.0101C44.7092 17.6058 43.5217 16.4183 42.1174 16.4183H36.7169V14.3655C36.7169 10.0452 30.1289 10.0452 30.1289 14.3655L30.129 14.3635Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
diagnosticCircled : Nri.Ui.Svg.V1.Svg
diagnosticCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M35 70C54.33 70 70 54.33 70 35C70 15.67 54.33 0 35 0C15.67 0 0 15.67 0 35C0 54.33 15.67 70 35 70"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M19.992 24.3635V26.4163H14.701C13.1893 26.4163 12.0018 27.6038 12.0018 29.0081V33.0042C12.0018 33.76 12.5409 34.2991 13.2967 34.2991C18.5877 34.2991 18.8047 40.9946 13.2967 40.9946C12.5409 40.9946 12.0018 41.5336 12.0018 42.2895V46.2856C12.0018 47.6899 13.1893 48.8774 14.701 48.8774H18.5877C19.3436 48.8774 19.992 48.3383 19.992 47.5825C19.992 42.2915 26.58 42.182 26.58 47.5825C26.58 48.3383 27.1191 48.8774 27.8749 48.8774H31.871C33.3827 48.8774 34.5702 47.6899 34.5702 46.2856V40.9946H36.5136C40.8339 40.9946 40.8339 34.2991 36.5136 34.2991H34.5702V29.0081C34.5702 27.6038 33.3827 26.4163 31.871 26.4163H26.58V24.3635C26.58 20.0432 19.992 20.0432 19.992 24.3635Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M42.129 24.3635V26.4163H36.838C36.5138 26.4163 36.1896 26.4163 35.8654 26.5237C36.297 27.2795 36.6212 28.1428 36.6212 29.0081V32.2483C43.5332 32.3557 43.5332 42.9398 36.6212 43.0473V46.2876C36.6212 47.1509 36.297 48.0161 35.8654 48.6626C36.1896 48.8794 36.5138 48.8794 36.838 48.8794H40.8341C41.59 48.8794 42.129 48.3403 42.129 47.5845C42.129 42.2935 48.717 42.184 48.717 47.5845C48.717 48.3403 49.3655 48.8794 50.1213 48.8794H54.1174C55.5217 48.8794 56.7092 47.6919 56.7092 46.2876V40.9966H58.6526C63.0803 40.9966 63.0803 34.3011 58.6526 34.3011H56.7092V29.0101C56.7092 27.6058 55.5217 26.4183 54.1174 26.4183H48.7169V24.3655C48.7169 20.0452 42.1289 20.0452 42.1289 24.3655L42.129 24.3635Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
peerReview : Nri.Ui.Svg.V1.Svg
peerReview =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.d "M24.2703 3.63251C31.7828 3.63251 38.1955 3.63251 40.5293 5.98016C42.8632 8.32781 42.8632 10.7704 42.8632 18.3274L37.551 15.6556"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M25.5988 46.3827C18.086 46.3827 11.6735 46.3827 9.33963 44.035C7.00574 41.6872 7.00574 39.2447 7.00574 31.6878L12.318 34.3595"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M40.9487 29.5192C40.9487 26.51 38.5092 24.0705 35.5 24.0705C32.4907 24.0705 30.0513 26.51 30.0513 29.5192C30.0513 32.5284 32.4907 34.9679 35.5 34.9679C38.5092 34.9679 40.9487 32.5284 40.9487 29.5192Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M43.1282 42.5962C43.1282 38.3832 39.713 34.968 35.5 34.968C31.2871 34.968 27.8718 38.3832 27.8718 42.5962"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18.9487 12.8526C18.9487 9.84332 16.5092 7.40384 13.5 7.40384C10.4907 7.40384 8.05127 9.84332 8.05127 12.8526C8.05127 15.8618 10.4907 18.3013 13.5 18.3013C16.5092 18.3013 18.9487 15.8618 18.9487 12.8526Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.1282 25.9295C21.1282 21.7165 17.713 18.3013 13.5 18.3013C9.28709 18.3013 5.87183 21.7165 5.87183 25.9295"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
practice : Nri.Ui.Svg.V1.Svg
practice =
    Nri.Ui.Svg.V1.init "0 0 46 46"
        [ Svg.path
            [ Attributes.d "M29.5234 3.20834C27.4713 2.5324 25.2784 2.16667 23 2.16667C11.4941 2.16667 2.16669 11.4941 2.16669 23C2.16669 34.5058 11.4941 43.8333 23 43.8333C34.5059 43.8333 43.8334 34.5058 43.8334 23C43.8334 20.8225 43.4992 18.723 42.8796 16.75"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M33.4166 23C33.4166 28.7529 28.7529 33.4167 23 33.4167C17.247 33.4167 12.5833 28.7529 12.5833 23C12.5833 17.247 17.247 12.5833 23 12.5833"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M38.625 7.37501L23 23M38.625 7.37501V2.16667M38.625 7.37501H43.8333"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
quiz : Nri.Ui.Svg.V1.Svg
quiz =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.d "M14.8097 5.75182C11.2866 5.8575 9.18613 6.24964 7.73268 7.70442C5.74292 9.69604 5.74292 12.9015 5.74292 19.3124V34.0453C5.74292 40.4563 5.74292 43.6617 7.73268 45.6533C9.72245 47.6449 12.9249 47.6449 19.3299 47.6449H30.6524C37.0573 47.6449 40.2597 47.6449 42.2495 45.6533C44.2393 43.6617 44.2393 40.4563 44.2393 34.0453V19.3124C44.2393 12.9015 44.2393 9.69604 42.2495 7.70444C40.7962 6.24964 38.6956 5.8575 35.1725 5.75182"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.8009 6.31793C14.8009 4.1293 16.5752 2.35507 18.7638 2.35507H31.2185C33.4071 2.35507 35.1814 4.1293 35.1814 6.31793C35.1814 8.50657 33.4071 10.2808 31.2185 10.2808H18.7638C16.5752 10.2808 14.8009 8.50657 14.8009 6.31793Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.5453 20.471H21.6033"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.3967 22.7355C28.3967 22.7355 29.529 22.7355 30.6612 25C30.6612 25 34.2577 19.3388 37.4547 18.2065"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.5453 34.058H21.6033"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.3967 36.3225C28.3967 36.3225 29.529 36.3225 30.6612 38.587C30.6612 38.587 34.2577 32.9257 37.4547 31.7935"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
quickWrite : Nri.Ui.Svg.V1.Svg
quickWrite =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M32.2832 13.3163L5.102 13.3163C4.27357 13.3163 3.602 13.9879 3.602 14.8163C3.602 15.6448 4.27357 16.3163 5.102 16.3163L29.2832 16.3163L32.2832 13.3163ZM16.8098 31.6224L26.5306 31.6224C27.359 31.6224 28.0306 30.9509 28.0306 30.1224C28.0306 29.294 27.359 28.6224 26.5306 28.6224L19.8056 28.6224L17.8736 30.5544C17.4879 30.94 17.1346 31.2939 16.8098 31.6224ZM14.0331 31.6224C14.7072 30.892 15.5043 30.0951 16.4579 29.1417L16.4579 29.1417L16.9771 28.6224L9.18363 28.6225C8.3552 28.6225 7.68363 29.294 7.68363 30.1225C7.68363 30.9509 8.3552 31.6225 9.18363 31.6224H14.0331ZM21.9076 26.5204L31.6326 26.5204C32.461 26.5204 33.1326 25.8488 33.1326 25.0204C33.1326 24.192 32.461 23.5204 31.6326 23.5204L24.9076 23.5204L21.9076 26.5204ZM22.0792 23.5204L19.0792 26.5204L2.04077 26.5204C1.21234 26.5204 0.540771 25.8488 0.540771 25.0204C0.540771 24.192 1.21234 23.5204 2.04077 23.5204L22.0792 23.5204ZM24.1812 21.4184H10.8844C10.056 21.4184 9.38439 20.7468 9.38439 19.9184C9.38439 19.0899 10.056 18.4184 10.8844 18.4184H27.1812L24.1812 21.4184ZM29.8746 18.5534L27.0097 21.4184H29.2517C30.0802 21.4184 30.7517 20.7468 30.7517 19.9184C30.7517 19.3122 30.3921 18.79 29.8746 18.5534ZM34.0788 14.3492L32.1117 16.3163H32.653C33.4814 16.3163 34.153 15.6448 34.153 14.8163C34.153 14.6532 34.127 14.4962 34.0788 14.3492Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M17.8736 30.5544L35.3224 13.1056L41.5067 19.2899L24.0579 36.7388C21.7366 39.0605 20.564 40.2109 19.1579 40.9966C18.444 41.3955 17.2215 41.8054 15.7401 42.2175C14.3802 42.596 12.8203 42.972 11.2713 43.3411C11.6406 41.7915 12.0162 40.2322 12.3947 38.8721C12.807 37.3904 13.2169 36.1681 13.6158 35.4543C14.4014 34.0482 15.5519 32.8756 17.8736 30.5544ZM36.7366 11.6914L42.9209 17.8757L46.7192 14.0772C48.427 12.3695 48.4269 9.60073 46.7192 7.89299C45.0116 6.18537 42.2428 6.1853 40.5349 7.89305L36.7366 11.6914Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
guidedDraft : Nri.Ui.Svg.V1.Svg
guidedDraft =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.d "M39.2281 29.6963C39.8492 29.0233 40.1596 28.6871 40.4896 28.4908C41.2856 28.0173 42.2659 28.0025 43.0752 28.4519C43.4109 28.6381 43.7309 28.965 44.3709 29.619C45.0111 30.2729 45.3311 30.5998 45.5134 30.9425C45.9531 31.7692 45.9388 32.7706 45.4752 33.584C45.2831 33.921 44.954 34.2381 44.2952 34.8725L36.459 42.4202C35.2109 43.6223 34.5867 44.2235 33.8067 44.5281C33.0267 44.8327 32.1694 44.8104 30.4544 44.7654L30.2213 44.7594C29.6992 44.7458 29.4381 44.739 29.2863 44.5667C29.1346 44.3946 29.1552 44.1285 29.1967 43.5969L29.2192 43.3081C29.3359 41.8113 29.3942 41.0629 29.6865 40.3902C29.9788 39.7175 30.4829 39.1713 31.4913 38.0788L39.2281 29.6963Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M45.8333 21.875V20.2448C45.8333 16.2039 45.8333 14.1835 44.6129 12.9281C43.3925 11.6728 41.4283 11.6728 37.5 11.6728H33.1695C31.2583 11.6728 31.2425 11.6691 29.5239 10.809L22.5831 7.33571C19.685 5.8855 18.236 5.16039 16.6924 5.21079C15.1488 5.26119 13.7474 6.07933 10.9446 7.71564L8.38658 9.2091C6.32786 10.411 5.29848 11.012 4.73254 12.0117C4.16663 13.0115 4.16663 14.229 4.16663 16.664V33.7827C4.16663 36.9821 4.16663 38.5819 4.87967 39.4723C5.35415 40.0646 6.01904 40.4629 6.75413 40.595C7.85883 40.7933 9.21138 40.0037 11.9164 38.4244C13.7532 37.3521 15.521 36.2383 17.7184 36.5404C19.5597 36.7933 21.2708 37.9552 22.9166 38.7787"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.6666 5.20833V36.4583"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M31.25 11.4583V28.125"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
guidedEssay : Nri.Ui.Svg.V1.Svg
guidedEssay =
    Nri.Ui.Svg.V1.init "0 0 54 55"
        [ Svg.g
            []
            [ Svg.path
                [ Attributes.d "M27 20C22.0099 20 18 24.1338 18 29.2816C18 34.4278 27 47 27 47C27 47 36 34.4278 36 29.2816C36 24.1338 31.9901 20 27 20ZM27 33.4992C24.7097 33.4992 22.9088 31.6435 22.9088 29.28C22.9088 26.9181 24.7082 25.0608 27 25.0608C29.2903 25.0608 31.0912 26.9165 31.0912 29.28C31.0912 31.6435 29.2903 33.4992 27 33.4992Z"
                ]
                []
            , Svg.path
                [ Attributes.d "M34.9222 0.799927H12.41C8.54623 0.799927 5.40002 3.94613 5.40002 7.80993V47.1899C5.40002 51.0537 8.54623 54.1999 12.41 54.1999H41.113C44.9768 54.1999 48.123 51.0537 48.123 47.1899V15.5503L34.9222 0.799927ZM35.8451 6.84531L42.7129 14.5226H35.8451V6.84531ZM41.1087 50.8623H12.41C10.3878 50.8623 8.74193 49.2165 8.74193 47.1899V7.80993C8.74193 5.78766 10.3878 4.13756 12.41 4.13756H32.5075V17.856H44.7811V47.1899C44.7811 49.2165 43.1342 50.8623 41.1087 50.8623Z"
                ]
                []
            ]
        ]


{-| -}
guidedShortResponse : Nri.Ui.Svg.V1.Svg
guidedShortResponse =
    Nri.Ui.Svg.V1.init "0 0 40 40"
        [ Svg.path
            [ Attributes.d "M11.2188 2.3522C8.83941 2.3522 6.56934 3.84174 5.61917 6.19262C4.73488 8.38044 5.21359 10.2423 6.21031 11.8366C7.02749 13.1437 8.21937 14.3103 9.29464 15.3628L9.29515 15.3633C9.50003 15.5638 9.70066 15.7602 9.89366 15.9526L9.89469 15.9537C10.2523 16.308 10.7273 16.5036 11.2188 16.5036C11.7103 16.5036 12.1854 16.308 12.543 15.9536C12.7255 15.7727 12.9147 15.5882 13.1077 15.3999C14.1947 14.3399 15.4033 13.1612 16.2291 11.8375C17.2246 10.2416 17.7016 8.37774 16.8184 6.19262C15.8683 3.84174 13.5982 2.3522 11.2188 2.3522ZM11.2179 6.13688C9.84404 6.13688 8.73034 7.31564 8.73034 8.7697C8.73034 10.2237 9.84404 11.4025 11.2179 11.4025C12.5916 11.4025 13.7054 10.2237 13.7054 8.7697C13.7054 7.31564 12.5916 6.13688 11.2179 6.13688Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 6H35"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5.15454 36.3156H21.0146"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.0146 13.6733H35.0088"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5 21H35"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5 29H35"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
selfReview : Nri.Ui.Svg.V1.Svg
selfReview =
    Nri.Ui.Svg.V1.init "0 0 50 50"
        [ Svg.path
            [ Attributes.d "M13.3297 31.7935V13.6775C13.3297 7.42432 18.399 2.35507 24.6522 2.35507C30.9053 2.35507 35.9746 7.42432 35.9746 13.6775V31.7935C35.9746 35.5405 35.1927 36.3225 31.4457 36.3225H17.8587C14.1117 36.3225 13.3297 35.5405 13.3297 31.7935Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.1232 11.413L22.3877 9.14854"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M22.3877 17.0743L26.9167 12.5453"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M42.7681 15.942V47.6449M6.53625 15.942V47.6449"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M6.53625 43.1159H42.7681"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M6.53625 25H13.3297M35.9747 25H42.7681"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
dailyWriting : Nri.Ui.Svg.V1.Svg
dailyWriting =
    Nri.Ui.Svg.V1.init "0 0 40 40"
        [ Svg.path
            [ Attributes.d "M26.6666 3.33333V9.99999M13.3333 3.33333V9.99999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.6667 6.66667H18.3333C12.0479 6.66667 8.90525 6.66667 6.95262 8.61929C5 10.5719 5 13.7146 5 20V23.3333C5 29.6187 5 32.7615 6.95262 34.714C8.90525 36.6667 12.0479 36.6667 18.3333 36.6667H21.6667C27.952 36.6667 31.0948 36.6667 33.0473 34.714C35 32.7615 35 29.6187 35 23.3333V20C35 13.7146 35 10.5719 33.0473 8.61929C31.0948 6.66667 27.952 6.66667 21.6667 6.66667Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5 16.6667H35"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
novels : Nri.Ui.Svg.V1.Svg
novels =
    Nri.Ui.Svg.V1.init "0 0 40 40"
        [ Svg.path
            [ Attributes.d "M34.1666 28.2143V16.6667C34.1666 10.3813 34.1666 7.23858 32.2139 5.28594C30.2614 3.33333 27.1186 3.33333 20.8333 3.33333H19.1666C12.8812 3.33333 9.7385 3.33333 7.78587 5.28594C5.83325 7.23858 5.83325 10.3813 5.83325 16.6667V32.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M34.1666 28.3333H9.99992C7.69874 28.3333 5.83325 30.1988 5.83325 32.5C5.83325 34.8012 7.69874 36.6667 9.99992 36.6667H34.1666"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M34.1667 36.6667C31.8655 36.6667 30 34.8012 30 32.5C30 30.1988 31.8655 28.3333 34.1667 28.3333"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M25 11.6667H15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 18.3333H15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
texts : Nri.Ui.Svg.V1.Svg
texts =
    Nri.Ui.Svg.V1.init "0 0 40 43"
        [ Svg.path
            [ Attributes.d "M32.9999 11.1207C34.2297 11.3776 35.1517 11.8126 35.8896 12.5492C37.6919 14.3484 37.6919 17.2442 37.6919 23.0357V29.1786C37.6919 34.9701 37.6919 37.866 35.8896 39.665C34.0872 41.4643 31.1862 41.4643 25.3842 41.4643H23.8459C18.0439 41.4643 15.143 41.4643 13.3406 39.665C12.5475 38.8734 12.1034 37.8695 11.8547 36.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.4999 6.53833C29.6724 6.79815 30.5597 7.22868 31.2744 7.94206C33.0767 9.74127 33.0767 12.637 33.0767 18.4286V24.5714C33.0767 30.3629 33.0767 33.2588 31.2744 35.0579C29.472 36.8572 26.571 36.8572 20.769 36.8572H19.2307C13.4288 36.8572 10.5278 36.8572 8.72539 35.0579C7.95316 34.287 7.51179 33.3149 7.25952 32"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.4613 19.9643V13.8214C28.4613 8.02989 28.4613 5.13413 26.659 3.33492C24.8565 1.53572 21.9556 1.53572 16.1536 1.53572H14.6153C8.81351 1.53572 5.91259 1.53572 4.11017 3.3349C2.30774 5.13408 2.30773 8.02983 2.30768 13.8213L2.30762 19.9641C2.30757 25.7558 2.30754 28.6515 4.10997 30.4508C5.91239 32.25 8.81336 32.25 14.6153 32.25H16.1536C21.9556 32.25 24.8565 32.25 26.659 30.4508C28.4613 28.6517 28.4613 25.7558 28.4613 19.9643Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.23071 9.21429H21.5384M9.23071 16.8929H21.5384M9.23071 24.5714H15.3846"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
genres : Nri.Ui.Svg.V1.Svg
genres =
    Nri.Ui.Svg.V1.init "0 0 40 40"
        [ Svg.path
            [ Attributes.d "M17.5836 5C11.163 5.01157 7.80075 5.1692 5.65205 7.31788C3.33325 9.6367 3.33325 13.3687 3.33325 20.8328C3.33325 28.297 3.33325 32.029 5.65205 34.3478C7.97087 36.6667 11.7029 36.6667 19.1671 36.6667C26.6311 36.6667 30.3633 36.6667 32.6821 34.3478C34.8308 32.1992 34.9884 28.837 34.9999 22.4163"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18.4261 21.6667C17.2204 6.44391 28.0039 2.12589 36.6342 3.60731C36.9827 8.65226 34.5142 10.5414 29.8132 11.4137C30.7211 12.3623 32.3251 13.5559 32.1521 15.048C32.0289 16.1098 31.3077 16.6307 29.8657 17.6727C26.7052 19.956 23.0441 21.3958 18.4261 21.6667"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15 28.3333C18.3333 19.1667 21.6007 16.0606 25 13.3333"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
genresCircled : Nri.Ui.Svg.V1.Svg
genresCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M70 35C70 15.67 54.33 0 35 0C15.67 0 0 15.67 0 35C0 54.33 15.67 70 35 70C54.33 70 70 54.33 70 35Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M32.5837 20C26.1631 20.0116 22.8009 20.1692 20.6522 22.3179C18.3334 24.6367 18.3334 28.3687 18.3334 35.8328C18.3334 43.297 18.3334 47.029 20.6522 49.3478C22.971 51.6667 26.703 51.6667 34.1672 51.6667C41.6312 51.6667 45.3634 51.6667 47.6822 49.3478C49.8309 47.1992 49.9885 43.837 50 37.4163"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M33.4261 36.6667C32.2204 21.4439 43.0039 17.1259 51.6342 18.6073C51.9827 23.6523 49.5142 25.5414 44.8132 26.4137C45.7211 27.3623 47.3251 28.5559 47.1521 30.048C47.0289 31.1098 46.3077 31.6307 44.8657 32.6727C41.7052 34.956 38.0441 36.3958 33.4261 36.6667Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M30 43.3333C33.3333 34.1667 36.6007 31.0606 40 28.3333"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
submitting : Nri.Ui.Svg.V1.Svg
submitting =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.fillOpacity ".5"
            , Attributes.d "M0 1.875v1.406h22.5V1.875H0zm0 3.867v1.406h24.32V5.742H0zM0 9.61v1.407h15.117V9.609H0zm0 5.625v1.407h24.32v-1.407H0zm0 3.868v1.406h23.125v-1.406H0zm0 3.867v1.406h13.75v-1.406H0z"
            ]
            []
        , Svg.path
            [ Attributes.fill "#FFF"
            , Attributes.d "M13.15 23.552l.867-5.111 6.325-12.527 1.15-.069.71-1.69 1.909.877-.702 1.715.338.924-6.827 12.878-3.178 3.069z"
            ]
            []
        , Svg.path
            [ Attributes.d "M24.32 5.78a.906.906 0 0 0-.405-1.249l-1.181-.602c-.237-.12-.444-.151-.711-.064-.268.087-.417.234-.538.47l-.481.945c-.178.058-.297-.002-.475.056-.267.087-.417.234-.537.47l-.662 1.3-.945-.482a.453.453 0 0 0-.624.203l-3.01 5.906a.453.453 0 0 0 .203.624c.058.179.236.12.325.092.09-.03.268-.087.328-.205l2.74-5.523.472.24-5.477 10.75c-.06.118-.031.208-.091.326l-.625 4.146c-.062.414.143.742.526 1.012.236.12.444.151.711.064.178-.058.268-.087.328-.205l2.987-2.942c.089-.029.15-.147.21-.265l6.62-12.995a.997.997 0 0 0-.169-1.127l.482-.945zm-2.008-1.024l1.182.602-.482.945-1.181-.602.481-.945zm-8.739 18.612l.591-3.642 2.127 1.083-2.718 2.559zm3.228-3.415L14.44 18.75l4.695-9.214 2.362 1.204-4.695 9.214zm5.116-10.041l-2.362-1.204 1.264-2.48 2.363 1.203-1.265 2.48z"
            ]
            []
        ]


{-| -}
rating : Nri.Ui.Svg.V1.Svg
rating =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.d "M4.961 19.785h-1.59C1.526 19.785 0 18.258 0 16.415V6.079c0-1.845 1.527-3.371 3.371-3.371H13.77c.318 0 .573.255.573.573a.571.571 0 0 1-.573.573H3.37c-1.21 0-2.195.986-2.195 2.195v10.336c0 1.21.986 2.195 2.195 2.195h2.194c.318 0 .574.255.574.573v3.56l4.165-4.006a.604.604 0 0 1 .415-.159h9.603c.891 0 1.718-.573 2.067-1.4a.586.586 0 0 1 .764-.318.586.586 0 0 1 .318.764 3.425 3.425 0 0 1-3.149 2.13h-9.35l-4.992 4.77a.604.604 0 0 1-.415.16c-.062 0-.159 0-.221-.032-.257-.035-.383-.257-.383-.48v-4.357zm20.036-11.99v.159l-.923 5.724c-.19 1.463-1.24 2.323-2.8 2.323h-6.806a2.36 2.36 0 0 1-1.652-.668c-.19.16-.446.255-.733.255H9.857a1.14 1.14 0 0 1-1.144-1.144V7.319a1.14 1.14 0 0 1 1.144-1.144h2.226c.35 0 .636.159.858.381h.032c.096-.032 2.513-.732 2.513-2.766V1.024c0-.255.159-.477.413-.54.064-.032 1.463-.446 2.482.318.636.477.985 1.272.985 2.385v2.29h3.212c1.368-.067 2.419.983 2.419 2.318zm-12.88-.509H9.922v7.124h2.195V7.286zm11.733.509c0-.7-.54-1.24-1.24-1.24h-3.785a.572.572 0 0 1-.573-.574V3.12c0-.699-.16-1.209-.509-1.431-.318-.222-.732-.255-1.017-.222v2.289c0 2.925-3.212 3.848-3.339 3.88h-.032v5.946c0 .667.54 1.24 1.241 1.24h6.806c1.494 0 1.622-.985 1.653-1.303v-.032l.795-5.691z"
            ]
            []
        ]


{-| -}
revising : Nri.Ui.Svg.V1.Svg
revising =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.d "M23.056 12.001h.58C23.637 5.893 18.64.926 12.5.926a11.169 11.169 0 0 0-10.214 6.66L0 7.128 1.652 12 5.07 8.142l-2.206-.44C4.93 3.184 9.831.71 14.668 1.74c4.837 1.03 8.32 5.291 8.388 10.261zM12.5 23.49a10.593 10.593 0 0 0 9.637-6.215l-2.206-.441 3.417-3.858L25 17.845l-2.286-.457a11.169 11.169 0 0 1-10.214 6.66c-6.14 0-11.137-4.968-11.137-11.073h.581c.03 5.833 4.754 10.537 10.556 10.514z"
            ]
            []
        , Svg.path
            [ Attributes.fillOpacity ".5"
            , Attributes.d "M5.833 13.348v1.068h13.555v-1.068H5.833zm0 2.938v1.068h12.652v-1.068H5.833zm0 2.938v1.068H11.4v-1.068H5.833z"
            ]
            []
        , Svg.path
            [ Attributes.fill "#FFF"
            , Attributes.d "M10.958 19.656l.648-3.88 4.769-9.505.87-.051.533-1.282 1.445.668-.529 1.301.257.702-5.147 9.77-2.4 2.328z"
            ]
            []
        , Svg.path
            [ Attributes.d "M19.382 6.173a.691.691 0 0 0-.308-.948l-.894-.458a.642.642 0 0 0-.537-.05.642.642 0 0 0-.406.356l-.363.717c-.135.044-.224-.002-.359.042a.642.642 0 0 0-.406.356l-.499.986-.715-.366a.341.341 0 0 0-.472.153l-2.268 4.482a.346.346 0 0 0 .154.474c.044.135.179.091.246.07.067-.022.202-.066.248-.156l2.065-4.19.357.183-4.129 8.156c-.045.09-.023.158-.068.247l-.468 3.148c-.046.315.11.564.4.769a.642.642 0 0 0 .537.05c.135-.044.202-.066.247-.156L14 17.808c.067-.022.112-.112.158-.201l4.99-9.86a.76.76 0 0 0-.129-.856l.363-.718zm-1.52-.779l.895.459-.363.717-.894-.459.363-.717zm-6.585 14.122l.442-2.765 1.61.826-2.052 1.94zm2.436-2.589l-1.787-.917 3.539-6.99 1.788.916-3.54 6.991zM17.57 9.31l-1.788-.917.953-1.883 1.788.917-.953 1.883z"
            ]
            []
        ]


{-| -}
startPrimary : Nri.Ui.Svg.V1.Svg
startPrimary =
    let
        toIcon circle =
            [ circle
            , Svg.path [ Attributes.fill "#fff", Attributes.d "M21.0869565,33.8146977 C20.6577447,34.0617674 20.1248751,34.0617674 19.6956522,33.8146977 C19.2664403,33.5683165 19,33.1074898 19,32.61405 L19,17.38595 C19,16.889723 19.2664403,16.4316724 19.6956522,16.1853023 C20.1248751,15.9382326 20.6577447,15.9382326 21.0869565,16.1853023 L34.3043478,23.8007347 C34.7335708,24.0478044 35,24.5051666 35,25.002082 C35,25.4955219 34.7363534,25.9535725 34.3043478,26.1999537 L21.0869565,33.8146977 Z" ] []
            ]
    in
    encircle 50 toIcon


{-| -}
startSecondary : Nri.Ui.Svg.V1.Svg
startSecondary =
    Nri.Ui.Svg.V1.init "0 60 50 51"
        [ Svg.g [ Attributes.transform "translate(0.000000, 60.000000)" ]
            [ Svg.path
                [ Attributes.d "M0,25.3650791 C0,39.1738284 11.19375,50.3650803 25,50.3650803 C38.80875,50.3650803 50,39.1738284 50,25.3650791 C50,11.5588298 38.80625,0.365080324 25,0.365080324 C11.19375,0.365080324 0,11.5588298 0,25.3650791 Z"
                , Attributes.fill "#fff"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentcolor"
                , Attributes.strokeWidth "3"
                , Attributes.d "M1.5,25.3650791 C1.5,38.3445152 12.021291,48.8650803 25,48.8650803 C37.9803224,48.8650803 48.5,38.3454018 48.5,25.3650791 C48.5,12.3872569 37.9778229,1.86508032 25,1.86508032 C12.0221771,1.86508032 1.5,12.3872569 1.5,25.3650791 Z"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.fill "currentcolor"
                , Attributes.d "M20.946522,34.4104003 C20.5071165,34.6639342 19.9615913,34.6639342 19.5221743,34.4104003 C19.0827687,34.1575729 18.8100004,33.6846894 18.8100004,33.1783395 L18.8100004,17.5518211 C18.8100004,17.0426111 19.0827687,16.5725764 19.5221743,16.3197604 C19.9615913,16.0662265 20.5071165,16.0662265 20.946522,16.3197604 L34.4778257,24.1344382 C34.9172427,24.3879721 35.1899996,24.8573004 35.1899996,25.3672168 C35.1899996,25.8735667 34.9200914,26.3436015 34.4778257,26.5964289 L20.946522,34.4104003 Z"
                ]
                []
            ]
        ]


{-| -}
assessment : Nri.Ui.Svg.V1.Svg
assessment =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.2916 36.0417V50.625C17.2916 51.5956 17.2916 52.081 17.4502 52.4639C17.6616 52.9744 18.0672 53.38 18.5777 53.5915C18.9606 53.75 19.4459 53.75 20.4166 53.75C21.3873 53.75 21.8727 53.75 22.2555 53.5915C22.766 53.38 23.1716 52.9744 23.383 52.4639C23.5416 52.081 23.5416 51.5956 23.5416 50.625V36.0417C23.5416 35.071 23.5416 34.5856 23.383 34.2027C23.1716 33.6923 22.766 33.2867 22.2555 33.0752C21.8727 32.9167 21.3873 32.9167 20.4166 32.9167C19.4459 32.9167 18.9606 32.9167 18.5777 33.0752C18.0672 33.2867 17.6616 33.6923 17.4502 34.2027C17.2916 34.5856 17.2916 35.071 17.2916 36.0417Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M31.875 40.2083V50.624C31.875 51.5946 31.875 52.08 32.0335 52.4629C32.245 52.9733 32.6506 53.379 33.161 53.5904C33.544 53.749 34.0294 53.749 35 53.749C35.9706 53.749 36.456 53.749 36.839 53.5904C37.3494 53.379 37.755 52.9733 37.9665 52.4629C38.125 52.08 38.125 51.5946 38.125 50.624V40.2083C38.125 39.2377 38.125 38.7523 37.9665 38.3694C37.755 37.859 37.3494 37.4533 36.839 37.2419C36.456 37.0833 35.9706 37.0833 35 37.0833C34.0294 37.0833 33.544 37.0833 33.161 37.2419C32.6506 37.4533 32.245 37.859 32.0335 38.3694C31.875 38.7523 31.875 39.2377 31.875 40.2083Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M46.4584 31.875V50.625C46.4584 51.5956 46.4584 52.081 46.6169 52.464C46.8284 52.9744 47.234 53.38 47.7444 53.5915C48.1273 53.75 48.6127 53.75 49.5834 53.75C50.554 53.75 51.0394 53.75 51.4223 53.5915C51.9327 53.38 52.3384 52.9744 52.5498 52.464C52.7084 52.081 52.7084 51.5956 52.7084 50.625V31.875C52.7084 30.9044 52.7084 30.4189 52.5498 30.0361C52.3384 29.5256 51.9327 29.12 51.4223 28.9086C51.0394 28.75 50.554 28.75 49.5834 28.75C48.6127 28.75 48.1273 28.75 47.7444 28.9086C47.234 29.12 46.8284 29.5256 46.6169 30.0361C46.4584 30.4189 46.4584 30.9044 46.4584 31.875Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M23.5416 23.5417C23.5416 25.2676 22.1425 26.6667 20.4166 26.6667C18.6907 26.6667 17.2916 25.2676 17.2916 23.5417C17.2916 21.8158 18.6907 20.4167 20.4166 20.4167C22.1425 20.4167 23.5416 21.8158 23.5416 23.5417Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M52.7084 19.375C52.7084 21.1009 51.3092 22.5 49.5834 22.5C47.8575 22.5 46.4584 21.1009 46.4584 19.375C46.4584 17.6491 47.8575 16.25 49.5834 16.25C51.3092 16.25 52.7084 17.6491 52.7084 19.375Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M38.125 27.7083C38.125 29.4342 36.7258 30.8333 35 30.8333C33.2742 30.8333 31.875 29.4342 31.875 27.7083C31.875 25.9824 33.2742 24.5833 35 24.5833C36.7258 24.5833 38.125 25.9824 38.125 27.7083Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M23.4237 24.4L31.9959 26.8492M37.7152 26.1572L46.8711 20.9253"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
practiceCircled : Nri.Ui.Svg.V1.Svg
practiceCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M40.6724 17.7899C38.888 17.2021 36.9811 16.8841 35 16.8841C24.9948 16.8841 16.884 24.9948 16.884 35C16.884 45.0051 24.9948 53.116 35 53.116C45.0051 53.116 53.1159 45.0051 53.1159 35C53.1159 33.1065 52.8253 31.2809 52.2866 29.5652"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M44.058 35C44.058 40.0025 40.0025 44.058 35 44.058C29.9974 44.058 25.942 40.0025 25.942 35C25.942 29.9974 29.9974 25.942 35 25.942"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M48.587 21.413L35 35M48.587 21.413V16.8841M48.587 21.413H53.1159"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
standards : Nri.Ui.Svg.V1.Svg
standards =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M32.9166 23.4583H53.75"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.25 43.1929C23.4722 44.6812 24.5833 45.4254 24.5833 46.5417C24.5833 47.6579 23.4722 48.4021 21.25 49.8904C19.0278 51.3787 17.9167 52.1227 17.0833 51.5648C16.25 51.0067 16.25 49.5183 16.25 46.5417C16.25 43.565 16.25 42.0767 17.0833 41.5185C17.9167 40.9606 19.0278 41.7046 21.25 43.1929Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.25 20.1096C23.4722 21.598 24.5833 22.3421 24.5833 23.4583C24.5833 24.5746 23.4722 25.3187 21.25 26.807C19.0278 28.2953 17.9167 29.0395 17.0833 28.4814C16.25 27.9232 16.25 26.4349 16.25 23.4583C16.25 20.4817 16.25 18.9934 17.0833 18.4353C17.9167 17.8772 19.0278 18.6213 21.25 20.1096Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M32.9166 35H53.75"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M32.9166 46.5417H53.75"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
modules : Nri.Ui.Svg.V1.Svg
modules =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M31.2135 22.8438C30.412 22.8437 29.6771 22.8436 29.0793 22.925C28.421 23.0146 27.7252 23.2254 27.1529 23.8048C26.5807 24.3842 26.3724 25.0887 26.2839 25.7553C26.2559 25.966 26.2377 26.2625 26.2258 26.5938H43.7743C43.7624 26.2625 43.7441 25.966 43.7161 25.7553C43.6276 25.0887 43.4195 24.3842 42.8471 23.8048C42.2748 23.2254 41.5791 23.0146 40.9208 22.925C40.323 22.8436 39.5882 22.8437 38.7867 22.8438H31.2135Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M46.5272 29.4062H23.4727C23.4649 29.414 23.457 29.4219 23.4492 29.4298C22.8769 30.0092 22.6687 30.7137 22.5802 31.3803C22.5271 31.78 22.5091 32.4878 22.5031 33.1562H47.497C47.4909 32.4878 47.473 31.78 47.4198 31.3803C47.3313 30.7137 47.1231 30.0092 46.5507 29.4298C46.543 29.4219 46.5352 29.414 46.5272 29.4062Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M50.8587 35.9688H19.1413C18.9992 36.3094 18.922 36.6623 18.8765 37.0053C18.7961 37.6103 18.7962 38.9071 18.7963 39.7188H51.2037C51.2039 38.9071 51.2039 37.6103 51.1235 37.0053C51.0779 36.6623 51.0007 36.3094 50.8587 35.9688Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M54.8515 42.4699C54.7228 42.5097 54.5862 42.5313 54.4445 42.5313H15.1404C15.1282 42.6284 15.1197 42.7234 15.1135 42.8159C15.0926 43.1258 15.0926 43.5766 15.0927 43.9784C15.0926 44.38 15.0926 44.7492 15.1135 45.0591C15.1359 45.3923 15.1871 45.7585 15.3393 46.1307C15.6683 46.9347 16.2992 47.5735 17.0932 47.9065C17.4608 48.0606 17.8226 48.1124 18.1515 48.1353C18.4577 48.1563 18.8222 48.1563 19.219 48.1563H50.7812C51.1778 48.1563 51.5425 48.1563 51.8486 48.1353C52.1777 48.1124 52.5393 48.0606 52.9069 47.9065C53.701 47.5735 54.3319 46.9347 54.6608 46.1307C54.813 45.7585 54.8641 45.3923 54.8865 45.0591C54.9075 44.7492 54.9075 44.38 54.9075 43.9784C54.9075 43.5766 54.9075 43.1258 54.8865 42.8159C54.8791 42.7039 54.8684 42.5883 54.8515 42.4699Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
writing : Nri.Ui.Svg.V1.Svg
writing =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M42.9509 27.5088C43.4208 25.3832 44.9112 21.7623 43.1636 19.8339C41.9709 18.5175 39.7272 18.5175 35.24 18.5175H34.7599C30.2727 18.5175 28.0291 18.5175 26.8362 19.8339C25.0887 21.7623 26.5792 25.3832 27.0491 27.5088"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M45.8016 41.27L38.2131 51.5305C36.7576 53.4985 36.0296 54.4825 34.9999 54.4825C33.9703 54.4825 33.2425 53.4985 31.7868 51.5305L24.1983 41.27C22.9815 39.6248 22.373 38.8021 22.4142 37.9255C22.4553 37.0488 23.2681 36.1306 24.8936 34.2939C26.4602 32.5238 27.0491 30.6907 27.0491 27.5096L42.9509 27.5088C42.9509 30.6899 43.5404 32.5233 45.1066 34.2937C46.7321 36.1308 47.5447 37.0492 47.5857 37.9258C47.6267 38.8023 47.0183 39.6248 45.8016 41.27Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M37.6973 40.0965C36.9038 40.6683 35.9825 40.9956 35 40.9956M35 40.9956C34.0174 40.9956 33.0962 40.6683 32.3026 40.0965M35 40.9956V54.4825"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
passageQuizCircled : Nri.Ui.Svg.V1.Svg
passageQuizCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M32.2826 27.7536H46.7753M32.2826 35H36.8116M46.7753 35H42.2464M32.2826 42.2464H36.8116M46.7753 42.2464H42.2464"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M25.942 26.8478H24.1304C20.7144 26.8478 19.0064 26.8478 17.9452 27.9091C16.884 28.9702 16.884 30.6782 16.884 34.0942V45.8696C16.884 48.3708 18.9117 50.3986 21.413 50.3986C23.9143 50.3986 25.942 48.3708 25.942 45.8696V26.8478Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M42.2464 19.6015H33.1884C31.5037 19.6015 30.6613 19.6015 29.9702 19.7867C28.0947 20.2892 26.6298 21.7541 26.1273 23.6296C25.9421 24.3207 25.9421 25.1631 25.9421 26.8478V45.8696C25.9421 48.3709 23.9144 50.3986 21.4131 50.3986H42.2464C47.3703 50.3986 49.9323 50.3986 51.5241 48.8067C53.116 47.2149 53.116 44.6529 53.116 39.529V30.471C53.116 25.3471 53.116 22.7851 51.5241 21.1933C49.9323 19.6015 47.3703 19.6015 42.2464 19.6015Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
planningDiagnosticCircled : Nri.Ui.Svg.V1.Svg
planningDiagnosticCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M36.0485 24.4568L36.0491 24.4561C36.1882 24.2892 36.2613 24.1034 36.2999 23.8464C36.3369 23.6005 36.3446 23.2723 36.3446 22.811L36.3445 21.0001C36.3445 19.4813 37.5757 18.25 39.0945 18.25H49.9979C51.5167 18.25 52.7479 19.4812 52.7479 21V31.907C52.7479 33.4258 51.5167 34.657 49.9979 34.657H48.9049C48.6029 34.657 48.4634 34.6409 48.3981 34.6223C48.3967 34.6219 48.3954 34.6215 48.3942 34.6212C48.3912 34.6069 48.3885 34.5813 48.3914 34.5355C48.3957 34.4672 48.4097 34.3828 48.4303 34.2587C48.4707 34.0165 48.526 33.6797 48.526 33.2117C48.526 32.1421 47.9837 31.2299 47.2263 30.5918C46.4708 29.9554 45.4825 29.5745 44.5471 29.5745C43.6074 29.5745 42.6182 29.977 41.8641 30.6218C41.1092 31.2674 40.5647 32.1787 40.5647 33.2117C40.5647 33.6805 40.6201 34.0177 40.6604 34.26C40.6811 34.3842 40.695 34.4684 40.6993 34.5365C40.7023 34.5829 40.6995 34.6083 40.6964 34.6219C40.6954 34.6222 40.6943 34.6225 40.6932 34.6229C40.6281 34.6413 40.4891 34.657 40.1875 34.657H39.0928C37.5741 34.657 36.3428 33.4258 36.3428 31.907V30.0963C36.3428 29.6342 36.3352 29.3059 36.2982 29.0602C36.2595 28.8033 36.1864 28.618 36.0474 28.4512L36.0468 28.4505C35.8948 28.2694 35.7073 28.1974 35.502 28.2057C35.3237 28.213 35.1364 28.2816 34.9676 28.346C34.9425 28.3556 34.9171 28.3654 34.8914 28.3753C34.5521 28.506 34.1604 28.6569 33.6405 28.6569C32.4251 28.6569 31.4381 27.67 31.4381 26.4528C31.4381 25.2374 32.4249 24.2504 33.6422 24.2504C34.1611 24.2504 34.5525 24.4011 34.892 24.5318C34.9179 24.5418 34.9434 24.5516 34.9687 24.5613C35.1377 24.6257 35.3251 24.6943 35.5035 24.7015C35.7088 24.7098 35.8965 24.6379 36.0485 24.4568Z"
            , Attributes.fill "white"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.path
            [ Attributes.d "M22.8109 34.9055C23.7389 34.9055 24.0482 34.938 24.296 35.1431C24.7745 35.5447 24.0003 36.1343 24.0003 37.3579C24.0003 38.7131 25.0992 39.812 26.4527 39.812C27.8079 39.812 28.9051 38.7131 28.9051 37.3596C28.9051 36.1343 28.1309 35.5464 28.6094 35.1448C28.8555 34.9397 29.1648 34.9073 30.0945 34.9073H34.9052V34.9073C34.9042 34.9073 34.9035 34.9065 34.9035 34.9055L34.9052 30.8126C34.9052 30.2127 34.8471 30.1273 34.7309 30.0777C34.4933 29.9752 34.1054 30.1854 33.2099 30.1854C31.3266 30.1854 29.8227 28.1978 29.8227 26.453C29.8227 24.7099 31.2497 22.7241 33.2099 22.7241C34.1037 22.7241 34.4916 22.9343 34.7309 22.8317C34.8454 22.7822 34.9052 22.6967 34.9052 22.0952L34.9047 20.9992C34.904 19.3429 33.5611 18.0005 31.9047 18.0005H21C19.3431 18.0005 18 19.3436 18 21.0005V31.9056C18 33.5625 19.3432 34.9057 21.0001 34.9056L22.8109 34.9055Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M30.8107 36.4542L34.6553 36.4558L34.6537 41.0165V41.0166C34.6537 41.4788 34.6614 41.807 34.6984 42.0527C34.737 42.3097 34.8102 42.4949 34.9492 42.6618L34.9498 42.6624C35.1018 42.8436 35.2895 42.9155 35.4948 42.9072C35.6732 42.9 35.8607 42.8314 36.0297 42.7669C36.0549 42.7573 36.0804 42.7475 36.1062 42.7376C36.446 42.6069 36.8379 42.4561 37.3578 42.4561C38.5733 42.4561 39.5602 43.4414 39.5602 44.6584C39.5602 45.874 38.5748 46.8608 37.3578 46.8608C36.8387 46.8608 36.4471 46.7096 36.1075 46.5785C36.0819 46.5686 36.0566 46.5588 36.0315 46.5492C35.8626 46.4846 35.6751 46.4157 35.4968 46.4082C35.2914 46.3997 35.1036 46.4715 34.9515 46.6527L34.9504 46.6541C34.8118 46.8214 34.7387 47.0068 34.7001 47.2637C34.6631 47.5094 34.6554 47.8372 34.6554 48.2985L34.6555 50.1093C34.6555 51.6281 33.4243 52.8594 31.9055 52.8594H21.0004C19.4816 52.8594 18.2504 51.6282 18.2504 50.1094V39.2059C18.2504 37.6872 19.4816 36.4559 21.0004 36.4559H22.0933C22.3956 36.4559 22.5354 36.4716 22.6011 36.49C22.6022 36.4903 22.6032 36.4906 22.6042 36.4909C22.6072 36.5053 22.6097 36.5309 22.6068 36.5758C22.6025 36.6441 22.5886 36.7284 22.5679 36.8525C22.5276 37.0947 22.4722 37.4315 22.4722 37.8995C22.4722 38.9691 23.0145 39.8813 23.772 40.5194C24.5275 41.1558 25.5157 41.5367 26.4512 41.5367C27.3908 41.5367 28.38 41.1347 29.1341 40.49C29.8891 39.8446 30.4335 38.9333 30.4335 37.8995C30.4335 37.4315 30.3782 37.0947 30.3379 36.8525C30.3172 36.7284 30.3032 36.6441 30.2989 36.5757C30.296 36.5291 30.2988 36.5033 30.3019 36.4893C30.3029 36.489 30.304 36.4887 30.3051 36.4883C30.3702 36.4699 30.5092 36.4542 30.8106 36.4542Z"
            , Attributes.fill "white"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.path
            [ Attributes.d "M48.1891 36.204C47.2611 36.204 46.9518 36.1732 46.704 35.9664C46.2255 35.5648 46.9997 34.9753 46.9997 33.7516C46.9997 32.3964 45.9008 31.2975 44.5473 31.2975C43.1921 31.2975 42.0949 32.3964 42.0949 33.7499C42.0949 34.9753 42.8691 35.5631 42.3906 35.9647C42.1445 36.1715 41.8352 36.2023 40.9055 36.2023L36.0966 36.204L36.0948 40.2987C36.0948 40.8985 36.153 40.9823 36.2692 41.0335C36.5067 41.1361 36.8947 40.9259 37.7902 40.9259C39.6734 40.9259 41.1773 42.9134 41.1773 44.6582C41.1773 46.4014 39.7504 48.3872 37.7902 48.3872C36.8964 48.3872 36.5084 48.177 36.2692 48.2795C36.1547 48.3291 36.0948 48.4145 36.0948 49.0161L36.0953 50.1103C36.096 51.7667 37.4389 53.1091 39.0953 53.1091H50C51.6569 53.1091 53 51.7659 53 50.1091V39.2039C53 37.547 51.6568 36.2039 49.9999 36.2039L48.1891 36.204Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
quizCircled : Nri.Ui.Svg.V1.Svg
quizCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M26.8478 19.6015C24.0293 19.686 22.3489 19.9997 21.1862 21.1635C19.5944 22.7568 19.5944 25.3212 19.5944 30.4499V42.2362C19.5944 47.365 19.5944 49.9294 21.1862 51.5227C22.778 53.116 25.34 53.116 30.4639 53.116H39.5219C44.6458 53.116 47.2078 53.116 48.7996 51.5227C50.3915 49.9294 50.3915 47.365 50.3915 42.2362V30.4499C50.3915 25.3212 50.3915 22.7568 48.7996 21.1636C47.637 19.9997 45.9565 19.686 43.138 19.6015"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M26.8408 20.0544C26.8408 18.3034 28.2602 16.8841 30.011 16.8841H39.9748C41.7257 16.8841 43.1451 18.3034 43.1451 20.0544C43.1451 21.8053 41.7257 23.2246 39.9748 23.2246H30.011C28.2602 23.2246 26.8408 21.8053 26.8408 20.0544Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M25.0363 31.3768H32.2826"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M37.7174 33.1884C37.7174 33.1884 38.6232 33.1884 39.529 35C39.529 35 42.4062 30.471 44.9638 29.5652"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M25.0363 42.2464H32.2826"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M37.7174 44.058C37.7174 44.058 38.6232 44.058 39.529 45.8696C39.529 45.8696 42.4062 41.3406 44.9638 40.4348"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
unitDiagnosticCircled : Nri.Ui.Svg.V1.Svg
unitDiagnosticCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.856 20.282V23.1224H21.5349C19.4432 23.1224 17.8 24.7655 17.8 26.7086V32.2379C17.8 33.2838 18.5459 34.0297 19.5918 34.0297C26.9129 34.0297 27.2131 43.2941 19.5918 43.2941C18.5459 43.2941 17.8 44.04 17.8 45.0859V50.6152C17.8 52.5583 19.4432 54.2014 21.5349 54.2014H26.9129C27.9587 54.2014 28.856 53.4556 28.856 52.4097C28.856 45.0886 37.9717 44.9371 37.9717 52.4097C37.9717 53.4556 38.7176 54.2014 39.7634 54.2014H45.2928C47.3845 54.2014 49.0276 52.5583 49.0276 50.6152V43.2941H51.7166C57.6945 43.2941 57.6945 34.0297 51.7166 34.0297H49.0276V26.7086C49.0276 24.7655 47.3845 23.1224 45.2928 23.1224H37.9717V20.282C37.9717 14.3041 28.856 14.3041 28.856 20.282Z"
            , Attributes.fill "white"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
quickWriteCircled : Nri.Ui.Svg.V1.Svg
quickWriteCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M40.8266 27.4531H19.0816C18.4189 27.4531 17.8816 27.9903 17.8816 28.6531C17.8816 29.3158 18.4189 29.8531 19.0816 29.8531L38.4266 29.8531L40.8266 27.4531ZM28.4479 42.098L36.2245 42.098C36.8872 42.098 37.4245 41.5607 37.4245 40.898C37.4245 40.2352 36.8872 39.698 36.2245 39.698L30.8445 39.698L29.2989 41.2435C28.9903 41.552 28.7077 41.8351 28.4479 42.098ZM26.2265 42.098C26.7658 41.5136 27.4034 40.8761 28.1663 40.1134L28.1663 40.1133L28.5817 39.698L22.3469 39.698C21.6842 39.698 21.1469 40.2352 21.1469 40.898C21.1469 41.5607 21.6842 42.098 22.3469 42.098H26.2265ZM32.5261 38.0163L40.3061 38.0163C40.9688 38.0163 41.5061 37.4791 41.5061 36.8163C41.5061 36.1536 40.9688 35.6163 40.3061 35.6163L34.9261 35.6163L32.5261 38.0163ZM32.6633 35.6163L30.2633 38.0163L16.6326 38.0163C15.9699 38.0163 15.4326 37.4791 15.4326 36.8163C15.4326 36.1536 15.9699 35.6163 16.6326 35.6163L32.6633 35.6163ZM34.345 33.9347H23.7075C23.0448 33.9347 22.5075 33.3974 22.5075 32.7347C22.5075 32.072 23.0448 31.5347 23.7075 31.5347H36.7449L34.345 33.9347ZM38.8997 31.6427L36.6077 33.9347H38.4014C39.0641 33.9347 39.6014 33.3974 39.6014 32.7347C39.6014 32.2497 39.3137 31.832 38.8997 31.6427ZM42.2631 28.2794L40.6894 29.8531H41.1224C41.7852 29.8531 42.3224 29.3158 42.3224 28.6531C42.3224 28.5226 42.3016 28.397 42.2631 28.2794Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M22.3899 51.3992L22.3899 51.3992L22.3899 51.3991C22.4294 51.2331 22.4692 51.066 22.5092 50.8984C22.4692 51.066 22.4294 51.2331 22.3899 51.3992ZM29.2989 41.2435L43.2579 27.2845L48.2054 32.2319L34.2463 46.191C32.3893 48.0484 31.4512 48.9687 30.3264 49.5973C29.7552 49.9164 28.7772 50.2443 27.5921 50.574C26.5042 50.8768 25.2562 51.1776 24.0171 51.4728C24.3125 50.2332 24.613 48.9857 24.9158 47.8977C25.2456 46.7123 25.5735 45.7345 25.8926 45.1634C26.5212 44.0386 27.4415 43.1005 29.2989 41.2435ZM44.3893 26.1531L49.3367 31.1005L52.3754 28.0618C53.7416 26.6956 53.7416 24.4806 52.3753 23.1144C51.0093 21.7483 48.7942 21.7482 47.4279 23.1144L44.3893 26.1531Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
guidedDraftCircled : Nri.Ui.Svg.V1.Svg
guidedDraftCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M46.3825 39.757C46.8793 39.2187 47.1276 38.9497 47.3916 38.7927C48.0285 38.4138 48.8126 38.402 49.4601 38.7615C49.7286 38.9105 49.9846 39.172 50.4966 39.6952C51.0088 40.2183 51.2648 40.4798 51.4106 40.754C51.7625 41.4153 51.751 42.2165 51.3801 42.8672C51.2265 43.1368 50.9631 43.3905 50.4361 43.898L44.1671 49.9362C43.1686 50.8978 42.6693 51.3788 42.0453 51.6225C41.4213 51.8662 40.7355 51.8483 39.3635 51.8123L39.177 51.8075C38.7593 51.7967 38.5505 51.7912 38.429 51.6533C38.3076 51.5157 38.3241 51.3028 38.3573 50.8775L38.3753 50.6465C38.4686 49.449 38.5153 48.8503 38.7491 48.3122C38.983 47.774 39.3863 47.337 40.193 46.463L46.3825 39.757Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M51.6667 33.5V32.1958C51.6667 28.9631 51.6667 27.3468 50.6904 26.3425C49.714 25.3382 48.1427 25.3382 45 25.3382H41.5357C40.0067 25.3382 39.994 25.3353 38.6192 24.6472L33.0665 21.8686C30.7481 20.7084 29.5889 20.1283 28.354 20.1686C27.1191 20.209 25.998 20.8635 23.7558 22.1725L21.7093 23.3673C20.0624 24.3288 19.2389 24.8096 18.7861 25.6094C18.3334 26.4092 18.3334 27.3832 18.3334 29.3312V43.0262C18.3334 45.5857 18.3334 46.8655 18.9038 47.5778C19.2834 48.0517 19.8153 48.3703 20.4034 48.476C21.2871 48.6347 22.3692 48.003 24.5332 46.7395C26.0026 45.8817 27.4169 44.9907 29.1748 45.2323C30.6479 45.4347 32.0167 46.3642 33.3334 47.023"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M28.3334 20.1667V45.1667"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M40 25.1667V38.5"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
guidedEssayCircled : Nri.Ui.Svg.V1.Svg
guidedEssayCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M36.6667 19.1667V20C36.6667 24.714 36.6667 27.0711 38.1312 28.5355C39.5957 30 41.9527 30 46.6667 30H47.5001M48.3334 32.7615V38.3333C48.3334 44.6187 48.3334 47.7615 46.3807 49.714C44.4282 51.6667 41.2854 51.6667 35.0001 51.6667C28.7147 51.6667 25.572 51.6667 23.6194 49.714C21.6667 47.7615 21.6667 44.6187 21.6667 38.3333V30.7597C21.6667 25.3514 21.6667 22.6472 23.1435 20.8155C23.4419 20.4455 23.7789 20.1085 24.149 19.8101C25.9806 18.3333 28.6848 18.3333 34.0931 18.3333C35.2691 18.3333 35.8569 18.3333 36.3954 18.5233C36.5074 18.5629 36.6171 18.6083 36.7242 18.6596C37.2394 18.9059 37.6551 19.3217 38.4866 20.1531L46.3807 28.0474C47.3442 29.0108 47.8259 29.4925 48.0797 30.1051C48.3334 30.7177 48.3334 31.3989 48.3334 32.7615Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M34.6002 33.4C32.246 33.4 29.9999 34.8315 29.0598 37.0908C28.1849 39.1933 28.6585 40.9827 29.6447 42.5148C30.4532 43.771 31.6325 44.8922 32.6964 45.9036L32.6969 45.9041C32.8996 46.0968 33.0981 46.2856 33.2891 46.4705L33.2901 46.4715C33.6439 46.812 34.1139 47 34.6002 47C35.0865 47 35.5566 46.812 35.9104 46.4714C36.091 46.2976 36.2782 46.1202 36.4692 45.9393C37.5447 44.9206 38.7405 43.7879 39.5575 42.5157C40.5425 40.982 41.0145 39.1907 40.1406 37.0908C39.2005 34.8315 36.9545 33.4 34.6002 33.4ZM34.5993 37.0372C33.24 37.0372 32.1381 38.17 32.1381 39.5674C32.1381 40.9648 33.24 42.0977 34.5993 42.0977C35.9585 42.0977 37.0605 40.9648 37.0605 39.5674C37.0605 38.17 35.9585 37.0372 34.5993 37.0372Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
guidedShortResponseCircled : Nri.Ui.Svg.V1.Svg
guidedShortResponseCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M26.2188 17.3522C23.8394 17.3522 21.5693 18.8417 20.6192 21.1926C19.7349 23.3804 20.2136 25.2423 21.2103 26.8366C22.0275 28.1437 23.2194 29.3103 24.2946 30.3628L24.2952 30.3633C24.5 30.5638 24.7007 30.7602 24.8937 30.9526L24.8947 30.9537C25.2523 31.308 25.7273 31.5036 26.2188 31.5036C26.7103 31.5036 27.1854 31.308 27.543 30.9536C27.7255 30.7727 27.9147 30.5882 28.1077 30.3999C29.1947 29.3399 30.4033 28.1612 31.2291 26.8375C32.2246 25.2416 32.7016 23.3777 31.8184 21.1926C30.8683 18.8417 28.5982 17.3522 26.2188 17.3522ZM26.2179 21.1369C24.844 21.1369 23.7303 22.3156 23.7303 23.7697C23.7303 25.2237 24.844 26.4025 26.2179 26.4025C27.5916 26.4025 28.7054 25.2237 28.7054 23.7697C28.7054 22.3156 27.5916 21.1369 26.2179 21.1369Z"
            , Attributes.fill "white"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M36 21H50"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.1545 51.3156H36.0146"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M36.0146 28.6733H50.0088"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 36H50"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 44H50"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
selfReviewCircled : Nri.Ui.Svg.V1.Svg
selfReviewCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M25.942 40.4348V25.942C25.942 20.9395 29.9974 16.8841 35 16.8841C40.0025 16.8841 44.058 20.9395 44.058 25.942V40.4348C44.058 43.4324 43.4324 44.058 40.4348 44.058H29.5652C26.5676 44.058 25.942 43.4324 25.942 40.4348Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M31.3768 24.1304L33.1884 22.3188"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M33.1884 28.6594L36.8115 25.0362"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M49.4927 27.7536V53.1159M20.5072 27.7536V53.1159"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.5072 49.4928H49.4927"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.5072 35H25.942M44.0579 35H49.4927"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
peerReviewCircled : Nri.Ui.Svg.V1.Svg
peerReviewCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M34.4163 17.906C40.4263 17.906 45.5564 17.906 47.4235 19.7841C49.2906 21.6622 49.2906 23.6163 49.2906 29.6619L45.0408 27.5245"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M35.4791 52.1062C29.4688 52.1062 24.3388 52.1062 22.4717 50.228C20.6046 48.3498 20.6046 46.3957 20.6046 40.3502L24.8544 42.4876"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M49.8591 38.6154C49.8591 36.208 47.9075 34.2564 45.5001 34.2564C43.0927 34.2564 41.1411 36.208 41.1411 38.6154C41.1411 41.0227 43.0927 42.9743 45.5001 42.9743C47.9075 42.9743 49.8591 41.0227 49.8591 38.6154Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M51.6026 49.0769C51.6026 45.7065 48.8704 42.9743 45.5 42.9743C42.1297 42.9743 39.3975 45.7065 39.3975 49.0769"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M27.8591 25.2821C27.8591 22.8747 25.9075 20.9231 23.5001 20.9231C21.0927 20.9231 19.1411 22.8747 19.1411 25.2821C19.1411 27.6894 21.0927 29.641 23.5001 29.641C25.9075 29.641 27.8591 27.6894 27.8591 25.2821Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M29.6026 35.7436C29.6026 32.3732 26.8704 29.641 23.5 29.641C20.1297 29.641 17.3975 32.3732 17.3975 35.7436"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
gradingAssistantCircled : Nri.Ui.Svg.V1.Svg
gradingAssistantCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M47.064 21.7611C48.0543 21.247 49.1117 22.3244 48.6072 23.3333L46.0229 28.501C45.7972 28.9522 45.8824 29.5001 46.2338 29.8582L50.259 33.9593C51.0449 34.76 50.366 36.1176 49.2682 35.9404L43.6459 35.0331C43.155 34.9539 42.6699 35.2058 42.4442 35.657L39.8599 40.8246C39.3554 41.8336 37.8784 41.5952 37.7045 40.4768L36.814 34.7484C36.7363 34.2482 36.3513 33.856 35.8603 33.7767L30.238 32.8695C29.1403 32.6923 28.9064 31.1875 29.8967 30.6734L34.9686 28.0403C35.4115 27.8104 35.6587 27.3161 35.5809 26.8159L34.6904 21.0876C34.5166 19.9691 35.849 19.2774 36.6349 20.0781L40.66 24.1792C41.0114 24.5373 41.5492 24.6241 41.9921 24.3942L47.064 21.7611Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M29.0279 36.9744C30.2227 35.7571 32.1598 35.7571 33.3546 36.9744C34.5494 38.1917 34.5494 40.1654 33.3546 41.3827L24.7012 50.1994C23.5064 51.4167 21.5693 51.4167 20.3745 50.1994C19.1797 48.9821 19.1797 47.0084 20.3745 45.7911L29.0279 36.9744Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| -}
dailyWritingCircled : Nri.Ui.Svg.V1.Svg
dailyWritingCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M41.6666 18.3333V25M28.3333 18.3333V25"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M36.6667 21.6667H33.3333C27.0479 21.6667 23.9052 21.6667 21.9526 23.6193C20 25.5719 20 28.7146 20 35V38.3333C20 44.6187 20 47.7615 21.9526 49.714C23.9052 51.6667 27.0479 51.6667 33.3333 51.6667H36.6667C42.952 51.6667 46.0948 51.6667 48.0473 49.714C50 47.7615 50 44.6187 50 38.3333V35C50 28.7146 50 25.5719 48.0473 23.6193C46.0948 21.6667 42.952 21.6667 36.6667 21.6667Z"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 31.6667H50"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
novelsCircled : Nri.Ui.Svg.V1.Svg
novelsCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M49.1666 43.2143V31.6667C49.1666 25.3813 49.1666 22.2386 47.2139 20.2859C45.2614 18.3333 42.1186 18.3333 35.8333 18.3333H34.1666C27.8812 18.3333 24.7385 18.3333 22.7859 20.2859C20.8333 22.2386 20.8333 25.3813 20.8333 31.6667V47.5"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M49.1666 43.3333H24.9999C22.6987 43.3333 20.8333 45.1988 20.8333 47.5C20.8333 49.8012 22.6987 51.6667 24.9999 51.6667H49.1666"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M49.1667 51.6667C46.8655 51.6667 45 49.8012 45 47.5C45 45.1988 46.8655 43.3333 49.1667 43.3333"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M40 26.6667H30"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M35 33.3333H30"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
textsCircled : Nri.Ui.Svg.V1.Svg
textsCircled =
    Nri.Ui.Svg.V1.init "0 0 70 70"
        [ Svg.circle
            [ Attributes.cx "35"
            , Attributes.cy "35"
            , Attributes.r "35"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M52.692 43.1786V37.0357C52.692 31.2442 52.692 28.3484 50.8897 26.5492C49.0873 24.75 46.1863 24.75 40.3843 24.75H38.846C33.0442 24.75 30.1433 24.75 28.3409 26.5492C26.5385 28.3484 26.5384 31.2441 26.5384 37.0356L26.5383 43.1784C26.5383 48.9701 26.5383 51.8658 28.3407 53.665C30.1431 55.4643 33.044 55.4643 38.846 55.4643H40.3843C46.1863 55.4643 49.0873 55.4643 50.8897 53.665C52.692 51.866 52.692 48.9701 52.692 43.1786Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M48.0768 38.5714V32.4286C48.0768 26.637 48.0768 23.7413 46.2745 21.9421C44.472 20.1429 41.5711 20.1429 35.7691 20.1429H34.2308C28.429 20.1429 25.5281 20.1429 23.7257 21.942C21.9232 23.7412 21.9232 26.637 21.9232 32.4284L21.9231 38.5713C21.9231 44.3629 21.923 47.2587 23.7254 49.0579C25.5279 50.8572 28.4288 50.8572 34.2308 50.8572H35.7691C41.5711 50.8572 44.472 50.8572 46.2745 49.0579C48.0768 47.2588 48.0768 44.3629 48.0768 38.5714Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M43.4613 33.9643V27.8214C43.4613 22.0299 43.4613 19.1341 41.659 17.3349C39.8565 15.5357 36.9556 15.5357 31.1536 15.5357H29.6153C23.8135 15.5357 20.9126 15.5357 19.1102 17.3349C17.3077 19.1341 17.3077 22.0298 17.3077 27.8213L17.3076 33.9641C17.3076 39.7558 17.3075 42.6515 19.11 44.4508C20.9124 46.25 23.8134 46.25 29.6153 46.25H31.1536C36.9556 46.25 39.8565 46.25 41.659 44.4508C43.4613 42.6517 43.4613 39.7558 43.4613 33.9643Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M24.2307 23.2143H36.5384M24.2307 30.8929H36.5384M24.2307 38.5714H30.3846"
            , Attributes.stroke "white"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]
