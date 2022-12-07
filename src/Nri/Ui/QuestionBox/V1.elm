module Nri.Ui.QuestionBox.V1 exposing
    ( QuestionBox
    , viewAnchored, AnchoredBoxMeasurementState
    , initAnchoredBoxState, updateAnchoredBoxState
    , Measurements, decodeMeasurements
    , Element
    , viewPointingTo, viewStandalone
    )

{-|

@docs QuestionBox

@docs viewAnchored, AnchoredBoxMeasurementState
@docs initAnchoredBoxState, updateAnchoredBoxState
@docs Measurements, decodeMeasurements
@docs Element

---

@docs viewPointingTo, viewStandalone

-}

import Css
import Css.Global
import Dict
import Dict.Extra
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (alt, css, id, src)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)
import Nri.Ui.Util exposing (safeIdString)


type alias Config msg =
    { id : Maybe String
    , markdown : Maybe String
    , actions : List { label : String, onClick : msg }
    , type_ : QuestionBoxType msg
    }


defaultConfig : Config msg
defaultConfig =
    { id = Nothing
    , markdown = Nothing
    , actions = []
    , type_ = Standalone
    }


type QuestionBoxType msg
    = Standalone
    | PointingTo (List (Html msg))
    | AnchoredTo (List (Html msg)) AnchoredBoxMeasurementState


{-| -}
type alias QuestionBox msg =
    { id : String
    , markdown : String
    , actions : List { label : String, onClick : msg }
    }


{-| -}
type AnchoredBoxMeasurementState
    = Measuring
    | WithOffset Float


{-| Initially, the anchored box will be rendered in the "Measuring" state.

While in the Measuring state, the anchored box will have visibility hidden. This will cause the box to take up vertical space but not show to the user until measurements have been taken.t

-}
initAnchoredBoxState : AnchoredBoxMeasurementState
initAnchoredBoxState =
    Measuring


{-| -}
type alias Measurements =
    { anchors : List Element
    , container : Element
    , box : Element
    }


{-| -}
type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Expects JSON in the format:

    {
        "anchors" = [ { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 } ],
        "container" = { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 },
        "box" = { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 }
    }

-}
decodeMeasurements : Decode.Value -> Result Decode.Error Measurements
decodeMeasurements =
    Decode.decodeValue decodeMeasurements_


decodeMeasurements_ : Decoder Measurements
decodeMeasurements_ =
    Decode.map3 Measurements
        (Decode.field "anchors" (Decode.list decodeElement))
        (Decode.field "container" decodeElement)
        (Decode.field "box" decodeElement)


decodeElement : Decoder Element
decodeElement =
    Decode.map4 Element
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)


{-| Pass measurements (which you will need to set up a port/subscription to acquire) in order to construct an anchored box state.

    ┌───────────────────────────────────────────────────────────────────────────────────────────────┐
    │                   ┌────────────────────────────────────────────────────────────────────────┐  │
    │                   │ <───────────────────────────────────────────────>                      │  │
    │    container.x    │                     targetMid              ┌──────────┐                │  │
    │<─────────────────>│                                            │  Target  │                │  │
    │<──────────────────╬───────────────────────────────────────────>└──────────┘                │  │
    │                   │     target.x                                                           │  │
    │                   │                                     ┌──────────────────────────┐       │  │
    │                   │ <──────────────────────────────────>│       QuestionBox        │       │  │
    │                   │         centeredBoxOffset           └──────────────────────────┘       │  │
    │                   │                                                                        │  │
    │                   │                                                                        │  │
    │                   │ Scaffolding Container                                                  │  │
    │                   └────────────────────────────────────────────────────────────────────────┘  │
    │                   <─────────────────────────────────────────────><─────────────────────────>  │
    │ Viewport                            maxOffset                       questionBox.width         │
    └───────────────────────────────────────────────────────────────────────────────────────────────┘

-}
updateAnchoredBoxState : Measurements -> AnchoredBoxMeasurementState
updateAnchoredBoxState measurements =
    let
        -- the middle of the anchor. this is what the box should
        -- be aligned to ideally.
        targetMid =
            alignTarget measurements

        -- the offset that leaves the box perfectly aligned with
        -- anchorMid. we might need to tweak it a bit to ensure
        -- that the box will remain inside the container.
        centeredBoxOffset =
            targetMid - (measurements.box.width / 2)

        -- this is the maximum offset we can use. going beyond
        -- this offset means that part of the box will fall out
        -- of the container.
        maxOffset =
            measurements.container.width - measurements.box.width
    in
    WithOffset (clamp 0 maxOffset centeredBoxOffset)


{-| Computes a final horizontal alignment point based on a bunch of elements
elements (relative to the container).

    This needs to care of highlights that:
        - consist of more than one segment (potentially discontinuous!)
        - span one or more lines

    We do this heuristically by first reducing the problem to the single-line
    case and then picking the middle point of that region. To reduce it to the
    single-line case, we pick the line with the longest distance between the
    start and end of the union of all highlighted content within the line.

      ┌───────────────────────────────┐
      │                        ╔════╗ │
      │  word  word  word  word║word║ │
      │                        ╚════╝ │
      │                        <────> │
      │ ╔════╗    ╔═══════╗           │
      │ ║word║ ,  ║woooord║ word .    │ [1] pick this line
      │ ╚════╝    ╚═══════╝           │
      │ <─────────────────>           │
      └───────────────────────────────┘
                ▲ [2] aim to align the box to the middle of this region


    Note that this may not work great if we expect multiple discontinuous highlights
    that are very far apart!

      ┌──────────────────────────────┐
      │ <──────────────────────────> │
      │╔════╗                 ╔════╗ │
      │║word║ word  word  word║word║ │
      │╚════╝                 ╚════╝ │
      └──────────────────────────────┘
                     ▲

    Monopic file to edit ascii diagrams using Monodraw:
    https://www.dropbox.com/s/gze2tds1nxyljui/question%20box%20position.monopic?dl=0

-}
alignTarget : Measurements -> Float
alignTarget { anchors, container } =
    {- -}
    let
        endOf element =
            element.x + element.width

        startEnd elementsInLine =
            List.foldl
                (\element ( currentMin, currentMax ) ->
                    ( min currentMin element.x, max currentMax (endOf element) )
                )
                ( container.x + container.width, 0 )
                elementsInLine
    in
    anchors
        -- group elements by line (ie. their vertical position)
        |> Dict.Extra.groupBy .y
        |> Dict.values
        -- get the beginning and ending of each line
        |> List.map (\line -> startEnd line)
        -- grab the line with the biggest horizontal span of highlighted content
        -- (note this wont make much sense for disjoint highlights that are very
        -- far apart)
        |> List.Extra.maximumBy (\( highlightStart, highlightEnd ) -> highlightEnd - highlightStart)
        |> Maybe.withDefault ( 0, 0 )
        -- get the midpoint between the start and end of the highlighted region
        |> (\( highlightStart, highlightEnd ) -> (highlightStart + highlightEnd) / 2 - container.x)


{-| -}
viewStandalone : QuestionBox msg -> Html msg
viewStandalone questionBox =
    div
        [ id questionBox.id
        , css [ Css.zIndex (Css.int 10), Css.minWidth (Css.px 300) ]
        , nriDescription "standalone-balloon-container"
        ]
        [ viewBalloon questionBox
            [ Balloon.nriDescription "standalone-balloon"
            ]
        ]


{-| -}
viewAnchored : QuestionBox msg -> AnchoredBoxMeasurementState -> List (Html msg) -> Html msg
viewAnchored questionBox state content =
    let
        offset_ =
            (case state of
                Measuring ->
                    0.0

                WithOffset o ->
                    o
            )
                -- Hack to remove left padding, ideally this would come from a variable
                - 20
    in
    div [ nriDescription "anchored-container " ]
        [ div [] content
        , div
            [ css
                [ Css.zIndex (Css.int 10)
                , Css.display Css.inlineBlock
                , case state of
                    Measuring ->
                        Css.visibility Css.hidden

                    WithOffset _ ->
                        Css.visibility Css.visible
                , Css.position Css.relative
                , Css.left (Css.px offset_)
                ]
            , id questionBox.id
            ]
            [ viewBalloon questionBox
                [ Balloon.nriDescription "anchored-balloon"
                ]
            ]
        ]


{-| -}
viewPointingTo : List (Html msg) -> QuestionBox msg -> Html msg
viewPointingTo content questionBox =
    span
        [ css [ Css.position Css.relative ]
        , nriDescription "pointing-to-container"
        ]
        (List.append
            content
            [ div
                [ id questionBox.id
                , css
                    [ Css.position Css.absolute
                    , Css.top (Css.pct 100)
                    , Css.left (Css.pct 50)
                    , Css.transform (Css.translateX (Css.pct -50))
                    , Css.zIndex (Css.int 10)
                    , Css.minWidth (Css.px 300)
                    , Css.textAlign Css.center
                    ]
                ]
                [ viewBalloon questionBox
                    [ Balloon.onBottom
                    , Balloon.nriDescription "pointing-to-balloon"
                    ]
                ]
            ]
        )


viewBalloon : QuestionBox msg -> List (Balloon.Attribute msg) -> Html msg
viewBalloon { markdown, actions } attributes =
    Balloon.view
        ([ Balloon.html
            [ viewCharacter
            , viewGuidance markdown
            , viewActions actions
            ]
         , Balloon.navy
         , Balloon.css [ Css.padding (Css.px 0), Css.position Css.relative ]
         ]
            ++ attributes
        )


viewGuidance : String -> Html msg
viewGuidance markdown =
    Balloon.view
        [ Balloon.nriDescription "character-guidance"
        , Balloon.markdown markdown
        , Balloon.onLeft
        , Balloon.white
        , Balloon.css
            [ Css.borderRadius (Css.px 16)
            , Css.padding (Css.px 10)
            , Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ]
            ]
        ]
        |> List.singleton
        |> div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.flexEnd
                , Css.margin (Css.px 8)
                , Css.marginRight (Css.px 16)
                ]
            ]


viewCharacter : Html msg
viewCharacter =
    img
        [ Attributes.width 70
        , Attributes.height 70
        , Attributes.css [ Css.position Css.absolute, Css.top (Css.px 2), Css.right (Css.px -38) ]
        , src "https://ucf018b22d191439face31e68b6b.previews.dropboxusercontent.com/p/thumb/ABtCu0ijqrgkNHZS6mWw4c6YQwA-qVtF_W5Gxy8wMGJujaEqn2LcA95f0Si7mwoCj30tlZIIHJhbxe4lStQ8tsMDe9gkg96lbSFItuMbfBF_hmOXunQEw8ns1Q9YwaW3FIjpUWY24K7TfqooigX6VZcIE0MMI1tttLB0N0sPssIFJsJ9vm_MZ2QfHQbdMICQWNxCEOBrdbHfrMyd3TW0RrClfSWAjZWLUArn6ZQVPbAfORss-uJINVI2nI5vxNqIpeIPVG9Fh0xR6hTWcgZUzaMd9HfRZeNI4YR3LWFmRGzg9CHQHJwwv9ixK5EHbbEpDot_W_VZHnJdMsSVFdZ0yCK1M8FSVnpKVQNuRQRxBl8xoH43xFqxohpqcPWVc55lE_MNlA5isTV2lKJ1L3nby1J7vrXrGZvHvgjEYlC-EK1UyA/p.png"
        , alt "says Panda"
        ]
        []


viewActions : List { label : String, onClick : msg } -> Html msg
viewActions actions =
    let
        containerStyles =
            [ Css.backgroundColor Colors.frost
            , Css.borderBottomRightRadius (Css.px 8)
            , Css.borderBottomLeftRadius (Css.px 8)
            , Css.margin Css.zero
            , Css.padding2 (Css.px 10) (Css.px 20)
            , Css.listStyle Css.none
            , Css.displayFlex
            , Css.property "gap" "10px"
            , Css.flexDirection Css.column
            ]
    in
    case actions of
        [] ->
            text ""

        { label, onClick } :: [] ->
            div [ css (Css.alignItems Css.center :: containerStyles) ]
                [ Button.button label
                    [ Button.secondary
                    , Button.onClick onClick
                    , Button.unboundedWidth
                    ]
                ]

        _ ->
            ul [ css containerStyles ]
                (List.map
                    (\{ label, onClick } ->
                        li []
                            [ Button.button label
                                [ Button.secondary
                                , Button.onClick onClick
                                , Button.fillContainerWidth
                                ]
                            ]
                    )
                    actions
                )
