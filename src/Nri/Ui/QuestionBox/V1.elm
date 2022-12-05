module Nri.Ui.QuestionBox.V1 exposing
    ( QuestionBox
    , viewAnchored, AnchoredBoxMeasurementState
    , initAnchoredBoxState, updateAnchoredBoxState
    , Measurements, decodeMeasurements
    , Element
    , viewPointingTo, viewStandalone
    , containerId
    )

{-|

@docs QuestionBox

@docs viewAnchored, AnchoredBoxMeasurementState
@docs initAnchoredBoxState, updateAnchoredBoxState
@docs Measurements, decodeMeasurements
@docs Element

---

@docs viewPointingTo, viewStandalone
@docs containerId

-}

import Browser.Events
import Css
import Dict
import Dict.Extra
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (alt, css, id, src)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Markdown
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)


type alias QuestionBox msg =
    { markdown : String
    , actions : List { label : String, onClick : msg }
    }


containerId : String -> String
containerId id =
    "Nri-Scaffolding-QuestionBox-" ++ id


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


viewStandalone : QuestionBox msg -> String -> Html msg
viewStandalone questionBox idString =
    div
        [ id (containerId idString)
        , css [ Css.zIndex (Css.int 10), Css.minWidth (Css.px 300) ]
        , nriDescription "standalone-balloon-container"
        ]
        [ viewBalloon questionBox
            [ Balloon.nriDescription "standalone-balloon"
            ]
        ]


viewAnchored : QuestionBox msg -> String -> AnchoredBoxMeasurementState -> List (Html msg) -> Html msg
viewAnchored questionBox idString state content =
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
            , id (containerId idString)
            ]
            [ viewBalloon questionBox
                [ Balloon.nriDescription "anchored-balloon"
                ]
            ]
        ]


viewPointingTo : List (Html msg) -> QuestionBox msg -> Html msg
viewPointingTo content questionBox =
    span
        [ css [ Css.position Css.relative ]
        , nriDescription "pointing-to-container"
        ]
        (List.append
            [ div
                [ css
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
            content
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
    div
        [ css
            [ Css.padding2 (Css.px 10) (Css.px 20)
            , Css.borderTopRightRadius (Css.px 8)
            , Css.borderTopLeftRadius (Css.px 8)
            ]
        ]
        (Markdown.toHtml Nothing markdown
            |> List.map Html.Styled.fromUnstyled
        )


viewCharacter : Html msg
viewCharacter =
    img
        [ Attributes.width 70
        , Attributes.height 70
        , Attributes.css [ Css.position Css.absolute, Css.top (Css.px 2), Css.right (Css.px -38) ]
        , src "https://ucf018b22d191439face31e68b6b.previews.dropboxusercontent.com/p/thumb/ABuZlp9el5klCRwjIUPww_gh0lHoH4Xdg4Vew2oLAOjBo3URksR0Az38JhQjXXHi39rlgYYkuOqa8g4g76t-7pbzRblZofw9DCiMOgins89gqXsMvEYLmTkQWl4N3JfjghyRkdbkVUz3ACNaF58lUqC9bprqanC0_xSPtLIEevazUzoXnFgs_c6aWbRM294xs0dpYg6UWPmFg01_OFYNO0urYViMRbjBppq30UOsd-LKsJjeTyBDflDmuv6hOtKCIE0Z_FxT-ntt17SdSUGXyOaiymA15SeaJtFFhq-8xPHG9Y3OPRm4m86VtcI2UxuQhJq-89ruP_-sD4s5E0aAeYt7FTsBlzusQ-9eJMdQ8mY1VCv9XeNeUJ4_oBqAqeOUhppNLoiACKPtJqA0qjAmSGE2IN_sJp8ifl1vs-pkCFVkFg/p.png"
        , alt "says Panda"
        ]
        []


viewActions : List { label : String, onClick : msg } -> Html msg
viewActions actions =
    if List.isEmpty actions then
        Html.Styled.text ""

    else
        div
            [ css
                [ Css.backgroundColor Colors.frost
                , Css.borderBottomRightRadius (Css.px 8)
                , Css.borderBottomLeftRadius (Css.px 8)
                , Css.padding2 (Css.px 10) (Css.px 20)
                , Css.display Css.inlineFlex
                , Css.property "gap" "20px"
                , Css.width (Css.pct 100)
                , Css.justifyContent Css.center
                ]
            ]
            (List.map
                (\{ label, onClick } ->
                    Button.button label
                        [ Button.secondary
                        , Button.onClick onClick
                        ]
                )
                actions
            )
