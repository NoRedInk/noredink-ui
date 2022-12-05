module Nri.Ui.QuestionBox.V1 exposing
    ( QuestionBox
    , viewAnchored, viewPointingTo, viewStandalone
    , containerId
    , AnchoredBoxState, Measurements, Element, initAnchoredBoxState, alignedToAnchors, subscriptionsForAnchoredBox, decodeMeasurements
    , hackyHardcodedOffset
    )

{-|

@docs QuestionBox
@docs viewAnchored, viewPointingTo, viewStandalone
@docs containerId

@docs AnchoredBoxState, Measurements, Element, initAnchoredBoxState, alignedToAnchors, subscriptionsForAnchoredBox, decodeMeasurements

-}

import Accessibility.Styled exposing (Html)
import Browser.Events
import Css
import Dict
import Dict.Extra
import Html.Styled exposing (div, span)
import Html.Styled.Attributes exposing (css, id)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Markdown
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 exposing (nriDescription)


type QuestionBox msg
    = QuestionBox
        { markdown : String
        , actions : List { label : String, onClick : msg }
        }


viewStandalone : QuestionBox msg -> String -> Html msg
viewStandalone questionBox idString =
    div
        [ id (containerId idString)
        , css [ Css.zIndex (Css.int 10), Css.minWidth (Css.px 300) ]
        , nriDescription "standalone-balloon-container"
        ]
        [ Balloon.view
            [ Balloon.html [ viewBalloonContent questionBox ]
            , Balloon.navy
            , Balloon.css [ Css.padding (Css.px 0) ]
            , Balloon.nriDescription "standalone-balloon"
            ]
        ]


containerId : String -> String
containerId id =
    "Nri-Scaffolding-QuestionBox-" ++ id


type AnchoredBoxState
    = Measuring
    | WithOffset Float


initAnchoredBoxState : AnchoredBoxState
initAnchoredBoxState =
    Measuring


hackyHardcodedOffset : Float -> AnchoredBoxState
hackyHardcodedOffset offset =
    WithOffset offset


subscriptionsForAnchoredBox : { onWindowResized : msg } -> Sub msg
subscriptionsForAnchoredBox { onWindowResized } =
    Browser.Events.onResize (\_ _ -> onWindowResized)


type alias Measurements =
    { anchors : List Element
    , container : Element
    , box : Element
    }


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


decodeMeasurements : Decoder Measurements
decodeMeasurements =
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


alignedToAnchors : Measurements -> AnchoredBoxState
alignedToAnchors measurements =
    {-
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


viewAnchored : QuestionBox msg -> String -> AnchoredBoxState -> List (Html msg) -> Html msg
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
            [ Balloon.view
                [ Balloon.html [ viewBalloonContent questionBox ]
                , Balloon.navy
                , Balloon.css [ Css.padding (Css.px 0) ]
                , Balloon.nriDescription "anchored-balloon"
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
                [ Balloon.view
                    [ Balloon.html [ viewBalloonContent questionBox ]
                    , Balloon.navy
                    , Balloon.css [ Css.padding (Css.px 0) ]
                    , Balloon.onBottom
                    , Balloon.nriDescription "pointing-to-balloon"
                    ]
                ]
            ]
            content
        )


viewBalloonContent : QuestionBox msg -> Html msg
viewBalloonContent (QuestionBox { markdown, actions }) =
    div
        [ nriDescription "balloon-content" ]
        [ div
            [ css
                [ Css.padding2 (Css.px 10) (Css.px 20)
                , Css.borderTopRightRadius (Css.px 8)
                , Css.borderTopLeftRadius (Css.px 8)
                ]
            ]
            (Markdown.toHtml Nothing markdown
                |> List.map Html.Styled.fromUnstyled
            )
        , viewActions actions
        ]


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
