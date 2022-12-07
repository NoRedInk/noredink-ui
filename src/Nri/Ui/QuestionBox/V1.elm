module Nri.Ui.QuestionBox.V1 exposing
    ( view, Attribute
    , id, markdown, actions, character
    , standalone, pointingTo, anchoredTo
    , AnchoredBoxMeasurements, initAnchoredBoxMeasurements, decodeAnchoredBoxMeasurements
    )

{-|

@docs view, Attribute

@docs id, markdown, actions, character
@docs standalone, pointingTo, anchoredTo


## Anchored box measurements

@docs AnchoredBoxMeasurements, initAnchoredBoxMeasurements, decodeAnchoredBoxMeasurements

-}

import Css
import Css.Global
import Dict
import Dict.Extra
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (alt, css, src)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (nriDescription)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Util exposing (safeIdString)


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { id : Maybe String
    , markdown : Maybe String
    , actions : List { label : String, onClick : msg }
    , type_ : QuestionBoxType msg
    , character : Maybe { name : String, icon : Svg }
    }


defaultConfig : Config msg
defaultConfig =
    { id = Nothing
    , markdown = Nothing
    , actions = []
    , type_ = Standalone
    , character = Nothing
    }


{-| -}
id : String -> Attribute msg
id id_ =
    Attribute (\config -> { config | id = Just id_ })


{-| -}
markdown : String -> Attribute msg
markdown content =
    Attribute (\config -> { config | markdown = Just content })


{-| -}
actions : List { label : String, onClick : msg } -> Attribute msg
actions actions_ =
    Attribute (\config -> { config | actions = actions_ })


{-| -}
character : { name : String, icon : Svg } -> Attribute msg
character details =
    Attribute (\config -> { config | character = Just details })


setType : QuestionBoxType msg -> Attribute msg
setType type_ =
    Attribute (\config -> { config | type_ = type_ })


type QuestionBoxType msg
    = Standalone
    | PointingTo (List (Html msg))
    | AnchoredTo (List (Html msg)) AnchoredBoxMeasurements


{-| This is the default type of question box. It doesn't have a programmatic or direct visual relationship to any piece of content.
-}
standalone : Attribute msg
standalone =
    setType Standalone


{-| This type of `QuestionBox` has an arrow pointing to the relevant part of the question.

Typically, you would use this type of `QuestionBox` type with a `Block`.

-}
pointingTo : List (Html msg) -> Attribute msg
pointingTo =
    setType << PointingTo


{-| This type of `QuestionBox` is only rendered after measurements of the DOM are made.

Tessa does not know when it's appropriate to use this type of QuestionBox -- sorry!

-}
anchoredTo : List (Html msg) -> AnchoredBoxMeasurements -> Attribute msg
anchoredTo content measurements =
    setType (AnchoredTo content measurements)


{-| -}
type AnchoredBoxMeasurements
    = Measuring
    | WithOffset Float


{-| Initially, the anchored box will be rendered in the "Measuring" state.

While in the Measuring state, the anchored box will have visibility hidden. This will cause the box to take up vertical space but not show to the user until measurements have been taken.t

-}
initAnchoredBoxMeasurements : AnchoredBoxMeasurements
initAnchoredBoxMeasurements =
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


{-| Decodes JSON in the format:

    {
        "anchors" = [ { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 } ],
        "container" = { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 },
        "box" = { "x" = 1.2, "y" = .3, "width" = 300.0, "height" = 325.5 }
    }

Produces an `AnchoredBoxMeasurement` for use with `anchoredTo`

-}
decodeAnchoredBoxMeasurements : Decode.Value -> Result Decode.Error AnchoredBoxMeasurements
decodeAnchoredBoxMeasurements =
    Decode.decodeValue (Decode.map updateAnchoredBoxState decodeMeasurements_)


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
updateAnchoredBoxState : Measurements -> AnchoredBoxMeasurements
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


view : List (Attribute msg) -> Html msg
view attributes =
    let
        config =
            List.foldl (\(Attribute f) a -> f a) defaultConfig attributes
    in
    case config.type_ of
        Standalone ->
            viewStandalone config

        AnchoredTo content measurements ->
            viewAnchored config measurements content

        PointingTo content ->
            viewPointingTo config content


{-| -}
viewStandalone : Config msg -> Html msg
viewStandalone config =
    div
        [ AttributesExtra.maybe Attributes.id config.id
        , css [ Css.zIndex (Css.int 10), Css.minWidth (Css.px 300) ]
        , nriDescription "standalone-balloon-container"
        ]
        [ viewBalloon config
            [ Balloon.nriDescription "standalone-balloon"
            ]
        ]


{-| -}
viewAnchored : Config msg -> AnchoredBoxMeasurements -> List (Html msg) -> Html msg
viewAnchored config state content =
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
            , AttributesExtra.maybe Attributes.id config.id
            ]
            [ viewBalloon config
                [ Balloon.nriDescription "anchored-balloon"
                ]
            ]
        ]


{-| -}
viewPointingTo : Config msg -> List (Html msg) -> Html msg
viewPointingTo config content =
    span
        [ css [ Css.position Css.relative ]
        , nriDescription "pointing-to-container"
        ]
        (List.append
            content
            [ div
                [ AttributesExtra.maybe Attributes.id config.id
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
                [ viewBalloon config
                    [ Balloon.onBottom
                    , Balloon.nriDescription "pointing-to-balloon"
                    ]
                ]
            ]
        )


viewBalloon : Config msg -> List (Balloon.Attribute msg) -> Html msg
viewBalloon config attributes =
    Balloon.view
        ([ Balloon.html
            (List.filterMap identity
                [ Maybe.map (viewGuidance config.character) config.markdown
                , viewActions config.actions
                ]
            )
         , Balloon.customTheme { backgroundColor = Colors.glacier, color = Colors.glacier }
         , Balloon.css [ Css.padding (Css.px 0) ]
         ]
            ++ attributes
        )


viewGuidance : Maybe { name : String, icon : Svg } -> String -> Html msg
viewGuidance withCharacter markdown_ =
    case withCharacter of
        Just character_ ->
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.flexEnd
                    , Css.margin (Css.px 8)
                    , Css.marginRight (Css.px 16)
                    , Css.position Css.relative
                    ]
                ]
                [ viewCharacter character_
                , Balloon.view
                    [ Balloon.nriDescription "character-guidance"
                    , Balloon.markdown markdown_
                    , Balloon.onLeft
                    , Balloon.alignArrowEnd
                    , Balloon.white
                    , Balloon.css
                        [ Css.borderRadius (Css.px 16)
                        , Css.padding (Css.px 10)
                        , Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ]
                        ]
                    ]
                ]

        Nothing ->
            -- TODO: Check styles look okay
            Text.mediumBody
                [ Text.markdown markdown_
                , Text.css
                    [ Css.padding (Css.px 10)
                    , Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ]
                    ]
                ]


viewCharacter : { name : String, icon : Svg } -> Html msg
viewCharacter { name, icon } =
    icon
        |> Svg.withLabel (name ++ " says, ")
        |> Svg.withWidth (Css.px 70)
        |> Svg.withHeight (Css.px 70)
        |> Svg.withCss
            [ Css.position Css.absolute
            , Css.bottom (Css.px -18)
            , Css.right (Css.px -53)
            ]
        |> Svg.toHtml


viewActions : List { label : String, onClick : msg } -> Maybe (Html msg)
viewActions actions_ =
    let
        containerStyles =
            [ Css.backgroundColor Colors.frost
            , Css.border3 (Css.px 1) Css.solid Colors.glacier
            , Css.borderBottomRightRadius (Css.px 8)
            , Css.borderBottomLeftRadius (Css.px 8)
            , Css.margin Css.zero
            , Css.padding2 (Css.px 10) (Css.px 40)
            , Css.listStyle Css.none
            , Css.displayFlex
            , Css.property "gap" "10px"
            , Css.flexDirection Css.column
            ]
    in
    case actions_ of
        [] ->
            Nothing

        { label, onClick } :: [] ->
            div [ css (Css.alignItems Css.center :: containerStyles) ]
                [ Button.button label
                    [ Button.onClick onClick, Button.unboundedWidth ]
                ]
                |> Just

        _ ->
            ul [ css containerStyles ]
                (List.map
                    (\{ label, onClick } ->
                        li []
                            [ Button.button label
                                [ Button.onClick onClick
                                , Button.fillContainerWidth
                                ]
                            ]
                    )
                    actions_
                )
                |> Just
