module Nri.Ui.Confetti.V2 exposing
    ( Model, init
    , view
    , Msg, burst, update, updatePageWidth, subscriptions
    )

{-|

@docs Model, init
@docs view
@docs Msg, burst, update, updatePageWidth, subscriptions

Patch changes:

  - adds stars to the confetti

Changes from V1:

  - removes custom words from confetti

-}

import Css exposing (Color)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V2 as UiIcon
import Particle exposing (Particle)
import Particle.System as ParticleSystem
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)



-- MODEL


{-| -}
type Model
    = System (ParticleSystem.System Confetti) Float


type Confetti
    = Square ConfettiSettings
    | StarOutline ConfettiSettings
    | StarFilled ConfettiSettings


type alias ConfettiSettings =
    { color : Color
    , rotations : Float
    , rotationOffset : Float
    }


{-| `center` An argument to Particle.withLocation that determines the horizontal center of viewport where you would like confetti to rain.
-}
init : Float -> Model
init center =
    System (ParticleSystem.init (Random.initialSeed 0)) center



-- VIEW


{-| -}
view : Model -> Html.Html msg
view (System system _) =
    system
        |> ParticleSystem.viewCustom viewConfetti
            (Html.div
                [ Attributes.css
                    [ Css.position Css.fixed
                    , Css.top Css.zero
                    , Css.left Css.zero
                    , Css.width (Css.pct 100)
                    , Css.height (Css.vh 100)
                    , Css.pointerEvents Css.none
                    , MediaQuery.prefersReducedMotion [ Css.display Css.none ]
                    ]
                ]
            )


viewConfetti : Particle Confetti -> Html.Html msg
viewConfetti particle =
    case Particle.data particle of
        Square { color, rotationOffset, rotations } ->
            let
                lifetime =
                    Particle.lifetimePercent particle
            in
            Html.div
                [ css
                    [ Css.backgroundColor color
                    , Css.width (Css.px 14)
                    , Css.height (Css.px 14)
                    ]
                , Attributes.style "position" "absolute"
                , Attributes.style "left" "0px"
                , Attributes.style "top" "0px"
                , Attributes.style "transform-origin" "center"
                , Attributes.style "will-change" "transform"
                , Attributes.style "transform" <|
                    "translateX("
                        ++ String.fromFloat (Particle.leftPixels particle)
                        ++ "px) translateY("
                        ++ String.fromFloat (Particle.topPixels particle)
                        ++ "px) rotate("
                        ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360)
                        ++ "deg)"
                ]
                []

        StarFilled settings ->
            viewSvg particle settings UiIcon.starFilled

        StarOutline settings ->
            viewSvg particle settings UiIcon.starOutline


viewSvg : Particle Confetti -> ConfettiSettings -> Svg.Svg -> Html.Html msg
viewSvg particle { color, rotationOffset, rotations } svg =
    let
        lifetime =
            Particle.lifetimePercent particle
    in
    svg
        |> Svg.withWidth (Css.px 20)
        |> Svg.withHeight (Css.px 20)
        |> Svg.withColor color
        |> Svg.withCss
            [ Css.position Css.absolute
            , Css.left Css.zero
            , Css.top Css.zero
            , Css.property "transform-origin" "center"
            , Css.property "will-change" "transform"
            , Css.property "transform" <|
                "translateX("
                    ++ String.fromFloat (Particle.leftPixels particle)
                    ++ "px) translateY("
                    ++ String.fromFloat (Particle.topPixels particle)
                    ++ "px) rotate("
                    ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360)
                    ++ "deg)"
            ]
        |> Svg.toHtml



-- UPDATE


{-| `burst` BURSTS CONFETTI!!!
-}
burst : Model -> Model
burst (System system center) =
    System
        (ParticleSystem.burst (particlesGenerator center) system)
        center


particlesGenerator : Float -> Generator (List (Particle Confetti))
particlesGenerator center =
    Random.list 200 <|
        particleGenerator center
            (Random.Extra.frequency
                ( 3 / 5, shapeGenerator Square )
                [ ( 1 / 5, shapeGenerator StarOutline )
                , ( 1 / 5, shapeGenerator StarFilled )
                ]
            )


shapeGenerator : (ConfettiSettings -> Confetti) -> Generator Confetti
shapeGenerator generator =
    Random.map3
        (\color rotations rotationOffset ->
            generator
                { color = color
                , rotations = rotations
                , rotationOffset = rotationOffset
                }
        )
        randomHighlightColor
        (normal 1 1)
        (Random.float 0 1)


randomHighlightColor : Generator Color
randomHighlightColor =
    Random.weighted
        ( 1 / 20, Colors.highlightYellowDark )
        [ ( 1 / 20, Colors.highlightCyanDark )
        , ( 1 / 20, Colors.highlightMagentaDark )
        , ( 1 / 20, Colors.highlightGreenDark )
        , ( 1 / 20, Colors.highlightBlueDark )
        , ( 1.5 / 10, Colors.highlightYellow )
        , ( 1.5 / 10, Colors.highlightCyan )
        , ( 1.5 / 10, Colors.highlightMagenta )
        , ( 1.5 / 10, Colors.highlightGreen )
        , ( 1.5 / 10, Colors.highlightBlue )
        ]


particleGenerator : Float -> Generator Confetti -> Generator (Particle Confetti)
particleGenerator center generator =
    Particle.init generator
        |> Particle.withLifetime (normal 2.5 1)
        |> Particle.withLocation (normal center (center / 2) |> Random.map (\x -> { x = x, y = -50 }))
        |> Particle.withDirection (Random.constant (degrees 0))
        |> Particle.withSpeed (Random.float 0 1000)
        |> Particle.withGravity 600
        |> Particle.withDrag
            (\confetti ->
                { density = 0.001226
                , coefficient = 1
                , area = 1
                }
            )


{-| -}
type alias Msg =
    ParticleSystem.Msg Confetti


{-| -}
update : ParticleSystem.Msg Confetti -> Model -> Model
update msg (System system center) =
    System (ParticleSystem.update msg system) center


{-| You will need to watch for page resize events and update the confetti model
with the new width. If you don't, your confetti will be off-center.

Why is this not part of subscriptions? Your application may already be listening
for browser resize events -- we don't want to double-up listeners unnecessarily.

-}
updatePageWidth : Int -> Model -> Model
updatePageWidth width (System system _) =
    System system (toFloat (width // 2))


{-| -}
subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions confettiMsg (System system _) =
    ParticleSystem.sub [] confettiMsg system
