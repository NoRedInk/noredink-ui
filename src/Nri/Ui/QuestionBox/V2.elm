module Nri.Ui.QuestionBox.V2 exposing
    ( view, Attribute
    , id, markdown, actions, character
    , standalone, pointingTo
    , containerCss
    , guidanceId
    )

{-|

@docs view, Attribute

@docs id, markdown, actions, character
@docs standalone, pointingTo
@docs containerCss

@docs guidanceId

-}

import Accessibility.Styled.Key as Key
import Browser.Dom exposing (Element)
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.CharacterIcon.V1 as CharacterIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (nriDescription)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Position exposing (xOffsetPx)


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { id : Maybe String
    , markdown : Maybe String
    , actions : List { label : String, onClick : msg }
    , type_ : QuestionBoxType msg
    , character : Maybe { name : String, icon : Svg }
    , containerCss : List Css.Style
    }


defaultConfig : Config msg
defaultConfig =
    { id = Nothing
    , markdown = Nothing
    , actions = []
    , type_ = Standalone
    , character = Just { name = "Panda", icon = CharacterIcon.panda }
    , containerCss = [ Css.maxWidth (Css.px 440) ]
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
character : Maybe { name : String, icon : Svg } -> Attribute msg
character details =
    Attribute (\config -> { config | character = details })


{-| -}
containerCss : List Css.Style -> Attribute msg
containerCss styles =
    Attribute (\config -> { config | containerCss = config.containerCss ++ styles })


setType : QuestionBoxType msg -> Attribute msg
setType type_ =
    Attribute (\config -> { config | type_ = type_ })


type QuestionBoxType msg
    = Standalone
    | PointingTo (Maybe Element)


{-| This is the default type of question box. It doesn't have a programmatic or direct visual relationship to any piece of content.
-}
standalone : Attribute msg
standalone =
    setType Standalone


{-| This type of `QuestionBox` is absolutely positioned & has an arrow pointing to its relatively positioned ancestor.

Typically, you would use this type of `QuestionBox` type with a `Block` by way of `Block.withHtml`.

You will need to pass a measurement, taken using Dom.Browser, in order for the question box to be positioned correctly horizontally so that it doesn't get cut off by the viewport.

-}
pointingTo : Maybe Element -> Attribute msg
pointingTo element =
    setType (PointingTo element)


{-| This helper is how we create an id for the guidance speech bubble element based on the `QuestionBox.id` value. Use this helper to manage focus.

When showing multiple questions in a sequence based on the user's answer, we want to move the user's focus to the guidance so that:

  - Screenreader users are alerted to a new context -- the new question!
  - Keyboard users's focus is somewhere convenient to answer the question (but not _on_ an answer, since we don't want accidental submissions or for the user to hit enter straight and miss the guidance!)

-}
guidanceId : String -> String
guidanceId id_ =
    id_ ++ "__guidance-speech-bubble"


{-|

    QuestionBox.view
        [ QuestionBox.markdown "**WOW**, great component!"
        ]

-}
view : List (Attribute msg) -> Html msg
view attributes =
    let
        config =
            List.foldl (\(Attribute f) a -> f a) defaultConfig attributes
    in
    case config.type_ of
        Standalone ->
            viewStandalone config

        PointingTo element ->
            viewPointingTo config element


{-| -}
viewStandalone : Config msg -> Html msg
viewStandalone config =
    div
        [ AttributesExtra.maybe Attributes.id config.id
        , css config.containerCss
        , nriDescription "standalone-balloon-container"
        ]
        [ viewBalloon config
            [ Balloon.nriDescription "standalone-balloon"
            ]
        ]


{-| -}
viewPointingTo : Config msg -> Maybe Element -> Html msg
viewPointingTo config element =
    let
        xOffset =
            Maybe.map xOffsetPx element
                |> Maybe.withDefault 0
    in
    viewBalloon config
        [ Balloon.onBottom
        , Balloon.nriDescription "pointing-to-balloon"
        , case config.id of
            Just id_ ->
                Balloon.id id_

            Nothing ->
                Balloon.css []
        , Balloon.containerCss
            [ Css.position Css.absolute
            , Css.top (Css.pct 100)
            , Css.left (Css.pct 50)
            , Css.transforms
                [ Css.translateX (Css.pct -50)
                , Css.translateY (Css.px 8)
                ]
            , Css.minWidth (Css.px 300)
            , Css.textAlign Css.center
            , Css.batch config.containerCss
            ]
        , Balloon.css <|
            if xOffset /= 0 then
                [ Css.property "transform" ("translateX(" ++ String.fromFloat xOffset ++ "px)")
                ]

            else
                []
        ]


viewBalloon : Config msg -> List (Balloon.Attribute msg) -> Html msg
viewBalloon config attributes =
    Balloon.view
        ([ Balloon.html
            (List.filterMap identity
                [ Maybe.map (viewGuidance config) config.markdown
                , viewActions config.character config.actions
                ]
            )
         , Balloon.customTheme { backgroundColor = Colors.glacier, color = Colors.glacier }
         , Balloon.css [ Css.padding (Css.px 0), Css.boxShadow Css.none ]
         ]
            ++ attributes
        )


viewGuidance : { config | id : Maybe String, character : Maybe { name : String, icon : Svg } } -> String -> Html msg
viewGuidance config markdown_ =
    case config.character of
        Just character_ ->
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.flexEnd
                    , Css.margin (Css.px 8)
                    , Css.marginRight (Css.px 20)
                    , Css.position Css.relative
                    ]
                ]
                [ viewCharacter character_
                , viewSpeechBubble config
                    [ Balloon.markdown markdown_
                    , Balloon.onLeft
                    , Balloon.alignArrowEnd
                    , Balloon.css [ Css.minHeight (Css.px 46) ]
                    ]
                ]

        Nothing ->
            viewSpeechBubble config
                [ Balloon.markdown markdown_
                , Balloon.css [ Css.margin2 (Css.px 10) (Css.px 20) ]
                ]


viewSpeechBubble : { config | id : Maybe String } -> List (Balloon.Attribute msg) -> Html msg
viewSpeechBubble config extraAttributes =
    Balloon.view
        ([ Balloon.nriDescription "guidance-speech-bubble"
         , Balloon.white
         , Balloon.css
            [ Css.borderRadius (Css.px 16)
            , Css.padding (Css.px 10)
            , Css.boxShadow Css.none
            , Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ]
            ]
         , Balloon.custom
            [ AttributesExtra.maybe (guidanceId >> Attributes.id) config.id
            , Key.tabbable False
            ]
         ]
            ++ extraAttributes
        )


viewCharacter : { name : String, icon : Svg } -> Html msg
viewCharacter { name, icon } =
    icon
        |> Svg.withLabel (name ++ " says, ")
        |> Svg.withWidth (Css.px 50)
        |> Svg.withHeight (Css.px 70)
        |> Svg.withCss
            [ Css.position Css.absolute
            , Css.bottom (Css.px -18)
            , Css.right (Css.px -48)
            ]
        |> Svg.toHtml


viewActions : Maybe character -> List { label : String, onClick : msg } -> Maybe (Html msg)
viewActions maybeCharacter actions_ =
    let
        containerStyles =
            [ Css.backgroundColor Colors.frost
            , Css.border3 (Css.px 1) Css.solid Colors.glacier
            , Css.borderBottomRightRadius (Css.px 8)
            , Css.borderBottomLeftRadius (Css.px 8)
            , Css.margin Css.zero
            , case maybeCharacter of
                Just _ ->
                    Css.padding4 (Css.px 10) (Css.px 30) (Css.px 10) (Css.px 10)

                Nothing ->
                    Css.padding2 (Css.px 10) (Css.px 20)
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
                    [ Button.onClick onClick
                    , Button.unboundedWidth
                    , Button.small
                    ]
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
                                , Button.small
                                ]
                            ]
                    )
                    actions_
                )
                |> Just
