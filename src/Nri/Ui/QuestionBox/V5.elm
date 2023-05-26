module Nri.Ui.QuestionBox.V5 exposing
    ( view, Attribute
    , id, markdown, actions, character
    , neutral, correct, incorrect, tip
    , containerCss
    , setLeftActions
    , guidanceId
    )

{-|


## Patch Changes

  - Modified `viewPointingTo` to be hidden when measurements are `Nothing` to reduce the jitter of the question box moving to its correct position.

@docs view, Attribute

@docs id, markdown, actions, character
@docs neutral, correct, incorrect, tip
@docs standalone, pointingTo
@docs containerCss
@docs setLeftActions

@docs guidanceId

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.CharacterIcon.V1 as CharacterIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { id : Maybe String
    , markdown : Maybe String
    , actions : List { label : String, onClick : msg }
    , theme : QuestionBoxTheme
    , character : Maybe { name : String, icon : Svg }
    , containerCss : List Css.Style
    , leftActions : Html msg
    }


type QuestionBoxTheme
    = Neutral
    | Correct
    | Incorrect
    | Tip


defaultConfig : Config msg
defaultConfig =
    { id = Nothing
    , markdown = Nothing
    , actions = []
    , theme = Neutral
    , character = Just { name = "Panda", icon = CharacterIcon.redPanda }
    , containerCss = []
    , leftActions = text ""
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


{-| Adds an arbitrary HTML on the left of the question box for the text to speech button
-}
setLeftActions : Html msg -> Attribute msg
setLeftActions leftActions =
    Attribute (\config -> { config | leftActions = leftActions })


{-| -}
neutral : Attribute msg
neutral =
    setTheme Neutral


{-| -}
correct : Attribute msg
correct =
    setTheme Correct


{-| -}
incorrect : Attribute msg
incorrect =
    setTheme Incorrect


{-| -}
tip : Attribute msg
tip =
    setTheme Tip


setTheme : QuestionBoxTheme -> Attribute msg
setTheme theme =
    Attribute (\config -> { config | theme = theme })


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
    viewContainer config


viewContainer : Config msg -> Html msg
viewContainer config =
    let
        { backgroundColor, shadowColor } =
            themeToColor config.theme
    in
    styled div
        [ Css.backgroundColor backgroundColor
        , Css.borderRadius (Css.px borderRounding)
        , Css.borderBottom3 (Css.px 8) Css.solid shadowColor
        , Css.maxWidth (Css.px 500)
        ]
        [ ExtraAttributes.nriDescription "question-box-container" ]
        [ styled div
            [ Css.lineHeight (Css.num 1.4)
            , Css.textAlign Css.left
            , Css.position Css.relative
            , Css.borderLeft3 (Css.px 1) Css.solid Colors.gray85
            , Css.borderTop3 (Css.px 1) Css.solid Colors.gray85
            , Css.borderRight3 (Css.px 1) Css.solid Colors.gray85
            , Css.borderTopLeftRadius (Css.px borderRounding)
            , Css.borderTopRightRadius (Css.px borderRounding)
            , Css.color Colors.gray20
            , Fonts.baseFont
            , Css.fontSize (Css.px 15)
            ]
            []
            (List.filterMap identity
                [ Just <|
                    div [ css [ Css.displayFlex ] ]
                        (List.filterMap identity
                            [ Just config.leftActions
                            , Maybe.map (viewGuidance config Nothing) config.markdown
                            ]
                        )
                , viewActions config.character config.actions
                ]
            )
        ]


borderRounding : Float
borderRounding =
    8


themeToColor : QuestionBoxTheme -> { backgroundColor : Css.Color, shadowColor : Css.Color }
themeToColor theme =
    case theme of
        Neutral ->
            { backgroundColor = Colors.cornflowerLight, shadowColor = Colors.cornflower }

        Correct ->
            { backgroundColor = Colors.greenLightest, shadowColor = Colors.green }

        Incorrect ->
            { backgroundColor = Colors.purpleLight, shadowColor = Colors.purple }

        Tip ->
            { backgroundColor = Colors.white, shadowColor = Colors.white }


viewGuidance :
    { config | id : Maybe String, character : Maybe { name : String, icon : Svg } }
    -> Maybe String
    -> String
    -> Html msg
viewGuidance config referencingId markdown_ =
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
                    referencingId
                    [ Balloon.markdown markdown_
                    , Balloon.css [ Css.minHeight (Css.px 46) ]
                    , Balloon.containerCss [ Css.marginRight (Css.px 8) ]
                    , Balloon.onLeft
                    , Balloon.alignArrowEnd
                    ]
                ]

        Nothing ->
            viewSpeechBubble config
                referencingId
                [ Balloon.markdown markdown_
                , Balloon.css [ Css.margin2 (Css.px 10) (Css.px 20) ]
                ]


viewSpeechBubble : { config | id : Maybe String } -> Maybe String -> List (Balloon.Attribute msg) -> Html msg
viewSpeechBubble config referencingId extraAttributes =
    Balloon.view
        ([ Balloon.nriDescription "guidance-speech-bubble"
         , Balloon.white
         , Balloon.css
            [ Css.borderRadius (Css.px 20)
            , Css.padding (Css.px 20)
            , Css.boxShadow Css.none
            , Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ]
            , Css.fontSize (Css.px 18)
            ]
         , Balloon.custom
            [ ExtraAttributes.maybe (guidanceId >> Attributes.id) config.id
            , Key.tabbable False
            ]
         , Balloon.custom <|
            case referencingId of
                Just id_ ->
                    [ Aria.describedBy [ id_ ]
                    , Aria.details id_
                    ]

                Nothing ->
                    []
         ]
            ++ extraAttributes
        )


viewCharacter : { name : String, icon : Svg } -> Html msg
viewCharacter { name, icon } =
    icon
        |> Svg.withLabel (name ++ " says, ")
        |> Svg.withWidth (Css.px 70)
        |> Svg.withHeight (Css.px 70)
        |> Svg.withCss
            [ Css.position Css.absolute
            , Css.bottom (Css.px -5)
            , Css.right (Css.px -58)
            ]
        |> Svg.toHtml


viewActions : Maybe character -> List { label : String, onClick : msg } -> Maybe (Html msg)
viewActions maybeCharacter actions_ =
    let
        containerStyles =
            [ Css.margin Css.zero
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
                                , Button.css [ Css.justifyContent Css.flexStart ]
                                ]
                            ]
                    )
                    actions_
                )
                |> Just
