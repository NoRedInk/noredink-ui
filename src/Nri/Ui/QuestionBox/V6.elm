module Nri.Ui.QuestionBox.V6 exposing
    ( view, Attribute
    , id, markdown
    , character, characterPosition
    , actions, actionsVertical, actionsHorizontal
    , neutral, correct, incorrect, tip
    , containerCss
    , leftActions
    , guidanceId
    )

{-| Changes from V5:

  - ???

@docs view, Attribute

@docs id, markdown
@docs character, characterPosition
@docs actions, actionsVertical, actionsHorizontal
@docs neutral, correct, incorrect, tip
@docs containerCss
@docs leftActions

@docs guidanceId

-}

import Content
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { id : Maybe String
    , markdown : Maybe String
    , actions : List (Action msg)
    , actionOrientation : ActionOrientation
    , theme : QuestionBoxTheme
    , character : Maybe { name : String, icon : Svg }
    , containerCss : List Css.Style
    , leftActions : Maybe (Html msg)
    , characterPosition : Maybe CharacterPosition
    }


type alias CharacterPosition =
    { width : Float
    , height : Float
    , top : Float
    }


type alias Action msg =
    { label : String, theme : Button.Attribute msg, onClick : msg }


type ActionOrientation
    = Horizontal
    | Vertical


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
    , actionOrientation = Vertical
    , theme = Neutral
    , character = Nothing
    , containerCss = []
    , leftActions = Nothing
    , characterPosition = Nothing
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
actions : List (Action msg) -> Attribute msg
actions actions_ =
    Attribute (\config -> { config | actions = actions_ })


{-| Arranges the buttons horizontally on the QuestionBox.
-}
actionsHorizontal : Attribute msg
actionsHorizontal =
    Attribute (\config -> { config | actionOrientation = Horizontal })


{-| Arranges the buttons vertically on the QuestionBox. This is the default behavior.
-}
actionsVertical : Attribute msg
actionsVertical =
    Attribute (\config -> { config | actionOrientation = Vertical })


{-| Note that the character _only_ appears when the theme is `tip`!
-}
character : { name : String, icon : Svg } -> Attribute msg
character details =
    Attribute (\config -> { config | character = Just details })


{-| -}
characterPosition : CharacterPosition -> Attribute msg
characterPosition characterPosition_ =
    Attribute (\config -> { config | characterPosition = Just characterPosition_ })


{-| -}
containerCss : List Css.Style -> Attribute msg
containerCss styles =
    Attribute (\config -> { config | containerCss = config.containerCss ++ styles })


{-| Adds an arbitrary HTML on the left of the question box for the text to speech button
-}
leftActions : Html msg -> Attribute msg
leftActions leftActions_ =
    Attribute (\config -> { config | leftActions = Just leftActions_ })


{-| A neutral box that should prompt the user with a question. This is the default theme.

This dialog DOES NOT show the character icon (they should be showed as part of the `CharacterStage`)

-}
neutral : Attribute msg
neutral =
    setTheme Neutral


{-| A box that signifies that an answer was correct.

This dialog DOES NOT show the character icon (they should be showed as part of the `CharacterStage`)

-}
correct : Attribute msg
correct =
    setTheme Correct


{-| A box that signifies that an answer was incorrect.

This dialog DOES NOT show the character icon (they should be showed as part of the `CharacterStage`)

-}
incorrect : Attribute msg
incorrect =
    setTheme Incorrect


{-| An informational box that shows the character icon.
-}
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
    id_ ++ "__guidance-speech"


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

        maybeCharacter =
            case config.theme of
                Tip ->
                    config.character

                _ ->
                    Nothing
    in
    styled div
        [ Css.backgroundColor backgroundColor
        , case config.theme of
            Tip ->
                Css.batch
                    [ Css.maxWidth (Css.px 443)
                    , Css.borderRadius (Css.px borderRounding)
                    , Css.border3 (Css.px 1) Css.solid Colors.gray85
                    ]

            _ ->
                Css.batch
                    [ Css.maxWidth (Css.px 500)
                    , Css.borderTopLeftRadius (Css.px borderRounding)
                    , Css.borderTopRightRadius (Css.px borderRounding)
                    , Css.borderBottomLeftRadius (Css.px (borderRounding * 2))
                    , Css.borderBottomRightRadius (Css.px (borderRounding * 2))
                    , Css.borderBottom3 (Css.px 8) Css.solid shadowColor
                    ]
        , Css.width (Css.pct 100)
        , Css.batch config.containerCss
        ]
        [ AttributesExtra.nriDescription "question-box-container"
        , case config.id of
            Nothing ->
                AttributesExtra.none

            Just id_ ->
                Attributes.id id_
        ]
        [ styled div
            [ Css.lineHeight (Css.num 1.4)
            , Css.textAlign Css.left
            , Css.position Css.relative
            , Css.color Colors.gray20
            , Fonts.baseFont
            , Css.fontSize (Css.px 18)
            , Css.displayFlex
            , Css.property "gap" "30px"
            , Css.flexDirection Css.column

            -- Approximately one line of text
            , Css.minHeight (Css.px 53)
            , case config.theme of
                Tip ->
                    Css.batch
                        [ Css.padding2 (Css.px 15) (Css.px 25)
                        ]

                _ ->
                    Css.batch
                        [ Css.borderLeft3 (Css.px 1) Css.solid Colors.gray85
                        , Css.borderTop3 (Css.px 1) Css.solid Colors.gray85
                        , Css.borderRight3 (Css.px 1) Css.solid Colors.gray85
                        , Css.borderTopLeftRadius (Css.px borderRounding)
                        , Css.borderTopRightRadius (Css.px borderRounding)
                        , Css.padding4 (Css.px 15) (Css.px 25) (Css.px 9) (Css.px 25)
                        , Css.marginBottom (Css.px 6)
                        ]
            ]
            []
            (List.filterMap identity
                [ Maybe.map viewLeftActions config.leftActions
                , Maybe.map (viewGuidance config maybeCharacter config.characterPosition) config.markdown
                , viewActions config.actions config.actionOrientation
                ]
            )
        ]


viewLeftActions : Html msg -> Html msg
viewLeftActions contents =
    styled div
        [ Css.display Css.inlineBlock
        , Css.position Css.absolute
        , Css.top (Css.px 16)
        , Css.left (Css.px 0)
        , Css.transform (Css.translateX (Css.pct -50))
        ]
        []
        [ contents ]


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
    { config | id : Maybe String }
    -> Maybe { name : String, icon : Svg }
    -> Maybe CharacterPosition
    -> String
    -> Html msg
viewGuidance config maybeCharacter maybeCharacterPosition markdown_ =
    case maybeCharacter of
        Just character_ ->
            div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.rowReverse
                    , Css.width (Css.pct 100)
                    , Css.property "gap" "10px"
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.spaceBetween
                    ]
                ]
                -- We intentionally render the character first and use "rowReverse" so
                -- that a11y content is presented as "Pands says ..." even
                -- though the character is floating on the right.
                [ viewCharacter character_ maybeCharacterPosition
                , viewContents config markdown_
                ]

        Nothing ->
            viewContents config markdown_


viewContents : { config | id : Maybe String } -> String -> Html msg
viewContents config markdown_ =
    styled div
        [ Css.alignSelf Css.flexStart
        , Css.width (Css.pct 100)
        , Css.lineHeight (Css.px 28)
        , Css.Global.children
            [ Css.Global.p
                [ Css.margin Css.zero
                , Css.marginBottom (Css.px 10)
                , Css.lastOfType [ Css.marginBottom Css.zero ]
                ]
            , Css.Global.ul
                [ Css.marginTop (Css.px 5)
                , Css.marginBottom (Css.px 10)
                ]
            ]
        ]
        [ AttributesExtra.maybe (guidanceId >> Attributes.id) config.id ]
        (Content.markdownInline markdown_)


viewCharacter : { name : String, icon : Svg } -> Maybe CharacterPosition -> Html msg
viewCharacter { name, icon } maybePosition =
    case maybePosition of
        Just position ->
            div [ css [ Css.width (Css.px position.width) ] ]
                [ div
                    [ css
                        [ Css.position Css.relative
                        ]
                    ]
                    [ icon
                        |> Svg.withLabel (name ++ " says, ")
                        |> Svg.withWidth (Css.px 60)
                        |> Svg.withCss
                            [ Css.right (Css.px -15)
                            , Css.top (Css.px position.top)
                            , Css.width (Css.px position.width)
                            , Css.height (Css.px position.height)
                            , Css.position Css.absolute
                            ]
                        |> Svg.toHtml
                    ]
                ]

        Nothing ->
            icon
                |> Svg.withLabel (name ++ " says, ")
                |> Svg.withWidth (Css.px 60)
                |> Svg.withCss
                    [ Css.position Css.relative
                    , Css.right (Css.px -15)
                    ]
                |> Svg.toHtml


viewActions : List (Action msg) -> ActionOrientation -> Maybe (Html msg)
viewActions actions_ actionOrientation =
    let
        containerStyles =
            [ Css.margin Css.zero
            , Css.listStyle Css.none
            , Css.padding Css.zero
            ]
                ++ orientationStyles

        orientationStyles =
            case actionOrientation of
                Horizontal ->
                    [ Css.displayFlex
                    , Css.property "gap" "10px"
                    , Css.flexDirection Css.row
                    ]

                Vertical ->
                    [ Css.displayFlex
                    , Css.property "gap" "15px"
                    , Css.flexDirection Css.column
                    ]

        buttonSize =
            case ( actions_, actionOrientation ) of
                ( _, Horizontal ) ->
                    Button.medium

                ( _ :: [], _ ) ->
                    Button.medium

                _ ->
                    Button.small

        buttonAlignment =
            case ( actions_, actionOrientation ) of
                ( _ :: _ :: _, Vertical ) ->
                    -- With multiple vertically stacked buttons we want the text to be left aligned.
                    [ Css.justifyContent Css.flexStart ]

                _ ->
                    []
    in
    case actions_ of
        [] ->
            Nothing

        { label, theme, onClick } :: [] ->
            div [ css (Css.alignItems Css.center :: containerStyles) ]
                [ Button.button label
                    [ Button.onClick onClick
                    , Button.fillContainerWidth
                    , buttonSize
                    , Button.css buttonAlignment
                    , theme
                    ]
                ]
                |> Just

        _ ->
            ul [ css containerStyles ]
                (List.map
                    (\{ label, theme, onClick } ->
                        li
                            [ css [ Css.flexGrow (Css.num 1) ]
                            ]
                            [ Button.button label
                                [ Button.onClick onClick
                                , Button.fillContainerWidth
                                , buttonSize
                                , Button.css buttonAlignment
                                , theme
                                ]
                            ]
                    )
                    actions_
                )
                |> Just
