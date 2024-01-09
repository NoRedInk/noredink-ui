module CommonControls exposing
    ( css, mobileCss, quizEngineMobileCss, narrowMobileCss, notMobileCss, css_
    , choice
    , icon, iconNotCheckedByDefault, rightIcon
    , uiIcon, rotatedUiIcon
    , customIcon
    , specificColor
    , string
    , content
    , httpError, badBodyString
    , guidanceAndErrorMessage
    , disabledListItem, premiumDisplay
    )

{-|

@docs css, mobileCss, quizEngineMobileCss, narrowMobileCss, notMobileCss, css_
@docs choice
@docs icon, iconNotCheckedByDefault, rightIcon
@docs uiIcon, rotatedUiIcon
@docs customIcon
@docs specificColor


### Content

@docs string
@docs content
@docs httpError, badBodyString
@docs guidanceAndErrorMessage

-}

import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Examples.Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.Svg.V1 exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


specificColor : String -> Control ( String, Css.Color )
specificColor match =
    Examples.Colors.all
        |> List.map
            (\( name, value ) ->
                ( name, Control.value ( "Colors." ++ name, value ) )
            )
        |> ControlExtra.specificChoice match


premiumDisplay : Control ( String, PremiumDisplay )
premiumDisplay =
    Control.choice
        [ ( "Free", Control.value ( "PremiumDisplay.Free", PremiumDisplay.Free ) )
        , ( "Premium Locked", Control.value ( "PremiumDisplay.PremiumLocked", PremiumDisplay.PremiumLocked ) )
        , ( "Premium Unlocked", Control.value ( "PremiumDisplay.PremiumUnlocked", PremiumDisplay.PremiumUnlocked ) )
        , ( "Premium Vouchered", Control.value ( "PremiumDisplay.PremiumVouchered", PremiumDisplay.PremiumVouchered ) )
        ]


httpError : Control Http.Error
httpError =
    Control.choice
        [ ( "Bad Url", Control.value (Http.BadUrl "/request-url") )
        , ( "Timeout", Control.value Http.Timeout )
        , ( "Network Error", Control.value Http.NetworkError )
        , ( "Bad Status: 401", Control.value (Http.BadStatus 401) )
        , ( "Bad Status: 404", Control.value (Http.BadStatus 404) )
        , ( "Bad Status: ???", Control.value (Http.BadStatus 500) )
        , ( "Bad Body (often, a JSON decoding problem)"
          , Control.value (Http.BadBody badBodyString)
          )
        ]


badBodyString : String
badBodyString =
    """
    The Json.Decode.oneOf at json.draft failed in the following 2 ways:



    (1) Problem with the given value:

        null

        Expecting an OBJECT with a field named `content`



    (2) Problem with the given value:

        null

        Expecting an OBJECT with a field named `code`
    """


string : ( String, String -> v ) -> String -> Control ( String, v )
string ( fName, f ) startingString =
    Control.map
        (\str ->
            ( fName ++ " " ++ Code.string str
            , f str
            )
        )
        (Control.string startingString)


content :
    { moduleName : String
    , paragraph : Maybe (String -> attribute)
    , plaintext : String -> attribute
    , markdown : Maybe (String -> attribute)
    , html : List (Html msg) -> attribute
    , httpError : Maybe (Http.Error -> attribute)
    }
    -> Control ( String, attribute )
content ({ moduleName } as config) =
    Control.choice
        ([ case config.paragraph of
            Just f ->
                Just
                    ( "paragraph"
                    , string ( Code.fromModule moduleName "paragraph", f ) quickBrownFox
                    )

            Nothing ->
                Nothing
         , Just
            ( "plain text (short)"
            , string ( Code.fromModule moduleName "plaintext", config.plaintext ) quickBrownFox
            )
         , Just
            ( "plain text (long, no newlines)"
            , Control.string longPangrams
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext \"" ++ str ++ "\""
                        , config.plaintext str
                        )
                    )
            )
         , Just
            ( "plain text (long, with newlines)"
            , Control.stringTextarea romeoAndJulietQuotation
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext\n\t\t\"\"\"" ++ str ++ "\t\t\"\"\""
                        , config.plaintext str
                        )
                    )
            )
         , case config.markdown of
            Just markdown_ ->
                Just
                    ( "markdown"
                    , Control.string markdown
                        |> Control.map
                            (\str ->
                                ( moduleName ++ ".markdown \"" ++ str ++ "\""
                                , markdown_ str
                                )
                            )
                    )

            Nothing ->
                Nothing
         , Just
            ( "HTML"
            , Control.value
                ( moduleName ++ ".html [ ... ]"
                , config.html exampleHtml
                )
            )
         , case config.httpError of
            Just httpError_ ->
                Just
                    ( "httpError"
                    , Control.map
                        (\error ->
                            ( moduleName ++ ".httpError error"
                            , httpError_ error
                            )
                        )
                        httpError
                    )

            Nothing ->
                Nothing
         ]
            |> List.filterMap identity
        )


quickBrownFox : String
quickBrownFox =
    "The quick brown fox jumps over the lazy dog."


longPangrams : String
longPangrams =
    "Waltz, bad nymph, for quick jigs vex. Glib jocks quiz nymph to vex dwarf. Sphinx of black quartz, judge my vow. How vexingly quick daft zebras jump!"


romeoAndJulietQuotation : String
romeoAndJulietQuotation =
    """
        Two households, both alike in dignity,
        In fair Verona, where we lay our scene,
        From ancient grudge break to new mutiny,
        Where civil blood makes civil hands unclean.
        From forth the fatal loins of these two foes
        A pair of star-cross’d lovers take their life;
        Whose misadventured piteous overthrows
        Do with their death bury their parents’ strife.
        The fearful passage of their death-mark’d love,
        And the continuance of their parents’ rage,
        Which, but their children’s end, nought could remove,
        Is now the two hours’ traffic of our stage;
        The which if you with patient ears attend,
        What here shall miss, our toil shall strive to mend.
    """


markdown : String
markdown =
    """
_Katie's dad suggests:_ Don't tip too much, or your waitress will **fall over**!

A new paragraph and [last but not link](#)
"""


exampleHtml : List (Html msg)
exampleHtml =
    [ Html.text "This is a "
    , Html.strong [] [ Html.text "bolded phrase" ]
    , Html.text ". "
    , ClickableText.link quickBrownFox
        [ ClickableText.small
        , ClickableText.icon UiIcon.starFilled
        , ClickableText.href "http://www.noredink.com"
        , ClickableText.appearsInline
        ]
    , Html.text " When I stepped out, into the bright sunlight from the "
    , Html.a
        [ Attributes.href "https://example.com" ]
        [ Html.text "darkness of the movie house " ]
    , Html.text "I had only two things on my mind: Paul Newman, and a ride home."
    ]


icon :
    String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
icon =
    customIcon uiIcon


customIcon :
    Control ( String, Svg )
    -> String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
customIcon iconList moduleName f =
    ControlExtra.optionalListItemDefaultChecked "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( moduleName ++ ".icon " ++ iconName, f iconValue )
            )
            iconList
        )


iconNotCheckedByDefault :
    String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
iconNotCheckedByDefault =
    customIconNotCheckedByDefault uiIcon


customIconNotCheckedByDefault :
    Control ( String, Svg )
    -> String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
customIconNotCheckedByDefault iconList moduleName f =
    ControlExtra.optionalListItem "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( moduleName ++ ".icon " ++ iconName, f iconValue )
            )
            iconList
        )


rightIcon :
    String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
rightIcon moduleName f =
    ControlExtra.optionalListItem "rightIcon"
        (List.map
            (\( displayName, code, iconValue ) ->
                ( displayName
                , Control.value
                    ( moduleName ++ ".rightIcon " ++ code
                    , f iconValue
                    )
                )
            )
            [ ( "arrowDown", "UiIcon.arrowDown", UiIcon.arrowDown )
            , ( "openInNewTab"
              , "(Svg.withLabel \"Opens in new tab\" UiIcon.openInNewTab)"
              , UiIcon.openInNewTab
              )
            ]
            |> Control.choice
        )


uiIcons : List ( String, Svg )
uiIcons =
    [ ( "arrowLeft", UiIcon.arrowLeft )
    , ( "gear", UiIcon.gear )
    , ( "unarchive", UiIcon.unarchive )
    , ( "share", UiIcon.share )
    , ( "preview", UiIcon.preview )
    , ( "skip", UiIcon.skip )
    , ( "copyToClipboard", UiIcon.copyToClipboard )
    , ( "gift", UiIcon.gift )
    , ( "home", UiIcon.home )
    , ( "library", UiIcon.library )
    , ( "searchInCicle", UiIcon.searchInCicle )
    ]


uiIcon : Control ( String, Svg )
uiIcon =
    choice "UiIcon" uiIcons


rotatedUiIcon : Int -> Control ( String, Svg )
rotatedUiIcon by =
    uiIcons
        |> List.map
            (\( name, value ) ->
                ( name, Control.value ( "UiIcon." ++ name, value ) )
            )
        |> ControlExtra.rotatedChoice by


choice : String -> List ( String, value ) -> Control ( String, value )
choice moduleName options =
    options
        |> List.map
            (\( name, value ) ->
                ( name, Control.value ( moduleName ++ "." ++ name, value ) )
            )
        |> Control.choice


disabledListItem : String -> (Bool -> b) -> Control (List ( String, b )) -> Control (List ( String, b ))
disabledListItem moduleName disabled =
    ControlExtra.optionalBoolListItem "disabled"
        ( moduleName ++ ".disabled True"
        , disabled True
        )


css :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
css =
    css_ "css"
        ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
        , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
        )


mobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
mobileCss =
    css_ "mobileCss"
        ( "[ Css.border3 (Css.px 4) Css.dotted Colors.orange ]"
        , [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]
        )


quizEngineMobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
quizEngineMobileCss =
    css_ "quizEngineMobileCss"
        ( "[ Css.border3 (Css.px 4) Css.solid Colors.aqua |> Css.important ]"
        , [ Css.border3 (Css.px 4) Css.solid Colors.aqua |> Css.important ]
        )


narrowMobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
narrowMobileCss =
    css_ "narrowMobileCss"
        ( "[ Css.border3 (Css.px 8) Css.solid Colors.magenta |> Css.important ]"
        , [ Css.border3 (Css.px 8) Css.solid Colors.magenta |> Css.important ]
        )


notMobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
notMobileCss =
    css_ "notMobileCss"
        ( "[ Css.backgroundColor Colors.purple ]"
        , [ Css.backgroundColor Colors.purple ]
        )


css_ :
    String
    -> ( String, List Css.Style )
    ->
        { moduleName : String
        , use : List Css.Style -> b
        }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
css_ helperName ( styles, default ) { moduleName, use } =
    ControlExtra.optionalListItem helperName
        (Control.value
            ( moduleName ++ "." ++ helperName ++ " " ++ styles
            , use default
            )
        )


guidanceAndErrorMessage :
    { moduleName : String
    , guidance : String -> b
    , guidanceHtml : List (Html msg) -> b
    , message : String
    , errorMessage : Maybe (Maybe String -> b)
    }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
guidanceAndErrorMessage ({ moduleName } as config) controls =
    [ Control.choice
        [ ( "string"
          , Control.string config.message
                |> Control.map
                    (\str ->
                        ( Code.fromModule moduleName "guidance " ++ Code.string str
                        , config.guidance str
                        )
                    )
          )
        , ( "html"
          , Control.value
                ( Code.fromModule moduleName "guidanceHtml [ text \"There is \", b [] [ text \"something\" ], text \" you need to be aware of.\" ]"
                , config.guidanceHtml [ Html.text "There is ", Html.b [] [ Html.text "something" ], Html.text " you need to be aware of." ]
                )
          )
        ]
        |> ControlExtra.optionalListItem "guidance"
        |> Just
    , Maybe.map
        (\errorMessage ->
            ControlExtra.optionalListItem "errorMessage"
                (Control.map
                    (\str ->
                        ( Code.fromModule moduleName "errorMessage " ++ Code.withParens (Code.maybeString (Just str))
                        , errorMessage (Just str)
                        )
                    )
                    (Control.string config.message)
                )
        )
        config.errorMessage
    ]
        |> List.filterMap identity
        |> List.foldl (\f a -> f a) controls
