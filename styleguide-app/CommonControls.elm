module CommonControls exposing
    ( css, mobileCss, quizEngineMobileCss, notMobileCss, css_
    , choice
    , icon, iconNotCheckedByDefault, uiIcon
    , customIcon, customIconNotCheckedByDefault
    , color
    , content
    , httpError
    , romeoAndJulietQuotation
    , disabledListItem, premiumDisplay
    )

{-|

@docs css, mobileCss, quizEngineMobileCss, notMobileCss, css_
@docs choice
@docs icon, iconNotCheckedByDefault, uiIcon
@docs customIcon, customIconNotCheckedByDefault
@docs color


### Content

@docs content
@docs httpError
@docs romeoAndJulietQuotation

-}

import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Examples.Colors
import Html.Styled as Html exposing (Html)
import Http
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.Svg.V1 exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


color : Control ( String, Css.Color )
color =
    choice "Colors" Examples.Colors.all


premiumDisplay : Control ( String, PremiumDisplay )
premiumDisplay =
    Control.choice
        [ ( "Free", Control.value ( "PremiumDisplay.Free", PremiumDisplay.Free ) )
        , ( "Premium Locked", Control.value ( "PremiumDisplay.PremiumLocked", PremiumDisplay.PremiumLocked ) )
        , ( "Premium Unlocked", Control.value ( "PremiumDisplay.PremiumUnlocked", PremiumDisplay.PremiumUnlocked ) )
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
          , Control.value
                (Http.BadBody
                    """
                        The Json.Decode.oneOf at json.draft failed in the following 2 ways:



                        (1) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `content`



                        (2) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `code`
                        """
                )
          )
        ]


content :
    { moduleName : String
    , plaintext : String -> attribute
    , markdown : Maybe (String -> attribute)
    , html : List (Html msg) -> attribute
    , httpError : Maybe (Http.Error -> attribute)
    }
    -> Control ( String, attribute )
content ({ moduleName } as config) =
    Control.choice
        ([ ( "plain text (short)"
           , Control.string quickBrownFox
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext \"" ++ str ++ "\""
                        , config.plaintext str
                        )
                    )
           )
         , ( "plain text (long, no newlines)"
           , Control.string longPangrams
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext \"" ++ str ++ "\""
                        , config.plaintext str
                        )
                    )
           )
         , ( "plain text (long, with newlines)"
           , Control.stringTextarea romeoAndJulietQuotation
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext\n\t\t\"\"\"" ++ str ++ "\t\t\"\"\""
                        , config.plaintext str
                        )
                    )
           )
         ]
            ++ (case config.markdown of
                    Just markdown_ ->
                        [ ( "markdown"
                          , Control.string markdown
                                |> Control.map
                                    (\str ->
                                        ( moduleName ++ ".markdown \"" ++ str ++ "\""
                                        , markdown_ str
                                        )
                                    )
                          )
                        ]

                    Nothing ->
                        []
               )
            ++ ( "HTML"
               , Control.value
                    ( moduleName ++ ".html [ ... ]"
                    , config.html exampleHtml
                    )
               )
            :: (case config.httpError of
                    Just httpError_ ->
                        [ ( "httpError"
                          , Control.map
                                (\error ->
                                    ( moduleName ++ ".httpError error"
                                    , httpError_ error
                                    )
                                )
                                httpError
                          )
                        ]

                    Nothing ->
                        []
               )
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
    "_Katie's dad suggests:_ Don't tip too much, or your waitress will **fall over**!"


exampleHtml : List (Html msg)
exampleHtml =
    [ Html.text "This is a "
    , Html.strong [] [ Html.text "bolded phrase" ]
    , Html.text ". "
    , ClickableText.link quickBrownFox
        [ ClickableText.small
        , ClickableText.icon UiIcon.starFilled
        , ClickableText.href "http://www.noredink.com"
        ]
    , Html.text " When I stepped out, into the bright sunlight from the darkness of the movie house, I had only two things on my mind: Paul Newman, and a ride home."
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


uiIcon : Control ( String, Svg )
uiIcon =
    [ ( "arrowLeft", UiIcon.arrowLeft )
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
        |> choice "UiIcon"


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
