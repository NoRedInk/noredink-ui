module CommonControls2 exposing
    ( css, mobileCss, quizEngineMobileCss, notMobileCss
    , choice
    , icon, iconNotCheckedByDefault, uiIcon
    , content
    , httpError
    , disabledListItem, premiumDisplay
    )

{-|

@docs css, mobileCss, quizEngineMobileCss, notMobileCss
@docs choice
@docs icon, iconNotCheckedByDefault, uiIcon


### Content

@docs content
@docs httpError

-}

import Code exposing (f, hardcode, val)
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Elm exposing (Expression)
import Html.Styled as Html exposing (Html)
import Http
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.Svg.V1 exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


premiumDisplay : Control ( Expression, PremiumDisplay )
premiumDisplay =
    Control.choice
        [ ( "Free", Control.value ( Elm.string "TODO", PremiumDisplay.Free ) )
        , ( "Premium Locked", Control.value ( Elm.string "TODO", PremiumDisplay.PremiumLocked ) )
        , ( "Premium Unlocked", Control.value ( Elm.string "TODO", PremiumDisplay.PremiumUnlocked ) )
        ]


httpError : Control ( Expression, Http.Error )
httpError =
    Control.choice
        [ ( "Bad Url", Control.value ( Elm.string "TODO", Http.BadUrl "/request-url" ) )
        , ( "Timeout", Control.value ( Elm.string "TODO", Http.Timeout ) )
        , ( "Network Error", Control.value ( Elm.string "TODO", Http.NetworkError ) )
        , ( "Bad Status: 401", Control.value ( Elm.string "TODO", Http.BadStatus 401 ) )
        , ( "Bad Status: 404", Control.value ( Elm.string "TODO", Http.BadStatus 404 ) )
        , ( "Bad Status: ???", Control.value ( Elm.string "TODO", Http.BadStatus 500 ) )
        , ( "Bad Body (often, a JSON decoding problem)"
          , Control.value
                ( Elm.string "TODO"
                , Http.BadBody
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
    -> Control ( Expression, attribute )
content ({ moduleName } as config) =
    Control.choice
        ([ ( "plain text (short)"
           , Control.string quickBrownFox
                |> Control.map
                    (\str ->
                        ( f moduleName "plaintext" [ Elm.string str ]
                        , config.plaintext str
                        )
                    )
           )
         , ( "plain text (long, no newlines)"
           , Control.string longPangrams
                |> Control.map
                    (\str ->
                        ( f moduleName "plaintext" [ Elm.string str ]
                        , config.plaintext str
                        )
                    )
           )
         , ( "plain text (long, with newlines)"
           , Control.stringTextarea romeoAndJulietQuotation
                |> Control.map
                    (\str ->
                        ( f moduleName "plaintext" [ Elm.string str ]
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
                                        ( f moduleName "markdown" [ Elm.string str ]
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
                    ( f moduleName "html" [ Elm.list [] ]
                    , config.html exampleHtml
                    )
               )
            :: (case config.httpError of
                    Just httpError_ ->
                        [ ( "httpError"
                          , Control.map
                                (\( errorExp, error ) ->
                                    ( f moduleName "httpError" [ errorExp ]
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
    -> Control (List ( Expression, value ))
    -> Control (List ( Expression, value ))
icon moduleName f_ =
    ControlExtra.optionalListItemDefaultChecked "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( f moduleName "icon" [ iconName ], f_ iconValue )
            )
            uiIcon
        )


iconNotCheckedByDefault :
    String
    -> (Svg -> value)
    -> Control (List ( Expression, value ))
    -> Control (List ( Expression, value ))
iconNotCheckedByDefault moduleName f_ =
    ControlExtra.optionalListItem "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( f moduleName "icon" [ iconName ], f_ iconValue )
            )
            uiIcon
        )


uiIcon : Control ( Expression, Svg )
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


choice : String -> List ( String, value ) -> Control ( Expression, value )
choice moduleName options =
    options
        |> List.map
            (\( name, value ) ->
                ( name, Control.value ( val moduleName name, value ) )
            )
        |> Control.choice


disabledListItem : String -> (Bool -> b) -> Control (List ( Expression, b )) -> Control (List ( Expression, b ))
disabledListItem moduleName disabled =
    ControlExtra.optionalBoolListItem "disabled"
        ( f moduleName "disabled" [ Elm.bool True ]
        , disabled True
        )


css :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( Expression, b ))
    -> Control (List ( Expression, b ))
css =
    css_ "css"
        ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
        , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
        )


mobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( Expression, b ))
    -> Control (List ( Expression, b ))
mobileCss =
    css_ "mobileCss"
        ( "[ Css.border3 (Css.px 4) Css.dotted Colors.orange ]"
        , [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]
        )


quizEngineMobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( Expression, b ))
    -> Control (List ( Expression, b ))
quizEngineMobileCss =
    css_ "quizEngineMobileCss"
        ( "[ Css.border3 (Css.px 4) Css.solid Colors.aqua |> Css.important ]"
        , [ Css.border3 (Css.px 4) Css.solid Colors.aqua |> Css.important ]
        )


notMobileCss :
    { moduleName : String, use : List Css.Style -> b }
    -> Control (List ( Expression, b ))
    -> Control (List ( Expression, b ))
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
    -> Control (List ( Expression, b ))
    -> Control (List ( Expression, b ))
css_ helperName ( styles, default ) { moduleName, use } =
    ControlExtra.optionalListItem helperName
        (Control.value
            ( f moduleName helperName [ hardcode styles ]
            , use default
            )
        )
