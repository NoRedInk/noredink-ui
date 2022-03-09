module CommonControls exposing
    ( css, mobileCss, quizEngineMobileCss, notMobileCss
    , choice
    , icon, iconNotCheckedByDefault, uiIcon
    , content
    , quickBrownFox, longPangrams, romeoAndJulietQuotation, markdown, exampleHtml, httpError
    , disabledListItem, premiumLevel
    )

{-|

@docs css, mobileCss, quizEngineMobileCss, notMobileCss
@docs choice
@docs icon, iconNotCheckedByDefault, uiIcon


### Content

@docs content
@docs quickBrownFox, longPangrams, romeoAndJulietQuotation, markdown, exampleHtml, httpError

-}

import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.Svg.V1 exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


premiumLevel : Control ( String, PremiumLevel )
premiumLevel =
    choice "PremiumLevel"
        [ ( "Free", Free )
        , ( "PremiumWithWriting", PremiumWithWriting )
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
    , markdown : String -> attribute
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
         , ( "markdown"
           , Control.string markdown
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".markdown \"" ++ str ++ "\""
                        , config.markdown str
                        )
                    )
           )
         , ( "HTML"
           , Control.value
                ( moduleName ++ ".html [ ... ]"
                , config.html exampleHtml
                )
           )
         ]
            ++ (case config.httpError of
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
icon moduleName f =
    ControlExtra.optionalListItemDefaultChecked "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( moduleName ++ ".icon " ++ iconName, f iconValue )
            )
            uiIcon
        )


iconNotCheckedByDefault :
    String
    -> (Svg -> value)
    -> Control (List ( String, value ))
    -> Control (List ( String, value ))
iconNotCheckedByDefault moduleName f =
    ControlExtra.optionalListItem "icon"
        (Control.map
            (\( iconName, iconValue ) ->
                ( moduleName ++ ".icon " ++ iconName, f iconValue )
            )
            uiIcon
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
disabledListItem moduleName f =
    ControlExtra.optionalBoolListItem "disabled"
        (\bool ->
            ( moduleName ++ ".disabled True"
            , f bool
            )
        )


css :
    { moduleName : String, use : List Css.Style -> b, default : String }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
css =
    css_ "css"


mobileCss :
    { moduleName : String, use : List Css.Style -> b, default : String }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
mobileCss =
    css_ "mobileCss"


quizEngineMobileCss :
    { moduleName : String, use : List Css.Style -> b, default : String }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
quizEngineMobileCss =
    css_ "quizEngineMobileCss"


notMobileCss :
    { moduleName : String, use : List Css.Style -> b, default : String }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
notMobileCss =
    css_ "notMobileCss"


css_ :
    String
    ->
        { moduleName : String
        , use : List Css.Style -> b
        , default : String
        }
    -> Control (List ( String, b ))
    -> Control (List ( String, b ))
css_ helperName { moduleName, use, default } =
    ControlExtra.optionalListItem helperName
        (Control.map
            (\( cssString, cssValue ) ->
                ( moduleName ++ "." ++ helperName ++ " " ++ cssString
                , use cssValue
                )
            )
            (ControlExtra.css default)
        )
