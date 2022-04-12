module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoUnused.CustomTypeConstructors
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    List.map
        (Review.Rule.ignoreErrorsForDirectories [ "../src" ]
            >> Review.Rule.ignoreErrorsForFiles [ "../styleguide-app/App.elm" ]
        )
        [ NoUnused.CustomTypeConstructors.rule []

        -- sometimes we just want to build a value without extracting it
        -- , NoUnused.CustomTypeConstructorArgs.rule
        -- this rules is not useful in here
        -- , NoUnused.Dependencies.rule
        , NoUnused.Exports.rule
        , NoUnused.Modules.rule

        -- We like to keep parameters around for readability.
        -- , NoUnused.Parameters.rule
        -- , NoUnused.Patterns.rule
        , NoUnused.Variables.rule
        ]
