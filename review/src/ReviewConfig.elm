module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []

    -- sometimes we just want to build a value without extracting it
    -- , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule

    -- We want to include all functions even if they're unused in this repository.
    -- , NoUnused.Exports.rule
    , NoUnused.Modules.rule

    -- We like to keep parameters around for readability.
    -- , NoUnused.Parameters.rule
    -- , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
