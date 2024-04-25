module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.ImportSimple
import Review.Rule


config : List Review.Rule.Rule
config =
    [ Review.ImportSimple.rule
    ]
