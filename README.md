The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule
[🔧 `Review.ImportSimple.rule`](https://package.elm-lang.org/packages/lue-bird/elm-review-import-simple/1.0.0/Review-ImportSimple/#rule "provides fixes")
limits all imports to the format
```elm
import Module.Name
-- ✅ only module name
import Module.Name exposing (ModuleName)
-- ✅ only module name + exposed matching type name ignoring .s
```

Anything else is reported:
```elm
import Test exposing (test)
-- ❌ values/functions
import ListNonEmpty exposing (ListNonEmpty(..))
-- ❌ variants
import Elm.Syntax.Expression as Expression
-- ❌ import alias
import Parser.Advanced exposing (Parser)
-- ❌ type only prefix of module name
import Elm.Syntax.Expression exposing (Expression)
-- ❌ type only suffix of module name
import Morph exposing (MorphIndependently)
-- ❌ type only shares module prefix
```

## try it

```bash
elm-review --template lue-bird/elm-review-import-simple/example
```

## add it to your config

```elm
module ReviewConfig exposing (config)

import Review.ImportSimple
import Review.Rule

config : List Review.Rule.Rule
config =
    [ Review.ImportSimple.rule
    ]
```

## why

### 👍 the module origin is obvious
Being able to find out where a reference comes from at a glance without mental hoops or editor interaction is nice.

### 👍 no inconsistent imports
Having a consistent import style in the whole project allows
  - moving code between modules/examples/chat forum
  - no need to switch mental context between modules ("oh, that's where we use exposed ui primitives")

Because these are quite valuable, finding a project-wide import style is a classic point of debate in teams.
Just remove the options.

If you don't want to give up aliases, at least consider
[`NoInconsistentAliases.rule`](https://dark.elm.dmy.fr/packages/sparksp/elm-review-imports/latest/NoInconsistentAliases#rule)

### 👍 no name overlap problems
An import exposing a value/function can be somewhat annoying
```elm
import Html exposing (img)
gallery |> List.map (\img -> -- name clash
viewIcon { color, img } = -- name clash
```

### 👍 simpler
Less to learn, less to keep in your head :)

### 🤷 guides you to consider different APIs
One example:
```elm
module ParallelHttp exposing (HttpRequest, HttpHeader, HttpExpectation, HttpResponse)
```
the advice "don't put part of the module name in front of function names" applies to types as well
```elm
module ParallelHttp exposing (Request, Header, Expectation, Response)
```
used as for example `ParallelHttp.Request`.

Another example
```elm
module List.NonEmpty exposing (NonEmpty)
```
it's not obvious that `NonEmpty` referring to a list.
```elm
module List.NonEmpty exposing (ListNonEmpty)
```


### 👎 record setters become ugly
```elm
{ defaultConfig | -- fine
{ MyIcon.defaultConfig | -- compiler error
```
This is likely an oversight in the language but a pain point nonetheless.

I suggest either converting the record to a builder instead or using
```elm
let
    myIconConfigWithOutline =
        MyIcon.defaultConfig
in
{ myIconConfigWithOutline | ...
```
or
```elm
MyIcon.defaultConfig |> (\r -> { r | ...
```
