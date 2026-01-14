# change log

#### 1.0.4
  - fix bug where types with the same name as the imported module name were not exposed
  - fix bug where `exposing (..)` was kept instead of `exposing (ModuleName)` when `ModuleName` was used as a reference

#### 1.0.3
  - correct bug around using references with a name that is also declared locally.
    Thanks for [noticing](https://github.com/lue-bird/elm-review-import-simple/issues/1), [Wolfgang Schuster](https://github.com/wolfadex)!

#### 1.0.2
  - provide band-aid for incorrect handling of the `List` type by `ModuleNameLookupTable`

#### 1.0.1
  - stop reporting using unqualified references that are implicitly exposed
