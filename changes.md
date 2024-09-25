# change log

#### 1.0.3
  - correct bug around using references with a name that is also declared locally.
    Thanks for [noticing](https://github.com/lue-bird/elm-review-import-simple/issues/1), [Wolfgang Schuster](https://github.com/wolfadex)!

#### 1.0.2
  - provide band-aid for incorrect handling of the `List` type by `ModuleNameLookupTable`

#### 1.0.1
  - stop reporting using unqualified references that are implicitly exposed
