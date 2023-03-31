{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "math"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "rationals"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
