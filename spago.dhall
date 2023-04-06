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
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
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
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
