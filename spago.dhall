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
  , "exceptions"
  , "math"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "rationals"
  , "spec"
  , "strings"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
