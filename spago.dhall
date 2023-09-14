{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
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
