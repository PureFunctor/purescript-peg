{ name = "peg"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "enums"
  , "exists"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tailrec"
  , "tuples"
  , "typelevel-prelude"
  , "unordered-collections"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-peg.git"
}
