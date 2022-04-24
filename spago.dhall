{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "dotenv"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "formatters"
  , "httpure"
  , "integers"
  , "interpolate"
  , "lazy"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-postgres"
  , "node-process"
  , "node-readline-aff"
  , "now"
  , "numbers"
  , "option"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "react"
  , "react-basic-dom"
  , "record"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
