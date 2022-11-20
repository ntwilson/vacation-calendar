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
  , "foreign-object"
  , "formatters"
  , "httpure"
  , "integers"
  , "interpolate"
  , "js-date"
  , "lazy"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-http"
  , "node-process"
  , "node-readline-aff"
  , "now"
  , "numbers"
  , "option"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "react"
  , "react-basic-dom"
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
