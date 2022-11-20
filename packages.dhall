{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220816/packages.dhall
        sha256:8b4467b4b5041914f9b765779c8936d6d4c230b1f60eb64f6269c71812fd7e98

in  upstream 
  with node-readline-aff =
    { dependencies =
      [ "aff"
      , "console"
      , "effect"
      , "either"
      , "node-readline"
      , "prelude"
      , "psci-support"
      , "strings"
      ]
    , repo = "https://github.com/ntwilson/purescript-node-readline-aff"
    , version = "master"
    }
  with option =
    { dependencies =
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "codec"
      , "codec-argonaut"
      , "datetime"
      , "effect"
      , "either"
      , "enums"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "identity"
      , "lists"
      , "maybe"
      , "prelude"
      , "profunctor"
      , "record"
      , "simple-json"
      , "strings"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/ntwilson/purescript-option"
    , version = "main"
    }
  with concur-react =
    { repo = "https://github.com/purescript-concur/purescript-concur-react"
    , version = "v0.5.0"
    , dependencies =
      [ "aff"
      , "arrays"
      , "concur-core"
      , "console"
      , "effect"
      , "either"
      , "exceptions"
      , "maybe"
      , "prelude"
      , "react"
      , "react-dom"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    }
  with concur-core =
    { dependencies =
      [ "aff"
      , "aff-bus"
      , "arrays"
      , "avar"
      , "console"
      , "control"
      , "datetime"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "free"
      , "identity"
      , "lazy"
      , "maybe"
      , "newtype"
      , "parallel"
      , "prelude"
      , "profunctor-lenses"
      , "tailrec"
      , "transformers"
      , "tuples"
      ]
    , repo = "https://github.com/purescript-concur/purescript-concur-core"
    , version = "v0.5.0"
    }