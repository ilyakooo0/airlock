module Ur.Types exposing
    ( Noun(..), Atom
    , Agent, Mark, Path, Ship
    )

{-|


# Nouns

@docs Noun, Atom


# Types

@docs Agent, Mark, Path, Ship

-}

import Bytes exposing (Bytes)


{-| An [Urbit agent](https://developers.urbit.org/reference/glossary/agent) (app) name like `journal` or `groups`.
-}
type alias Agent =
    String


{-| An [Urbit subscription path](https://developers.urbit.org/reference/glossary/path).
-}
type alias Path =
    List String


{-| An [Urbit mark](https://developers.urbit.org/reference/arvo/clay/marks/marks)
-}
type alias Mark =
    String


{-| A ship name like `~zod` or `~racfer-hattes`.

Also know as `@p`.

-}
type alias Ship =
    String


{-| An [Urbit Noun](https://developers.urbit.org/reference/glossary/noun).
-}
type Noun
    = Cell ( Noun, Noun )
    | Atom Atom


{-| An [Urbit Atom](https://developers.urbit.org/reference/glossary/atom)
-}
type alias Atom =
    Bytes
