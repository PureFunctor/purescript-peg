module Text.Parsing.PEG.Cache where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Text.Parsing.PEG.Types (Position, Delta, Tag)
import Type.Prelude (class IsSymbol, Proxy)
import Unsafe.Coerce (unsafeCoerce)


-- | A key in a cache.
-- |
-- | Implemented as a Tuple newtype as to avoid the overhead from the
-- | Hashable instance for Record.
newtype Key = Key (Tuple Position Tag)

derive instance Newtype Key _

derive newtype instance Eq Key

derive newtype instance Hashable Key


-- | An untagged entry in a cache.
type UntaggedEntry e =
  { entry ∷ e
  , delta ∷ Delta
  }


-- | A tagged entry in a cache.
type Entry e =
  { entry ∷ e
  , delta ∷ Delta
  , tag ∷ Tag
  }


-- | A mapping of tagged keys to tagged entries. The type parameter `r`
-- | is existentially quantified through `Exists` to [un]safely remove
-- | static guarantees.
newtype Cache_ r = Cache_ (HashMap Key (Entry r))


-- | A mapping of tagged keys to tagged entries. The type parameter `t`
-- | declares a row of symbol tags and types consumed by operations on
-- | the cache.
-- |
-- | Example:
-- | ```purescript
-- | type Rules = ( _A ∷ String, _B ∷ Number )
-- |
-- | cache ∷ Cache Rules
-- | cache = empty
-- | ```
newtype Cache ∷ Row Type → Type
newtype Cache t = Cache (Exists Cache_)


-- | An empty cache.
empty ∷ ∀ t. Cache t
empty = Cache $ mkExists $ Cache_ HashMap.empty


-- | Look up an entry in a cache given a tag and a position.
-- |
-- | The provided `tag` must be a member of the `tags` assigned to the
-- | cache, which allows the `entry` type to be statically determined.
-- |
-- | Example:
-- | ```purescript
-- | type Rules = ( _A ∷ String, _B ∷ Number )
-- |
-- | cache ∷ Cache Rules
-- | cache = …
-- |
-- | _A ∷ Proxy "_A"
-- | _A = Proxy
-- |
-- | atZero ∷ Maybe (Entry String)
-- | atZero = lookup _A 0 cache
-- | ```
lookup ∷
  forall tag entry rest tags
  .  IsSymbol tag
  => Cons tag entry rest tags
  => Proxy tag
  -> Position
  -> Cache tags
  -> Maybe (Entry entry)
lookup name position (Cache m) = runExists lookup_ m
  where
  lookup_ ∷ ∀ inner. Cache_ inner → Maybe (Entry entry)
  lookup_ (Cache_ m_) = coerceE (HashMap.lookup k m_)
    where
    tag ∷ String
    tag = reflectSymbol name

    k ∷ Key
    k = Key (Tuple position tag)

    coerceE ∷ Maybe (Entry inner) → Maybe (Entry entry)
    coerceE = unsafeCoerce


-- | Insert or modify an entry in a cache given a tag, a position, and
-- | an untagged entry.
-- |
-- | The provided `tag` must be a member of the `tags` assigned to the
-- | cache.
-- |
-- | Example:
-- | ```purescript
-- | type Rules = ( _A ∷ String, _B ∷ Number )
-- |
-- | cache ∷ Cache Rules
-- | cache = …
-- |
-- | _A ∷ Proxy "_A"
-- | _A = Proxy
-- |
-- | atZero ∷ Cache Rules
-- | atZero = insert _A 0 { delta: 1, entry: "v" }
-- | ```
insert ∷
  forall tag entry rest tags
  .  IsSymbol tag
  => Cons tag entry rest tags
  => Proxy tag
  -> Position
  -> UntaggedEntry entry
  -> Cache tags
  -> Cache tags
insert name position { delta, entry } (Cache m) = runExists insert_ m
  where
  insert_ ∷ ∀ inner. Cache_ inner → Cache tags
  insert_ (Cache_ m_) = Cache (mkExists (Cache_ (HashMap.insert k (coerceI v) m_)))
    where
    tag ∷ Tag
    tag = reflectSymbol name

    k ∷ Key
    k = Key (Tuple position tag)

    v ∷ Entry entry
    v = { entry, delta, tag }

    coerceI ∷ Entry entry → Entry inner
    coerceI = unsafeCoerce
