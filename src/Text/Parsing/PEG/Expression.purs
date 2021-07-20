module Text.Parsing.PEG.Expression where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodePoints as String
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Text.Parsing.PEG.Cache (Cache)
import Text.Parsing.PEG.Cache as Cache
import Text.Parsing.PEG.Types (Position)
import Type.Proxy (Proxy)


-- | The input state of an expression.
type Node t =
  { position ∷ Position
  , string ∷ String
  , cache ∷ Cache t
  }


-- | An error that occurs during parsing.
type Error t =
  { position ∷ Position
  , error ∷ String
  , node ∷ Node t
  }


-- | The result of running an expression.
type Result r t =
  { result ∷ r
  , next ∷ Node t
  }


newtype Expression ∷ Row Type → Type → Type
newtype Expression t r = Expression (Node t → Either (Error t) (Result r t))


-- | Run an expression given a node.
unExpression ∷ ∀ t r. Expression t r → Node t → Either (Error t) (Result r t)
unExpression (Expression e) = e


-- | Run an expression given a string.
runExpression ∷ ∀ t r. Expression t r → String → Either (Error t) r
runExpression (Expression e) string =
  e { string, position: 0, cache: Cache.empty } <#> _.result


-- | Enables caching for an expression by turning it into a rule.
rule ∷
  ∀ name type' _tail tags
  .  IsSymbol name
  => Cons name type' _tail tags
  => Proxy name
  -> Expression tags type'
  -> Expression tags type'
rule name (Expression e) = Expression e'
  where
  e' ∷ Node tags → Either (Error tags) (Result type' tags)
  e' { position, string, cache } =
    case Cache.lookup name position cache of
      Just { entry : result, delta } →
        Right { result
              , next :
                { cache
                , position : position + delta
                , string : String.drop delta string
                }
              }
      Nothing → e { position, string, cache } <#> insertToCache
    where
    insertToCache ∷ Result type' tags → Result type' tags
    insertToCache { result, next } =
      let untagged =
            { delta : next.position - position
            , entry : result
            }
      in { result
         , next: next { cache = Cache.insert name position untagged cache }
         }


instance Functor (Expression t) where
  map f (Expression e) = Expression (map (\x → x { result = f x.result }) <<< e)


instance Apply (Expression t) where
  apply (Expression e1) (Expression e2) = Expression \s → do
    { result : f, next : t } ← e1 s
    { result : x, next : u } ← e2 t
    pure { result: f x, next: u }


instance Applicative (Expression t) where
  pure result = Expression \next → Right { result, next }


instance Bind (Expression t) where
  bind (Expression e) f = Expression \s → do
    { result, next } ← e s
    case f result of
      Expression e' → e' next


instance Alt (Expression t) where
  alt (Expression f) (Expression g) = Expression \node →
    case f node of
      Left error | node.position == error.position → g node
                 | otherwise → Left error
      right → right


instance Plus (Expression t) where
  empty = fail "No alternative"


instance Alternative (Expression t)


fail ∷ ∀ t r. String → Expression t r
fail error = Expression \node@{ position } → Left { error, position, node }
