module Text.Parsing.PEG.Combinators where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List, manyRec)
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.Maybe (Maybe(..))
import Data.Traversable as Traversable
import Text.Parsing.PEG (Expression(..), unExpression)


-- | Run expressions on ordered choice, returning the first match.
choice ∷ ∀ t r. Array (Expression t r) → Expression t r
choice expressions = Expression expression
  where
  expression baseNode = tailRec chooser { cache: baseNode.cache, exprs: expressions }
    where
    chooser { cache, exprs } =
      case Array.uncons exprs of
        Just { head, tail } ->
          case unExpression head ( baseNode { cache = cache } ) of
            Right result -> Done (Right result)
            Left { node } -> Loop { cache: node.cache, exprs: tail }
        Nothing ->
          Done $ Left { error: "No choice found", position: baseNode.position, node: baseNode }


-- | Run a sequence of expressions, collecting the results.
sequence ∷ ∀ t r. Array (Expression t r) → Expression t (Array r)
sequence = Traversable.sequence


-- | Match an expression zero or more times.
zeroOrMore ∷ ∀ t r. Expression t r → Expression t (List r)
zeroOrMore = manyRec


-- | Match an expression one or more times.
oneOrMore ∷ ∀ t r. Expression t r → Expression t (NonEmptyList r)
oneOrMore p = cons' <$> p <*> zeroOrMore p
