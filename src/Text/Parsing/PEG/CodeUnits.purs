module Text.Parsing.PEG.CodeUnits where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..))
import Text.Parsing.PEG (Expression(..), fail, try, (<?>))


-- | Match any character.
anyChar ∷ ∀ t. Expression t Char
anyChar = Expression anyChar'
  where
  anyChar' node@{ position, string, cache } =
    case String.charAt position string of
      Just c →
        Right { result : c
              , next : { cache
                      , string
                      , position : position + 1
                      }
              }
      Nothing →
        Left { node
             , position
             , error : "Unexpected EOF"
             }


-- | Match any digit.
anyDigit ∷ ∀ t. Expression t Char
anyDigit = try do
  c ← anyChar
  if c >= '0' && c <= '9'
    then pure c
    else fail $ "Character " <> show c <> " is not a digit"


-- | Match a character that satisfies a predicate.
satisfy ∷ ∀ t. (Char → Boolean) → Expression t Char
satisfy predicate = try do
  c ← anyChar
  if predicate c
    then pure c
    else fail $ "Could not match predicate with " <> show c


-- | Match a literal character.
character ∷ ∀ t. Char → Expression t Char
character c = satisfy (_ == c) <?> "Could not match character " <> show c


-- | Match a literal string
literal ∷ ∀ t. String → Expression t String
literal pattern = Expression literal'
  where
  literal' node@{ position, string, cache } =
    case String.indexOf' (Pattern pattern) position string of
      Just position' | position == position' → do
        Right { result : pattern
              , next : { cache
                       , string
                       , position : position + String.length pattern
                       }
              }
      _ →
        Left { node
             , position
             , error : "Could not match '" <> show string <> "'."
             }


-- | Match an inclusive range of code units.
range ∷ ∀ t. Char → Char → Expression t Char
range low high = try do
  code ← anyChar
  if low <= code && code <= high
    then pure code
    else fail $ "Not in range of " <> show low <> " and " <> show high
