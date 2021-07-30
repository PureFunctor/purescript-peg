module Text.Parsing.PEG.CodePoints where

import Prelude

import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (manyRec)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Traversable (foldMap)
import Text.Parsing.PEG (Expression(..), fail, try, (<?>))


-- | Match the end-of-file.
eof ∷ ∀ t. Expression t Unit
eof = Expression eof'
  where
  eof' node@{ position, string }
    | position < String.length string = Left { node, position, error : "Expected EOF" }
    | otherwise = Right { result: unit, next: node }


-- | Match any code point.
anyCodePoint ∷ ∀ t. Expression t CodePoint
anyCodePoint = Expression anyCodePoint'
  where
  anyCodePoint' node@{ position, string, cache } =
    case String.codePointAt position string of
      Just codepoint →
        Right { result : codepoint
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


-- | Match any character.
anyChar ∷ ∀ t. Expression t Char
anyChar = try do
  codepoint ← anyCodePoint
  case fromCharCode $ fromEnum codepoint of
    Just c → pure c
    Nothing → fail $ "CodePoint " <> show codepoint <> " is not a character"


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


-- | Match a literal string.
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


-- | Match an inclusive range of code points.
range ∷ ∀ t. CodePoint → CodePoint → Expression t CodePoint
range low high = try do -- Expression range'
  codepoint ← anyCodePoint
  if low <= codepoint && codepoint <= high
    then pure codepoint
    else fail $ "Not in range of " <> show low <> " and " <> show high


-- | Match many whitespace characters.
whiteSpace ∷ ∀ t. Expression t String
whiteSpace = do
  ws ← manyRec (satisfy (\c → c == ' ' || c == '\t' || c == '\n' || c == '\r'))
  pure (foldMap SCU.singleton ws)
