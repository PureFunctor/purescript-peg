module Text.Parsing.PEG.CodePoints where

import Prelude

import Control.Alternative ((<|>))
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints as String
import Data.String.Pattern (Pattern(..))
import Text.Parsing.PEG.Expression (Expression(..), Node, fail)


-- | Modify the error message of an expression.
withError ∷ ∀ t r. Expression t r → String → Expression t r
withError expression error = expression <|> fail error

infixl 3 withError as <?>


-- | Match any character.
anyChar ∷ ∀ t. Expression t Char
anyChar = Expression anyChar'
  where
  anyChar' ∷ Node t → _
  anyChar' node@{ position, string, cache } =
    case String.codePointAt position string of
      Just codepoint →
        case fromCharCode $ fromEnum codepoint of
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
                 , error : "CodePoint " <> show codepoint <> " is not a character"
                 }
      Nothing →
        Left { node
             , position
             , error : "Unexpected EOF"
             }


-- | Match a character that satisfies a predicate.
satisfy ∷ ∀ t. (Char → Boolean) → Expression t Char
satisfy predicate = do
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
