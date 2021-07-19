module Text.Parsing.PEG.CodePoints where

import Prelude

import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints as String
import Text.Parsing.PEG.Expression (Expression(..), Node)


-- | Match any character.
anyChar ∷ ∀ t. Expression t Char
anyChar = Expression anyChar'
  where
  anyChar' ∷ Node t → _
  anyChar' node@{ position, string, cache } =
    case String.codePointAt position string of
      Just codepoint →
        case fromCharCode $ fromEnum codepoint of
          Just character →
            Right { result : character
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
