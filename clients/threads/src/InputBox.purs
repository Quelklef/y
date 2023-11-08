module InputBox
  ( InputBox
  , content
  , default
  , height
  , prevContent
  , reset
  , selectionStart
  , selectionEnd
  , setContent
  , setContentAndCursor
  , setHeight
  , setSelectionRange
  , setCursor
  , undo
  )
  where

import MasonPrelude

import Data.String as String

newtype InputBox =
  InputBox
    { content :: String
    , prevContent :: String
    , height :: Number
    , selectionRange :: Int /\ Int
    }

height :: InputBox -> Number
height (InputBox r) = r.height

content :: InputBox -> String
content (InputBox r) = r.content

defaultHeight :: Number
defaultHeight = 30.0

default :: InputBox
default =
  InputBox
    { content: ""
    , prevContent: ""
    , height: defaultHeight
    , selectionRange: 0 /\ 0
    }

-- | store current content in prevContent
reset :: InputBox -> InputBox
reset (InputBox r) =
  InputBox
    { content: ""
    , prevContent: r.content
    , height: defaultHeight
    , selectionRange: 0 /\ 0
    }

setContent :: String -> InputBox -> InputBox
setContent newContent (InputBox r) =
  InputBox
  $ r { content = newContent
      , prevContent = r.content
      }

setHeight :: Number -> InputBox -> InputBox
setHeight newHeight (InputBox r) = InputBox $ r { height = newHeight }

undo :: InputBox -> InputBox
undo (InputBox r) =
  InputBox
  $ r { content = r.prevContent
      , prevContent = r.content
      }

prevContent :: InputBox -> String
prevContent (InputBox r) = r.prevContent

selectionStart :: InputBox -> Int
selectionStart (InputBox r) = fst r.selectionRange

selectionEnd :: InputBox -> Int
selectionEnd (InputBox r) = snd r.selectionRange

setSelectionRange :: Int /\ Int -> InputBox -> InputBox
setSelectionRange range (InputBox r) = InputBox $ r { selectionRange = range }

setCursor :: Int -> InputBox -> InputBox
setCursor = setSelectionRange <. join Tuple

setContentAndCursor :: String -> InputBox -> InputBox
setContentAndCursor str =
  setContent str
  .> setCursor (String.length str)
