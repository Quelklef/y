module Y.Client.Colors (make) where

import Prelude

import Debug as Debug

import Data.Int (pow, toNumber)
import Data.String.CodeUnits (toCharArray)
import Data.Char (toCharCode)
import Data.Foldable (sum)
import Math (remainder)

-- @make seed@ is a sequence of CSS color values each of which is
-- reasonably far apart from all of the previous.
--
-- We use this to generate the colors that we associate with users.
--
-- We don't want the color sequence to be the same for all
-- conversations, as that would be boring. The role of the @seed@
-- parameter is to counteract this. The @seed@ gets turned into
-- a hue shift which is applied to the entire sequence. (This is
-- done in a chaotic manner, so the actual values of the seeds
-- doesn't matter, only that they differ from sequence to sequence.)
make :: String -> Int -> String
make seed index =
  let hueShift = stringToPercent seed
      hue = (nthHue index + hueShift) * 360.0
  in
    "hsl(" <> show hue <> ", 100%, 50%)"

-- | Takes a string and chaotically returns a number in [0, 1]
stringToPercent :: String -> Number
stringToPercent = sha1
              >>> toCharArray
              >>> map (\char -> if toCharCode char >= toCharCode 'a'
                                then toCharCode char - toCharCode 'a'
                                else toCharCode char - toCharCode '0' + 26)
              >>> sum
              >>> toNumber
              >>> (_ / 36.0)
              >>> (_ `remainder` 1.0)

foreign import sha1 :: String -> String

nthHue :: Int -> Number
nthHue = case _ of
    0 -> 0.0
    n -> dist n

  where

  -- | Yields all reciprocal powers of two in the following order:
  -- |
  -- |                             1/2,
  -- |                     1/4,            3/4,
  -- |                 1/8,    3/8,    5/8,    7/8,
  -- |
  -- | ...continuing infinitely. (Read left-to-right, top-to-bottom)
  -- |
  -- | This function is 1-indexed, not 0-indexed.
  dist = case _ of
    1 -> 0.5
    n ->
      let anchor = dist (n `div` 2)
          sign = (n `mod` 2) * 2 - 1
          rbump = pow 2 $ (floorLog2 n) + 1
      in
         anchor + toNumber sign / toNumber rbump

  floorLog2 = case _ of
    1 -> 0
    x -> 1 + floorLog2 (x `div` 2)
