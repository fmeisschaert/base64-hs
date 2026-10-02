{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Codec.Base64.Decode
Description : Base64 decoding
Copyright   : (c) Frank Meisschaert, 2026
License     : BSD-3-Clause
Maintainer  : fmeisschaert@gmail.com
Stability   : experimental
Portability : portable

Decodes Base64 text into bytes, as described in
<https://www.rfc-editor.org/rfc/rfc4648 RFC 4648>.

Decoding never fails. It decodes as much of the input as it can and stops
at the first character it can't use. 'decode' returns only the bytes.
'decodeResult' also returns the input that wasn't consumed, so callers can
detect malformed input or continue with whatever follows the encoded data.

= Decoding modes

By default, decoding is /strict/. It stops at the first character that
isn't in the selected alphabet, including whitespace and @=@. If that
character interrupts a group, the bytes completed so far are still
returned.

With 'Lenient', non-alphabet characters are skipped and decoding stops only
at @=@ or at the end of the input. With 'Lenient' and 'IgnorePadChar',
decoding always continues to the end of the input.

In both modes, 'Padding' makes the decoder consume the @=@ characters that
follow a final incomplete group, so they don't appear in 'decodeRest'.

= Limitations

* Leftover bits in a final incomplete group are discarded without being
  checked. Non-canonical encodings such as @\"TR==\"@ decode the same as
  their canonical form, @\"TQ==\"@.

* The input is consumed up to the point where decoding stops before any
  output is returned, so 'decode' isn't suitable for incremental decoding
  of infinite streams.
-}
module Codec.Base64.Decode (
    -- * Decoding
    decode,
    decodeResult,
    -- * Decoding results
    DecodeResult(..)
) where

import Data.Bits
import Data.Char
import Data.Word

import Codec.Base64.Flags

char2integral :: (Integral a) => Char -> a
char2integral = fromIntegral . ord

-- Map a character to its 6-bit value, given the characters for 62 and 63.
-- Characters outside the alphabet are returned in 'Left'.
fromBase64 :: Char -> Char -> Char -> Either Char Word
fromBase64 c62 c63 c | c >= 'A' && c <= 'Z' = Right $ char2integral c - 65
                     | c >= 'a' && c <= 'z' = Right $ char2integral c - 71
                     | c >= '0' && c <= '9' = Right $ char2integral c + 4
                     | c == c62             = Right $ 62
                     | c == c63             = Right $ 63
                     | otherwise            = Left c

-- | The full result of decoding: the decoded value, the input that wasn't
-- consumed, and the decoder's state when it stopped.
--
-- The input was well formed and fully consumed if 'decodeRest' is empty
-- and 'decodeState' is @(0,0)@.
data DecodeResult a = DecodeResult
    { decodeState :: (Int,Word)
      -- ^ Characters decoded but not yet turned into bytes: their count
      -- (0 to 3) and their bits.
      --
      -- This is always @(0,0)@, except when the input ends with a single
      -- character that can't form a byte, such as an input whose length
      -- leaves a remainder of 1 when divided by 4. In that case, the count
      -- is 1, and 'decodeRest' starts at that character.
    , decodeRest :: String
      -- ^ The input that wasn't consumed, starting at the first character
      -- that couldn't be used. This is empty if all of the input was
      -- decoded.
    , decoded :: a
      -- ^ The decoded value.
    }
  deriving (Show)

-- | Applies a function to 'decoded', leaving the other fields unchanged.
instance Functor DecodeResult where
    fmap f (DecodeResult state rest x) = DecodeResult state rest (f x)

-- | Decode Base64 text, returning the bytes together with the input that
-- wasn't consumed and the final decoder state.
--
-- Use this function when you need to tell whether the whole input was
-- valid, or when the Base64 data is followed by other content.
--
-- A complete, padded input is fully consumed:
--
-- >>> decodeResult [Padding] "TWE="
-- DecodeResult {decodeState = (0,0), decodeRest = "", decoded = [77,97]}
--
-- Without 'Padding', the @=@ is left in the rest:
--
-- >>> decodeResult [] "TWE="
-- DecodeResult {decodeState = (0,0), decodeRest = "=", decoded = [77,97]}
--
-- Strict decoding stops at the end of the first padded value, so
-- concatenated values can be decoded one at a time:
--
-- >>> decodeResult [Padding] "TQ==TWE="
-- DecodeResult {decodeState = (0,0), decodeRest = "TWE=", decoded = [77]}
--
-- Strict decoding stops at whitespace:
--
-- >>> decodeResult [Padding] "TW Fu"
-- DecodeResult {decodeState = (0,0), decodeRest = " Fu", decoded = [77]}
--
-- Lenient decoding skips whitespace:
--
-- >>> decodeResult [Lenient, Padding] "TW Fu\nTQ=="
-- DecodeResult {decodeState = (0,0), decodeRest = "", decoded = [77,97,110,77]}
--
-- A single trailing character can't form a byte:
--
-- >>> decodeResult [Padding] "TWFuT"
-- DecodeResult {decodeState = (1,19), decodeRest = "T", decoded = [77,97,110]}
decodeResult :: Flags -> String -> DecodeResult [Word8]
decodeResult flags s =
    let fset = (`elem` flags)
        nset = not . fset

        (c62,c63) = if fset Url then ('-','_') else ('+','/')
        from64 = fromBase64 c62 c63

        word8 :: Word -> Word8
        word8 = fromIntegral

        skipPadding :: Int -> String -> String
        skipPadding _ cs | nset Padding = cs
        skipPadding n cs | n <= 0       = cs
        skipPadding n ('=':cs)          = skipPadding (n-1) cs
        skipPadding _ cs                = cs

        goEnd :: String -> (Int,Word) -> String -> DecodeResult [Word8]
        goEnd rest state@(n,buf) cs = case n of
            0 -> DecodeResult state rest []
            1 -> DecodeResult state rest []      -- 6 bits in buffer
            2 -> let b0 = word8 $ buf .>>. 4     -- 12 bits in buffer
                     cs' = skipPadding 2 cs
                 in DecodeResult (0,0) cs' [b0]
            3 -> let b0 = word8 $ buf .>>. 10    -- 18 bits in buffer
                     b1 = word8 $ buf .>>. 2
                     cs' = skipPadding 1 cs
                 in DecodeResult (0,0) cs' [b0,b1]
            _ -> error "goEnd: unreachable code"

        ---
        goStrict :: String -> (Int,Word) -> String -> DecodeResult [Word8]

        goStrict _ (4,buf) cs =
            let b0 = word8 $ buf .>>. 16     -- 24 bits in buffer
                b1 = word8 $ buf .>>. 8
                b2 = word8 $ buf
            in (b0:) . (b1:) . (b2:) <$> goStrict cs (0,0) cs

        goStrict rest state@(n,buf) cs@(c:cs') = case from64 c of
            Left _  -> goEnd rest state cs
            Right w -> goStrict rest (n+1,buf .<<. 6 .|. w) cs'

        goStrict rest state [] = goEnd rest state []

        ---
        goLenient :: String -> (Int,Word) -> String -> DecodeResult [Word8]
        goLenient _ (4,buf) cs =
            let b0 = word8 $ buf .>>. 16     -- 24 bits in buffer
                b1 = word8 $ buf .>>. 8
                b2 = word8 $ buf
            in (b0:) . (b1:) . (b2:) <$> goLenient cs (0,0) cs

        goLenient rest state@(n,buf) cs@(c:cs') = case from64 c of
            Left '=' | nset IgnorePadChar -> goEnd rest state cs
            Left _   -> goLenient rest state cs'
            Right w  -> goLenient rest (n+1,buf .<<. 6 .|. w) cs'

        goLenient rest state [] = goEnd rest state []

        --
        go = if fset Lenient then goLenient else goStrict

    in go s (0,0) s

-- | Decode Base64 text into bytes. Any input after the point where
-- decoding stopped is silently ignored.
--
-- @decode flags = 'decoded' . 'decodeResult' flags@
--
-- To detect malformed or trailing input, use 'decodeResult'.
--
-- >>> decode [Padding] "TWFu"
-- [77,97,110]
--
-- >>> decode [Url] "-_8"
-- [251,255]
--
-- With the standard alphabet, @-@ isn't valid, so nothing is decoded:
--
-- >>> decode [] "-_8"
-- []
decode :: Flags -> String -> [Word8]
decode flags s = decoded $ decodeResult flags s
