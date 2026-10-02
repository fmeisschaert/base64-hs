{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Codec.Base64.Encode
Description : Base64 encoding
Copyright   : (c) Frank Meisschaert, 2026
License     : BSD-3-Clause
Maintainer  : fmeisschaert@gmail.com
Stability   : experimental
Portability : portable

Encodes a sequence of bytes as Base64 text, as described in
<https://www.rfc-editor.org/rfc/rfc4648 RFC 4648>.
-}
module Codec.Base64.Encode (
    encode
) where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Word

import Codec.Base64.Flags

integral2char :: (Integral a) => a -> Char
integral2char = chr . fromIntegral

-- Map a 6-bit value to its character in the alphabet, given the characters
-- for 62 and 63.
toBase64 :: Char -> Char -> Word -> Maybe Char
toBase64 c62 c63 w | w < 26    = Just $ integral2char $ w + 65
                   | w < 52    = Just $ integral2char $ w + 71
                   | w < 62    = Just $ integral2char $ w - 4
                   | w == 62   = Just $ c62
                   | w == 63   = Just $ c63
                   | otherwise = Nothing

-- | Encode a list of bytes as Base64.
--
-- Each group of 3 input bytes becomes 4 output characters. If the last
-- group has 1 or 2 bytes, it becomes 2 or 3 characters, followed by @==@
-- or @=@ when 'Padding' is set.
--
-- Only 'Padding' and 'Url' affect encoding. 'Lenient' and 'IgnorePadChar'
-- are ignored.
--
-- The output is produced lazily, so long or streamed input can be
-- encoded incrementally.
--
-- >>> encode [Padding] [77,97,110]    -- "Man"
-- "TWFu"
--
-- >>> encode [Padding] [77]           -- "M"
-- "TQ=="
--
-- >>> encode [] [77]
-- "TQ"
--
-- >>> encode [] [251,255]
-- "+/8"
--
-- >>> encode [Url] [251,255]
-- "-_8"
encode :: Flags -> [Word8] -> String
encode flags =
    let fset = (`elem` flags)

        (c62,c63) = if fset Url then ('-','_') else ('+','/')

        to64 = fromJust . toBase64 c62 c63
        word :: Word8 -> Word
        word = fromIntegral

        go (b0:b1:b2:bs) =
            let w =  word b0 .<<. 16
                 .|. word b1 .<<. 8
                 .|. word b2
                c0 = to64 $ w .>>. 18 .&. 0x3f
                c1 = to64 $ w .>>. 12 .&. 0x3f
                c2 = to64 $ w .>>.  6 .&. 0x3f
                c3 = to64 $ w         .&. 0x3f
            in c0 : c1 : c2 : c3 : go bs
        go [b0,b1] =
            let e = take 3 $ go [b0,b1,0]
            in if fset Padding then e ++ "=" else e
        go [b0] =
            let e = take 2 $ go [b0,0,0]
            in if fset Padding then e ++ "==" else e
        go [] = []
    in go
