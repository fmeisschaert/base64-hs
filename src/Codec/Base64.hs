{-|
Module      : Codec.Base64
Description : Base64 encoding and decoding in pure Haskell
Copyright   : (c) Frank Meisschaert, 2026
License     : BSD-3-Clause
Maintainer  : fmeisschaert@gmail.com
Stability   : experimental
Portability : portable

Base64 encoding and decoding as described in
<https://www.rfc-editor.org/rfc/rfc4648 RFC 4648>. Binary data is
represented as @['Data.Word.Word8']@ and Base64 text as 'String'.

This module re-exports "Codec.Base64.Flags", "Codec.Base64.Encode" and
"Codec.Base64.Decode", so importing it is usually enough.

= Quick start

>>> encode [Padding] [72,101,108,108,111]    -- "Hello"
"SGVsbG8="

>>> decode [Padding] "SGVsbG8="
[72,101,108,108,111]

= Variants

Every function takes a list of 'Flag's that selects the variant:

* @[Padding]@ selects standard Base64 with the @+@ and @/@ alphabet and
  @=@ padding.
* @[Url]@ selects the URL- and filename-safe alphabet with @-@ and @_@,
  without padding. Add 'Padding' to pad the output.
* Add 'Lenient' when decoding text that may contain whitespace or line
  breaks.

For details, see 'Flag'.

= Error handling

Decoding never throws an exception. It stops at the first character it
can't use. 'decode' discards the rest of the input, while 'decodeResult'
returns it so you can check that the whole input was consumed:

>>> decodeRest (decodeResult [Padding] "SGVsbG8=!")
"!"
-}
module Codec.Base64 (
    -- * Flags
    Flag(..),
    Flags,
    -- * Encoding
    encode,
    -- * Decoding
    decode,
    decodeResult,
    DecodeResult(..)
) where

import Codec.Base64.Flags
import Codec.Base64.Encode
import Codec.Base64.Decode
