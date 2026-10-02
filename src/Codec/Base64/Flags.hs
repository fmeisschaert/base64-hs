{-|
Module      : Codec.Base64.Flags
Description : Options controlling Base64 encoding and decoding
Copyright   : (c) Frank Meisschaert, 2026
License     : BSD-3-Clause
Maintainer  : fmeisschaert@gmail.com
Stability   : experimental
Portability : portable

Options that select the Base64 variant and how strictly input is
decoded. The same 'Flags' type is used by both
'Codec.Base64.Encode.encode' and 'Codec.Base64.Decode.decode'. A flag that
doesn't apply to an operation is ignored.

Some common combinations:

[@[Padding]@] Standard Base64 as specified in
  <https://www.rfc-editor.org/rfc/rfc4648#section-4 RFC 4648, section 4>.

[@[Url]@] Unpadded URL- and filename-safe Base64, as used by JWT
  and many web APIs
  (<https://www.rfc-editor.org/rfc/rfc4648#section-5 RFC 4648, section 5>).

[@[Url, Padding]@] Padded URL- and filename-safe Base64.

[@[Lenient, Padding]@] Decodes standard Base64 that may contain line
  breaks or other whitespace, such as MIME bodies or PEM files.
-}
module Codec.Base64.Flags
where

-- | A single option for encoding or decoding.
data Flag
    = Padding
      -- ^ __Encoding:__ pad the output with @=@ so that its length is a
      -- multiple of 4.
      --
      -- __Decoding:__ consume the @=@ padding characters that follow a
      -- final incomplete group. Without this flag, they are left in
      -- 'Codec.Base64.Decode.decodeRest'. Padding is accepted but never
      -- required.
    | Url
      -- ^ Use the URL- and filename-safe alphabet, which has @-@ and @_@
      -- for values 62 and 63, instead of the standard @+@ and @/@.
      -- Applies to both encoding and decoding.
    | Lenient
      -- ^ __Decoding only:__ skip characters that aren't in the alphabet,
      -- such as whitespace and line breaks, instead of stopping at them.
      -- Decoding still stops at the first @=@, unless 'IgnorePadChar' is
      -- also set.
    | IgnorePadChar
      -- ^ __Decoding only, together with 'Lenient':__ treat @=@ like
      -- any other non-alphabet character and skip it, so that decoding
      -- continues to the end of the input.
      --
      -- Because padding is discarded, groups are no longer delimited.
      -- Use this only when @=@ can appear only at the very end of the
      -- input. Concatenated padded encodings won't decode correctly.
  deriving (Eq,Show)

-- | A set of options, given as a list. Order and duplicates don't
-- matter, and @[]@ selects unpadded, strict, standard-alphabet Base64.
type Flags = [Flag]
