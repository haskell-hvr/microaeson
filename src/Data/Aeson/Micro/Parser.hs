module Data.Aeson.Micro.Parser where

import           Control.Exception     as E
import           Control.Monad
import           Data.Char
import           Data.Word
import qualified GHC.Foreign           as GHC
import           GHC.IO.Encoding
import           System.IO.Unsafe
import           Text.Read             (readMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.Char8

decodeEscapedHex :: BS.ByteString -> Maybe Char
decodeEscapedHex bs = do
  [0x5c,0x75,d1,d2,d3,d4] <- pure (BS.unpack bs)

  let cp = (0x10*((0x10*((0x10 * h2n d1) + h2n d2)) + h2n d3)) + h2n d4

  guard (not (0xd800 <= cp && cp <= 0xdfff))

  pure (chr cp)

decodeEscapedHexSurr :: BS.ByteString -> Maybe Char
decodeEscapedHexSurr bs = do
  [0x5c,0x75,h1,h2,h3,h4,0x5c,0x75,l1,l2,l3,l4] <- pure (BS.unpack bs)

  let hsurr = (0x10*((0x10*((0x10 * h2n h1) + h2n h2)) + h2n h3)) + h2n h4
      lsurr = (0x10*((0x10*((0x10 * h2n l1) + h2n l2)) + h2n l3)) + h2n l4

  guard ((0xd800 <= hsurr && hsurr <= 0xdbff) && (0xdc00 <= lsurr && lsurr <= 0xdfff))

  let cp = 0x10000 + ((hsurr-0xd800)*0x400) + (lsurr-0xdc00)

  pure (chr cp)

decodeNumber :: BS.ByteString -> Maybe Double
decodeNumber = readMaybe . BS.Char8.unpack

h2n :: Word8 -> Int
h2n w
  | 0x30 <= w && w <= 0x39  = fromIntegral (w-0x30)
  | 0x41 <= w && w <= 0x46  = fromIntegral (w-0x37)
  | 0x61 <= w && w <= 0x66  = fromIntegral (w-0x57)
  | otherwise               = undefined

decodeEscaped :: BS.ByteString -> Maybe Char
decodeEscaped bs = do
  [0x5c,c] <- pure (BS.unpack bs)
  case c of
    0x22 -> pure '\x22'
    0x5c -> pure '\x5c'
    0x2f -> pure '\x2f'
    0x62 -> pure '\x08'
    0x66 -> pure '\x0c'
    0x6e -> pure '\x0a'
    0x72 -> pure '\x0d'
    0x74 -> pure '\x09'
    _    -> Nothing

decodeUnescaped :: BS.ByteString -> Maybe String
decodeUnescaped = decodeString utf8

decodeString :: TextEncoding -> BS.ByteString -> Maybe String
decodeString te bs = unsafePerformIO (decodeStringIO te bs)

{-# NOINLINE decodeStringIO #-}
decodeStringIO :: TextEncoding -> BS.ByteString -> IO (Maybe String)
decodeStringIO te bs = cvtEx <$> try (BS.useAsCStringLen bs (GHC.peekCStringLen te))
  where
    cvtEx :: Either IOException a -> Maybe a
    cvtEx = either (const Nothing) Just

