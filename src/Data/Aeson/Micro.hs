{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Minimal JSON / RFC 7159 support
--
-- This API provides a subset (with a couple of divergences; see below) of
-- [aeson](hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
-- API but puts the emphasis on simplicity rather than performance and features.
--
-- The 'ToJSON'/'FromJSON' instances are intended to have an encoding
-- compatible with @aeson@'s encoding.
--
-- == Limitations and divergences from @aeson@'s API
--
-- In order to reduce the dependency footprint and keep the code
-- simpler, the following divergences from the @aeson@ API have to be
-- made:
--
-- * There are no `FromJSON`/`ToJSON` instances for `Char` & `String`.
-- * The type synonym (& the constructor of the same name) 'Object' uses @containers@'s 'Map.Map' rather than a 'HashMap' @unordered-containers@.
-- * 'Array' is represented by an ordinary list rather than a 'Vector' from the @vector@ package.
-- * 'Number' uses 'Double' instead of 'Scientific'
--
module Data.Aeson.Micro
    ( -- * Core JSON types
      Value(..)
    , Object
    , Pair

      -- ** Constructors
    , (.=)
    , object
    , emptyArray
    , emptyObject

      -- ** Accessors
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)

      -- * Encoding and decoding
    , encode
    , encodeStrict
    , encodeToBuilder

    , decodeStrict
    , decode

      -- * Prism-style parsers
    , withObject
    , withText
    , withArray
    , withNumber
    , withBool

      -- * Type conversion
    , FromJSON(parseJSON)
    , Parser
    , ToJSON(toJSON)

    ) where

import           Control.Monad
import           Data.Char
import           Data.Data                (Data)
import           Data.Int
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Typeable            (Typeable)
import           Data.Word
import           GHC.Generics             (Generic)

import           Control.DeepSeq
import qualified Data.ByteString          as BS
import           Data.ByteString.Builder  (Builder)
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as BS.Lazy
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL

import           Data.Aeson.Micro.Parser
import           Data.Aeson.Micro.Scanner (Lexeme (..), scanLexemes)

-- TODO: We may want to replace 'String' with 'Text' or 'ByteString'

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array  [Value]
           | String !Text
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Eq, Read, Show, Generic, Data, Typeable)

instance NFData Value

instance IsString Value where
  fromString = String . fromString

-- | A key\/value pair for an 'Object'
type Pair = (Text, Value)

-- | A JSON \"object\" (key/value map).
type Object = Map.Map Text Value

infixr 8 .=

-- | A key-value pair for encoding a JSON object.
(.=) :: ToJSON v => Text -> v -> Pair
k .= v  = (k, toJSON v)

-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object . Map.fromList

emptyObject :: Value
emptyObject = Object mempty

emptyArray :: Value
emptyArray = Array mempty

----------------------------------------------------------------------------

(.:) :: FromJSON a => Object -> Text -> Parser a
m .: k = maybe (pfail "key not found") parseJSON (Map.lookup k m)

(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
m .:? k = maybe (pure Nothing) parseJSON (Map.lookup k m)

(.:!) :: FromJSON a => Object -> Text -> Parser (Maybe a)
m .:! k = maybe (pure Nothing) (fmap Just . parseJSON) (Map.lookup k m)

(.!=) :: Parser (Maybe a) -> a -> Parser a
mv .!= def = fmap (maybe def id) mv

----------------------------------------------------------------------------

-- | A type that can be converted to JSON.
class ToJSON a where
  -- | Convert a Haskell value to a JSON-friendly intermediate type.
  toJSON :: a -> Value

instance ToJSON () where
  toJSON () = Array []

instance ToJSON Value where
  toJSON = id

instance ToJSON Bool where
  toJSON = Bool

instance ToJSON a => ToJSON [a] where
  toJSON = Array . map toJSON

instance ToJSON v => ToJSON (Map.Map Text v) where
  toJSON = Object . Map.map toJSON

instance ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing  = Null
  toJSON (Just a) = toJSON a

instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array [toJSON a, toJSON b]

instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]

instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]

instance ToJSON Text where
  toJSON = String

instance ToJSON TL.Text where
  toJSON = toJSON . TL.toStrict

instance ToJSON Float where
  toJSON = Number . realToFrac

instance ToJSON Double where
  toJSON = Number

instance ToJSON Int    where  toJSON = Number . realToFrac
instance ToJSON Int8   where  toJSON = Number . realToFrac
instance ToJSON Int16  where  toJSON = Number . realToFrac
instance ToJSON Int32  where  toJSON = Number . realToFrac

instance ToJSON Word   where  toJSON = Number . realToFrac
instance ToJSON Word8  where  toJSON = Number . realToFrac
instance ToJSON Word16 where  toJSON = Number . realToFrac
instance ToJSON Word32 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64  where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger

------------------------------------------------------------------------------
-- 'BB.Builder'-based encoding

encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict = BS.Lazy.toStrict . encode

encode :: ToJSON a => a -> BS.Lazy.ByteString
encode = BB.toLazyByteString . encodeToBuilder

-- | Serialise value as JSON/UTF8-encoded 'Builder'
encodeToBuilder :: ToJSON a => a -> Builder
encodeToBuilder = encodeValueBB . toJSON

encodeValueBB :: Value -> Builder
encodeValueBB jv = case jv of
  Bool True  -> "true"
  Bool False -> "false"
  Null       -> "null"
  Number n
    | isNaN n || isInfinite n   -> encodeValueBB Null
    | Just i <- doubleToInt64 n -> BB.int64Dec i
    | otherwise                 -> BB.doubleDec n
  Array a  -> encodeArrayBB a
  String s -> encodeStringBB s
  Object o -> encodeObjectBB o

encodeArrayBB :: [Value] -> Builder
encodeArrayBB [] = "[]"
encodeArrayBB jvs = BB.char8 '[' <> go jvs <> BB.char8 ']'
  where
    go = Data.Monoid.mconcat . intersperse (BB.char8 ',') . map encodeValueBB

encodeObjectBB :: Object -> Builder
encodeObjectBB m
  | Map.null m  = "{}"
  | otherwise = BB.char8 '{' <> go jvs <> BB.char8 '}'
  where
    jvs = Map.toList m
    go = Data.Monoid.mconcat . intersperse (BB.char8 ',') . map encPair
    encPair (l,x) = encodeStringBB l <> BB.char8 ':' <> encodeValueBB x

encodeStringBB :: Text -> Builder
encodeStringBB str = BB.char8 '"' <> go str <> BB.char8 '"'
  where
    go = T.encodeUtf8Builder . escapeText

------------------------------------------------------------------------------
-- helpers

-- | Try to convert 'Double' into 'Int64', return 'Nothing' if not
-- representable loss-free as integral 'Int64' value.
doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64)
    = Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x

-- | Minimally escape a 'String' in accordance with RFC 7159, "7. Strings"
escapeText :: Text -> Text
escapeText s
  | not (T.any needsEscape s) = s
  | otherwise                 = (T.pack . escape . T.unpack) s
  where
    escape [] = []
    escape (x:xs) = case x of
      '\\' -> '\\':'\\':escape xs
      '"'  -> '\\':'"':escape xs
      '\b' -> '\\':'b':escape xs
      '\f' -> '\\':'f':escape xs
      '\n' -> '\\':'n':escape xs
      '\r' -> '\\':'r':escape xs
      '\t' -> '\\':'t':escape xs
      c | ord c < 0x10 -> '\\':'u':'0':'0':'0':intToDigit (ord c):escape xs
        | ord c < 0x20 -> '\\':'u':'0':'0':'1':intToDigit (ord c - 0x10):escape xs
        | otherwise    -> c : escape xs

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\','"']

----------------------------------------------------------------------------
----------------------------------------------------------------------------

newtype Parser a = P { unP :: Maybe a }
                 deriving (Functor,Applicative,Monad)

pfail :: String -> Parser a
pfail _ = P Nothing

class FromJSON a where
  parseJSON :: Value -> Parser a

instance FromJSON Value where
  parseJSON = pure

instance FromJSON Bool where
  parseJSON = withBool "Bool" pure

instance FromJSON Text where
  parseJSON = withText "Text" pure

instance FromJSON TL.Text where
  parseJSON = withText "Text" (pure . TL.fromStrict)

instance FromJSON a => FromJSON [a] where
  parseJSON = withArray "[a]" (mapM parseJSON)

instance FromJSON Double where
  parseJSON = withNumber "Double" pure

instance FromJSON Float where
  parseJSON = withNumber "Float" (pure . realToFrac)

-- FIXME: lossy conversions

instance FromJSON Integer where
  parseJSON = withNumber "Int" (pure . round)

instance FromJSON Int where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Int8 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Int16 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Int32 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Int64 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Word where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Word8 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Word16 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Word32 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)

instance FromJSON Word64 where
  parseJSON = withNumber "Int" (pure . fromInteger . round)


instance FromJSON () where
  parseJSON = withArray "()" $ \lst ->
    case lst of
      [] -> pure ()
      _  -> pfail "expected ()"

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
  parseJSON = withArray "(a,b)" $ \lst ->
    case lst of
      [a,b] -> liftM2 (,) (parseJSON a) (parseJSON b)
      _     -> pfail "expected (a,b)"

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
  parseJSON = withArray "(a,b,c)" $ \lst ->
    case lst of
      [a,b,c] -> liftM3 (,,) (parseJSON a) (parseJSON b) (parseJSON c)
      _       -> pfail "expected (a,b,c)"

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a,b,c,d) where
  parseJSON = withArray "(a,b,c,d)" $ \lst ->
    case lst of
      [a,b,c,d] -> liftM4 (,,,) (parseJSON a) (parseJSON b) (parseJSON c) (parseJSON d)
      _         -> pfail "expected (a,b,c,d)"

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON Null = pure Nothing
  parseJSON j    = Just <$> parseJSON j

instance FromJSON Ordering where
  parseJSON = withText "{'LT','EQ','GT'}" $ \s ->
    case s of
      "LT" -> pure LT
      "EQ" -> pure EQ
      "GT" -> pure GT
      _    -> pfail "expected {'LT','EQ','GT'}"

instance FromJSON v => FromJSON (Map.Map Text v) where
  parseJSON = withObject "Map Text v" $ mapM parseJSON

-- "prisms"

withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _        f (Bool arr) = f arr
withBool expected _ v          = typeMismatch expected v

withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v

withArray :: String -> ([Value] -> Parser a) -> Value -> Parser a
withArray _        f (Array lst) = f lst
withArray expected _ v           = typeMismatch expected v

withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _        f (Object obj) = f obj
withObject expected _ v            = typeMismatch expected v

withNumber :: String -> (Double -> Parser a) -> Value -> Parser a
withNumber _        f (Number n) = f n
withNumber expected _ v          = typeMismatch expected v

typeMismatch :: String -> Value -> Parser a
typeMismatch expected _ = pfail ("expected " ++ expected)

----------------------------------------------------------------------------

decode :: FromJSON a => BS.Lazy.ByteString -> Maybe a
decode = decodeStrict . BS.Lazy.toStrict

decodeStrict :: FromJSON a => BS.ByteString -> Maybe a
decodeStrict bs = do
  v <- parseValue bs
  unP (parseJSON v)

type LexStream = [(Lexeme,BS.ByteString)]

parseValue :: BS.ByteString -> Maybe Value
parseValue = goValue0 . scanLexemes
  where
    goValue0 :: LexStream -> Maybe Value
    goValue0 x = snd <$> goValue x

    goValue :: LexStream -> Maybe (LexStream, Value)
    goValue ((L_True,_):xs)     = Just (xs,Bool True)
    goValue ((L_False,_):xs)    = Just (xs,Bool False)
    goValue ((L_Null,_):xs)     = Just (xs,Null)
    goValue ((L_Number,bs):xs)  = (\n->(xs,Number n)) <$> decodeNumber bs
    goValue ((L_StrStart,_):xs) = goString xs
    goValue ((L_ArrStart,_):xs) = goArray xs
    goValue ((L_ObjStart,_):xs) = goObject xs
    goValue _                   = Nothing

    goArray :: LexStream -> Maybe (LexStream, Value)
    goArray xs0 = (Array <$>) <$> go0 xs0
      where
        go0 ((L_ArrEnd,_):xs) = pure (xs, [])
        go0 xs                = do
          (xs', v) <- goValue xs
          go1 [v] xs'

        go1 acc ((L_ArrEnd,_):xs) = pure (xs, reverse acc)
        go1 acc ((L_Comma, _):xs) = do
          (xs', v) <- goValue xs
          go1 (v:acc) xs'
        go1 _ _ = Nothing

    goObject :: LexStream -> Maybe (LexStream, Value)
    goObject xs0 = ((Object . Map.fromList) <$>) <$> go0 xs0
      where
        go0 ((L_ObjEnd,_):xs) = pure (xs, [])
        go0 xs                = do
          ((L_Colon,_):xs', String k) <- goValue xs
          (xs'',v) <- goValue xs'
          go1 [(k,v)] xs''

        go1 acc ((L_ObjEnd,_):xs) = pure (xs, reverse acc)
        go1 acc ((L_Comma, _):xs) = do
          ((L_Colon,_):xs', String k) <- goValue xs
          (xs'',v) <- goValue xs'
          go1 ((k,v):acc) xs''
        go1 _ _ = Nothing

    goString :: LexStream -> Maybe (LexStream, Value)
    goString xs0 = ((String . T.pack) <$>) <$> go [] xs0
      where
        go _   []              = Nothing
        go acc ((lx,chunk):xs) = case lx of
          L_StrEnd -> pure (xs, concat (reverse acc))

          L_StrUnescaped -> do
            s <- decodeUnescaped chunk
            go (s:acc) xs

          L_StrEscaped -> do
            c <- decodeEscaped chunk
            go ([c]:acc) xs

          L_StrEscapedHex -> do
            c <- decodeEscapedHex chunk
            go ([c]:acc) xs

          L_StrEscapedHexSurr -> do
            c <- decodeEscapedHexSurr chunk
            go ([c]:acc) xs

          _ -> Nothing
