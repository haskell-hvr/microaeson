{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Aeson.KeyMap     as KeyMap
import           Data.Aeson.Key        (fromText, toText)
import qualified Data.Map.Strict       as Map
import qualified Data.Vector           as V

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()

import qualified Data.Aeson            as REF
import qualified Data.Aeson.Micro      as IUT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]
  where
    qcProps = testGroup "QC"
      [ QC.testProperty "roundtrip(ref/ref)" $
          \x -> (fmap fromAeson . REF.decode . REF.encode . toAeson) x == Just x

      , QC.testProperty "roundtrip(ref/iut)" $
          \x -> (fmap fromAeson . REF.decode . IUT.encode) x == Just x

      , QC.testProperty "roundtrip(iut/ref)" $
          \x -> (IUT.decode . REF.encode . toAeson) x == Just x

      , QC.testProperty "roundtrip(iut/iut)" $
          \x -> (IUT.decodeStrict . IUT.encodeStrict) x == Just (x :: IUT.Value)
      ]


instance Arbitrary IUT.Value where
  arbitrary = sized value
    where
      value 0 = oneof [ IUT.String <$> arbitrary
                      , IUT.Number <$> arbitrary
                      , IUT.Bool   <$> arbitrary
                      , pure IUT.Null
                      ]
      value n | n>0
        = oneof [ IUT.String <$> arbitrary
                , IUT.Number <$> arbitrary
                , IUT.Bool   <$> arbitrary
                , pure IUT.Null
                , IUT.Array  <$> resize ((10*n) `div` 14) arbitrary
                , IUT.Object <$> resize ((10*n) `div` 14) arbitrary
                ]
              | otherwise = pure IUT.Null


toAeson :: IUT.Value -> REF.Value
toAeson j = case j of
  IUT.String t -> REF.String t
  IUT.Number n -> REF.toJSON n
  IUT.Bool b   -> REF.Bool b
  IUT.Null     -> REF.Null
  IUT.Array l  -> REF.Array (V.fromList (map toAeson l))
  IUT.Object m -> REF.object [ (fromText k, toAeson v) | (k,v) <- Map.toList m ]

fromAeson :: REF.Value -> IUT.Value
fromAeson j = case j of
  REF.String t -> IUT.String t
  REF.Bool b   -> IUT.Bool b
  REF.Null     -> IUT.Null
  REF.Number s -> IUT.Number (realToFrac s)
  REF.Array v  -> IUT.Array (map fromAeson (V.toList v))
  REF.Object m -> IUT.object [ (toText k, fromAeson v) | (k,v) <- KeyMap.toList m ]
