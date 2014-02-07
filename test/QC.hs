{-# LANGUAGE TemplateHaskell #-}
-- Needed for protobuf
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- We do not fear orphans here
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
import Control.Applicative

import Data.Protobuf.TH
import Data.Protobuf.API

import Data.Typeable
import Data.Serialize
import qualified Data.Sequence      as Seq
import qualified Data.Vector.HFixed as H
import Data.Vector.HFixed.HVec (HVec)
import Data.Text (pack)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck


import Data.Int


----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "tests"
  [
    --testTagged p_serialize (T :: T (Message "Name.Simple"))
    testTagged p_serialize (T :: T (Message "Name.Simple.Nested"))
  , testTagged p_serialize (T :: T (Message "Name.Seq"))
  , testTagged p_serialize (T :: T (Message "Name.Seqp"))
  ]

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------
-- Properties
----------------------------------------------------------------

data T a = T

p_serialize :: (Protobuf a, Arbitrary a, Eq a) => T a -> a -> Bool
p_serialize _ a
  = Right a == (runGet getMessage . runPut . serialize) a

testTagged :: forall a b. (Testable b, Typeable a) => (T a -> b) -> T a -> TestTree
testTagged prop t
  = testProperty (show $ typeOf (undefined :: a)) (prop t)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

$(generateProtobuf [] [] ["test/test.proto"])

instance Arbitrary (Name_Seq) where
  arbitrary = H.mk1 <$> arbitrarySeq
instance Arbitrary (Name_Seqp) where
  arbitrary = H.mk1 <$> arbitrarySeq
instance Arbitrary (Name_Simple) where
  arbitrary = H.mk3 <$> arbitrary <*> arbitrary <*> arbitrarySeq
instance Arbitrary (Name_Simple_Nested) where
  arbitrary = H.mk1 . pack <$> arbitrary

arbitrarySeq :: Arbitrary a => Gen (Seq.Seq a)
arbitrarySeq = Seq.fromList <$> arbitrary
