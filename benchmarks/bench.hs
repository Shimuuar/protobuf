{-# LANGUAGE TemplateHaskell #-}
-- Needed for protobuf
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- We do not fear orphans here
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns #-}
import Data.Protobuf.TH
import Data.Protobuf.API

import Data.Typeable
import Data.Serialize
import qualified Data.Sequence      as Seq
import qualified Data.Vector.HFixed as H
import Data.Vector.HFixed.HVec (HVec)

import Criterion.Main



----------------------------------------------------------------
-- Benchmarks
----------------------------------------------------------------

-- Data sample with different length
seq_1,seq_3,seq_10,seq_30,seq_100 :: Message "Name.Seq"
seq_1   = H.mk1 $ Seq.fromList [1     ]
seq_3   = H.mk1 $ Seq.fromList [1..3  ]
seq_10  = H.mk1 $ Seq.fromList [1..10 ]
seq_30  = H.mk1 $ Seq.fromList [1..30 ]
seq_100 = H.mk1 $ Seq.fromList [1..100]
-- Packed sequences with different length
pseq_1,pseq_3,pseq_10,pseq_30,pseq_100 :: Message "Name.Seqp"
pseq_1   = H.mk1 $ Seq.fromList [1     ]
pseq_3   = H.mk1 $ Seq.fromList [1..3  ]
pseq_10  = H.mk1 $ Seq.fromList [1..10 ]
pseq_30  = H.mk1 $ Seq.fromList [1..30 ]
pseq_100 = H.mk1 $ Seq.fromList [1..100]


benchDecode :: forall msg. Protobuf msg => String -> msg -> Benchmark
{-# INLINE benchDecode #-}
benchDecode name a
  = bench name $ whnf run bs
  where
    bs    = runPut $ serialize a
    run b = case runGet getMessage b of
              Right !x -> (x :: msg)
              Left  _  -> error "Impossible"

main :: IO ()
main = do
  defaultMain $
    [ bgroup "sequence"
      [ benchDecode "1"   seq_1
      , benchDecode "3"   seq_3
      , benchDecode "10"  seq_10
      , benchDecode "30"  seq_30
      , benchDecode "100" seq_100
      ]
    , bgroup "packed_seq"
      [ benchDecode "1"   pseq_1
      , benchDecode "3"   pseq_3
      , benchDecode "10"  pseq_10
      , benchDecode "30"  pseq_30
      , benchDecode "100" pseq_100
      ] 
    ]


----------------------------------------------------------------
$(generateProtobuf [] [] ["test/test.proto"])