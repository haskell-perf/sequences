{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Sequence as S
import Control.Exception (evaluate)

data Conser = forall f. NFData (f Int) => Conser String (Int -> f Int)
data Replicator = forall f. NFData (f Int) => Replicator String (Int -> Int -> f Int)
data Indexing = forall f. NFData (f Int) => Indexing String (f Int) (f Int -> Int -> Int)

main :: IO ()
main = do
  list <- sampleList
  vector <- sampleVector
  uvector <- sampleUVVector
  seq <- sampleSeq
  defaultMain
    [ bgroup
        "Consing"
        (conses
           [ Conser "Data.List" conslist
           , Conser "Data.Vector" consvector
           , Conser "Data.Vector.Unboxed" consuvector
           , Conser "Data.Sequence" consseq
           ])
    , bgroup
        "Replicate"
        (replicators
           [ Replicator "Data.List" L.replicate
           , Replicator "Data.Vector" V.replicate
           , Replicator "Data.Vector.Unboxed" UV.replicate
           , Replicator "Data.Sequence" S.replicate
           ])
    , bgroup "Indexing" (indexes 
                         [ Indexing "Data.List" list (L.!! )
                         , Indexing "Data.Vector" vector (V.!)
                         , Indexing "Data.Vector.Unboxed" uvector (UV.!)
                         , Indexing "Data.Sequence" seq (S.index)
                         ])
    ]
  where
    conses funcs =
      [ bench (title ++ " 0.." ++ show i) $ nf func i
      | i <- [10, 1000, 10000]
      , Conser title func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ " " ++ show i) $ nf (\(x,y) -> func x y) (i,1234)
      | i <- [10, 1000, 10000]
      , Replicator title func <- funcs
      ]
    sampleList :: IO [Int]
    sampleList = evaluate $ force [1..10000]
    
    sampleVector :: IO (V.Vector Int)
    sampleVector = evaluate $ force $ V.generate 10000 id

    sampleUVVector :: IO (UV.Vector Int)
    sampleUVVector = evaluate $ force $ UV.generate 10000 id

    sampleSeq :: IO (S.Seq Int)
    sampleSeq = evaluate $ force $ S.fromList [1..10000]

    indexes funcs = [ bench (title ++ " " ++ show index) $ nf (\x -> func payload x) index
                    | index <- [100, 1000, 9999]
                    , Indexing title payload func <- funcs ]

conslist :: Int -> [Int]
conslist n0 = go n0 []
  where go 0 acc = acc
        go n !acc = go (n - 1) (n : acc)

consvector :: Int -> V.Vector Int
consvector n0 = go n0 V.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (V.cons n acc)

consuvector :: Int -> UV.Vector Int
consuvector n0 = go n0 UV.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (UV.cons n acc)

consseq :: Int -> S.Seq Int
consseq n0 = go n0 S.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (n S.<| acc)
