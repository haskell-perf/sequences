{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.ST
import           Criterion.Main
import           Criterion.Types
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Massiv.Array as M
import qualified Data.RRBVector as RRB
import           System.Directory
import           System.Random

data Conser = forall f. NFData (f Int) => Conser String (Int -> IO (f Int)) (Int -> f Int -> f Int)
data Append = forall f. NFData (f Int) => Append String (Int -> IO (f Int)) (f Int -> f Int -> f Int) (f Int -> f Int)
data Replicator = forall f. NFData (f Int) => Replicator String (Int -> Int -> f Int)
data Indexing = forall f. NFData (f Int) => Indexing String (IO (f Int)) (f Int -> Int -> Int)
data Length = forall f. NFData (f Int) => Length String (Int -> IO (f Int)) (f Int -> Int)
data Min = forall f. NFData (f Int) => Min String (Int -> IO (f Int)) (f Int -> Int)
data Max = forall f. NFData (f Int) => Max String (Int -> IO (f Int)) (f Int -> Int)
data Sort = forall f. NFData (f Int) => Sort String (Int -> IO (f Int)) (f Int -> f Int)
data RemoveElement = forall f. NFData (f Int) => RemoveElement String (IO (f Int)) ((Int -> Bool) -> f Int -> f Int)
data RemoveByIndex = forall f. NFData (f Int) => RemoveByIndex String (IO (f Int)) ((Int -> Int -> Bool) -> f Int -> f Int)

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    [ bgroup
        "Consing"
        (conses
           [ Conser "Data.List" sampleList (:)
           , Conser "Data.Vector" sampleVector V.cons
           , Conser "Data.Vector.Unboxed" sampleUVVector UV.cons
           , Conser "Data.Vector.Storable" sampleSVVector SV.cons
           , Conser "Data.Sequence" sampleSeq (S.<|)
           , Conser "Data.RRBVector" sampleRRB (RRB.<|)
           ])
    , bgroup
        "Indexing"
        (let size = 10005
         in indexes
              [ Indexing "Data.List" (sampleList size) (L.!!)
              , Indexing "Data.Vector" (sampleVector size) (V.!)
              , Indexing "Data.Vector.Unboxed" (sampleUVVector size) (UV.!)
              , Indexing "Data.Vector.Storable" (sampleSVVector size) (SV.!)
              , Indexing "Data.Sequence" (sampleSeq size) S.index
              , Indexing "Data.Massiv.Array" (sampleMassivUArray size) M.index'
              , Indexing "Data.RRBVector" (sampleRRB size) (RRB.!)
              ])
    , bgroup
        "Append"
        (appends
           [ Append "Data.List" sampleList (++) force
           , Append "Data.Vector" sampleVector (V.++) id
           , Append "Data.Vector.Unboxed" sampleUVVector (UV.++) id
           , Append "Data.Vector.Storable" sampleSVVector (SV.++) id
           , Append "Data.Sequence" sampleSeq (S.><) id
           , Append "Data.RRBVector" sampleRRB (RRB.><) id
           ])
    , bgroup
        "Length"
        (lengths
           [ Length "Data.List" sampleList L.length
           , Length "Data.Vector" sampleVector V.length
           , Length "Data.Vector.Unboxed" sampleUVVector UV.length
           , Length "Data.Vector.Storable" sampleSVVector SV.length
           , Length "Data.Sequence" sampleSeq S.length
           , Length "Data.Massiv.Array" sampleMassivUArray M.elemsCount
           , Length "Data.RRBVector" sampleRRB length
           ])
    , bgroup
        "Stable Sort"
        (sorts
           [ Sort "Data.List" randomSampleList L.sort
           , Sort "Data.Vector" randomSampleVector sortVec
           , Sort "Data.Vector.Unboxed" randomSampleUVVector sortUVec
           , Sort "Data.Vector.Storable" randomSampleSVVector sortSVec
           , Sort "Data.Sequence" randomSampleSeq S.sort
           ])
    , bgroup
        "Replicate"
        (replicators
           [ Replicator "Data.List" L.replicate
           , Replicator "Data.Vector" V.replicate
           , Replicator "Data.Vector.Unboxed" UV.replicate
           , Replicator "Data.Vector.Storable" SV.replicate
           , Replicator "Data.Sequence" S.replicate
           , Replicator "Data.RRBVector" RRB.replicate
           ])
    , bgroup
        "Min"
        (mins
           [ Min "Data.List" randomSampleList L.minimum
           , Min "Data.Vector" randomSampleVector V.minimum
           , Min "Data.Vector.Unboxed" randomSampleUVVector UV.minimum
           , Min "Data.Vector.Storable" randomSampleSVVector SV.minimum
           , Min "Data.Sequence" randomSampleSeq minimum
           , Min "Data.Massiv.Array" randomSampleMassivUArray M.minimum'
           , Min "Data.RRBVector" randomSampleRRB minimum
           ])
    , bgroup
        "Max"
        (maxs
           [ Max "Data.List" randomSampleList L.maximum
           , Max "Data.Vector" randomSampleVector V.maximum
           , Max "Data.Vector.Unboxed" randomSampleUVVector UV.maximum
           , Max "Data.Vector.Storable" randomSampleSVVector SV.maximum
           , Max "Data.Sequence" randomSampleSeq maximum
           , Max "Data.Massiv.Array" randomSampleMassivUArray M.maximum'
           , Max "Data.RRBVector" randomSampleRRB maximum
           ])
    , bgroup
        "Filter Element"
        (let size = 10005
         in removeElems
              [ RemoveElement "Data.List" (sampleList size) L.filter
              , RemoveElement "Data.Vector" (sampleVector size) V.filter
              , RemoveElement
                  "Data.Vector.Unboxed"
                  (sampleUVVector size)
                  UV.filter
              , RemoveElement
                  "Data.Vector.Storable"
                  (sampleSVVector size)
                  SV.filter
              , RemoveElement "Data.Sequence" (sampleSeq size) S.filter
              ])
    , bgroup
        "Filter By Index"
        (let size = 10005
         in removeByIndexes
              [ RemoveByIndex "Data.Vector" (sampleVector size) V.ifilter
              , RemoveByIndex
                  "Data.Vector.Unboxed"
                  (sampleUVVector size)
                  UV.ifilter
              , RemoveByIndex
                  "Data.Vector.Storable"
                  (sampleSVVector size)
                  SV.ifilter
              ])
    ]
  where
    appends funcs =
      [ env
        (payload i)
        (\p -> bench (title ++ ":" ++ show i) $ whnf (\x -> forcer (func x x)) p)
      | i <- [10, 100, 1000, 10000]
      , Append title payload func forcer <- funcs
      ]
    conses funcs =
      [ env
        (sample i)
        (\p -> bench (title ++ ":" ++ show i) (whnf (\e -> func e p) 1))
      | i <- [10, 100, 1000, 10000]
      , Conser title sample func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ ":" ++ show i) $ nf (\(x, y) -> func x y) (i, 1234)
      | i <- [10, 100, 1000, 10000]
      , Replicator title func <- funcs
      ]
    indexes funcs =
      [ env
        payload
        (\p -> bench (title ++ ":" ++ show index) $ nf (\x -> func p x) index)
      | index <- [10, 100, 1000, 10000]
      , Indexing title payload func <- funcs
      ]
    lengths funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Length title payload func <- funcs
      ]
    mins funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Min title payload func <- funcs
      ]
    maxs funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Max title payload func <- funcs
      ]
    sorts funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Sort title payload func <- funcs
      ]
    removeElems funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $ nf (\x -> func (/= relem) x) p)
      | relem <- [1, 100, 1000, 10000 :: Int]
      , RemoveElement title payload func <- funcs
      ]
    removeByIndexes funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $
           nf (\x -> func (\index _ -> index /= relem) x) p)
      | relem <- [1, 100, 1000, 10000 :: Int]
      , RemoveByIndex title payload func <- funcs
      ]

sortVec :: V.Vector Int -> V.Vector Int
sortVec vec =
  runST
    (do mv <- V.thaw vec
        V.sort mv
        V.unsafeFreeze mv)

sortUVec :: UV.Vector Int -> UV.Vector Int
sortUVec vec =
  runST
    (do mv <- UV.thaw vec
        V.sort mv
        UV.unsafeFreeze mv)

sortSVec :: SV.Vector Int -> SV.Vector Int
sortSVec vec =
  runST
    (do mv <- SV.thaw vec
        V.sort mv
        SV.unsafeFreeze mv)

randomSampleList :: Int -> IO [Int]
randomSampleList i = evaluate $ force (take i (randoms (mkStdGen 0)))

randomSampleVector :: Int -> IO (V.Vector Int)
randomSampleVector i = evaluate $ force $ V.fromList (take i (randoms (mkStdGen 0)))

randomSampleUVVector :: Int -> IO (UV.Vector Int)
randomSampleUVVector i = evaluate $ force $ UV.fromList (take i (randoms (mkStdGen 0)))

randomSampleSVVector :: Int -> IO (SV.Vector Int)
randomSampleSVVector i = evaluate $ force $ SV.fromList (take i (randoms (mkStdGen 0)))

randomSampleSeq :: Int -> IO (S.Seq Int)
randomSampleSeq i = evaluate $ force $ S.fromList (take i (randoms (mkStdGen 0)))

randomSampleMassivUArray :: Int -> IO (M.Array M.U Int Int)
randomSampleMassivUArray i = evaluate $ force ma where
  ma = M.fromList M.Seq (take i (randoms (mkStdGen 0)))

randomSampleRRB :: Int -> IO (RRB.Vector Int)
randomSampleRRB i = evaluate $ force $ RRB.fromList (take i (randoms (mkStdGen 0)))

sampleList :: Int -> IO [Int]
sampleList i = evaluate $ force [1..i]

sampleVector :: Int -> IO (V.Vector Int)
sampleVector i = evaluate $ force $ V.fromList [1..i]

sampleUVVector :: Int -> IO (UV.Vector Int)
sampleUVVector i = evaluate $ force $ UV.fromList [1..i]

sampleSVVector :: Int -> IO (SV.Vector Int)
sampleSVVector i = evaluate $ force $ SV.fromList [1..i]

sampleSeq :: Int -> IO (S.Seq Int)
sampleSeq i = evaluate $ force $ S.fromList [1..i]

sampleMassivUArray :: Int -> IO (M.Array M.U Int Int)
sampleMassivUArray i = evaluate $ force ma where
  ma :: M.Array M.U Int Int
  ma =  M.fromList M.Seq [1..i]

sampleRRB :: Int -> IO (RRB.Vector Int)
sampleRRB i = evaluate $ force $ RRB.fromList [1..i]
