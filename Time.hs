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
import qualified Data.DList as D
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Massiv.Array as M
import qualified Data.RRBVector as RRB
import qualified Acc
import qualified GHC.Exts
import           System.Directory
import           System.Random

data Conser        = forall f. NFData (f Int) => Conser        String (Int -> IO (f Int)) (Int -> f Int -> f Int)
data Snocer        = forall f. NFData (f Int) => Snocer        String (Int -> IO (f Int)) (f Int -> Int -> f Int)
data Append        = forall f. NFData (f Int) => Append        String (Int -> IO (f Int)) (f Int -> f Int -> f Int) (f Int -> f Int)
data Replicator    = forall f. NFData (f Int) => Replicator    String (Int -> Int -> f Int)
data Indexing      = forall f. NFData (f Int) => Indexing      String (IO (f Int)) (f Int -> Int -> Int)
data Normalization = forall f. NFData (f Int) => Normalization String (Int -> IO (f Int))
data Length        = forall f. NFData (f Int) => Length        String (Int -> IO (f Int)) (f Int -> Int)
data Min           = forall f. NFData (f Int) => Min           String (Int -> IO (f Int)) (f Int -> Int)
data Max           = forall f. NFData (f Int) => Max           String (Int -> IO (f Int)) (f Int -> Int)
data Sort          = forall f. NFData (f Int) => Sort          String (Int -> IO (f Int)) (f Int -> f Int)
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
           [ Conser "Data.List"            sampleList     (:)
           , Conser "Data.DList"           sampleDList    D.cons
           , Conser "Data.Vector"          sampleVector   V.cons
           , Conser "Data.Vector.Unboxed"  sampleUVVector UV.cons
           , Conser "Data.Vector.Storable" sampleSVVector SV.cons
           , Conser "Data.Sequence"        sampleSeq      (S.<|)
           , Conser "Data.RRBVector"       sampleRRB      (RRB.<|)
           , Conser "Acc"                  sampleAcc      Acc.cons
           ])
    , bgroup
        "Snocing"
        (snocs
           [ Snocer "Data.DList"           sampleDList    D.snoc
           , Snocer "Data.Vector"          sampleVector   V.snoc
           , Snocer "Data.Vector.Unboxed"  sampleUVVector UV.snoc
           , Snocer "Data.Vector.Storable" sampleSVVector SV.snoc
           , Snocer "Data.Sequence"        sampleSeq      (S.|>)
           , Snocer "Data.RRBVector"       sampleRRB      (RRB.|>)
           , Snocer "Acc"                  sampleAcc      (flip Acc.snoc)
           ])
    , bgroup
        "Append"
        (appends
           [ Append "Data.List"            sampleList     (++)      force
           , Append "Data.DList"           sampleDList    D.append id
           , Append "Data.Vector"          sampleVector   (V.++)   id
           , Append "Data.Vector.Unboxed"  sampleUVVector (UV.++)  id
           , Append "Data.Vector.Storable" sampleSVVector (SV.++)  id
           , Append "Data.Sequence"        sampleSeq      (S.><)   id
           , Append "Data.RRBVector"       sampleRRB      (RRB.><) id
           , Append "Acc"                  sampleAcc      (<>)     id
           ])
    , bgroup
        "Normalization"
        (normalizations
           [ Normalization "Data.List"            sampleList
           , Normalization "Data.DList"           sampleDList
           , Normalization "Data.Vector"          sampleVector
           , Normalization "Data.Vector.Unboxed"  sampleUVVector
           , Normalization "Data.Vector.Storable" sampleSVVector
           , Normalization "Data.Sequence"        sampleSeq
           , Normalization "Data.Massiv.Array"    sampleMassivUArray
           , Normalization "Data.RRBVector"       sampleRRB
           , Normalization "Data.Acc"             sampleAcc
           ])
    , bgroup
        "Indexing"
        (let size = 10005 in
         indexes
           [ Indexing "Data.List"            (sampleList         size) (L.!!)
           , Indexing "Data.Vector"          (sampleVector       size) (V.!)
           , Indexing "Data.Vector.Unboxed"  (sampleUVVector     size) (UV.!)
           , Indexing "Data.Vector.Storable" (sampleSVVector     size) (SV.!)
           , Indexing "Data.Sequence"        (sampleSeq          size) S.index
           , Indexing "Data.Massiv.Array"    (sampleMassivUArray size) M.index'
           , Indexing "Data.RRBVector"       (sampleRRB          size) (RRB.!)
           ])
    , bgroup
        "Length"
        (lengths
           [ Length "Data.List"            sampleList         L.length
           , Length "Data.DList"           sampleDList        length
           , Length "Data.Vector"          sampleVector       V.length
           , Length "Data.Vector.Unboxed"  sampleUVVector     UV.length
           , Length "Data.Vector.Storable" sampleSVVector     SV.length
           , Length "Data.Sequence"        sampleSeq          S.length
           , Length "Data.Massiv.Array"    sampleMassivUArray M.elemsCount
           , Length "Data.RRBVector"       sampleRRB          length
           , Length "Acc"                  sampleAcc          length
           ])
    , bgroup
        "Replicate"
        (replicators
           [ Replicator "Data.List"            L.replicate
           , Replicator "Data.DList"           D.replicate
           , Replicator "Data.Vector"          V.replicate
           , Replicator "Data.Vector.Unboxed"  UV.replicate
           , Replicator "Data.Vector.Storable" SV.replicate
           , Replicator "Data.Sequence"        S.replicate
           , Replicator "Data.RRBVector"       RRB.replicate
           ])
    , bgroup
        "Min"
        (mins
           [ Min "Data.List"            randomSampleList         L.minimum
           , Min "Data.DList"           randomSampleDList        minimum
           , Min "Data.Vector"          randomSampleVector       V.minimum
           , Min "Data.Vector.Unboxed"  randomSampleUVVector     UV.minimum
           , Min "Data.Vector.Storable" randomSampleSVVector     SV.minimum
           , Min "Data.Sequence"        randomSampleSeq          minimum
           , Min "Data.Massiv.Array"    randomSampleMassivUArray M.minimum'
           , Min "Data.RRBVector"       randomSampleRRB          minimum
           , Min "Acc"                  randomSampleAcc          minimum
           ])
    , bgroup
        "Max"
        (maxs
           [ Max "Data.List"            randomSampleList         L.maximum
           , Max "Data.DList"           randomSampleDList        maximum
           , Max "Data.Vector"          randomSampleVector       V.maximum
           , Max "Data.Vector.Unboxed"  randomSampleUVVector     UV.maximum
           , Max "Data.Vector.Storable" randomSampleSVVector     SV.maximum
           , Max "Data.Sequence"        randomSampleSeq          maximum
           , Max "Data.Massiv.Array"    randomSampleMassivUArray M.maximum'
           , Max "Data.RRBVector"       randomSampleRRB          maximum
           , Max "Acc"                  randomSampleAcc          maximum
           ])
    , bgroup
        "Filter Element"
        (let size = 10005 in
         removeElems
           [ RemoveElement "Data.List"            (sampleList     size) L.filter
           , RemoveElement "Data.Vector"          (sampleVector   size) V.filter
           , RemoveElement "Data.Vector.Unboxed"  (sampleUVVector size) UV.filter
           , RemoveElement "Data.Vector.Storable" (sampleSVVector size) SV.filter
           , RemoveElement "Data.Sequence"        (sampleSeq      size) S.filter
           ])
    , bgroup
        "Filter By Index"
        (let size = 10005 in
         removeByIndexes
           [ RemoveByIndex "Data.Vector"          (sampleVector   size) V.ifilter
           , RemoveByIndex "Data.Vector.Unboxed"  (sampleUVVector size) UV.ifilter
           , RemoveByIndex "Data.Vector.Storable" (sampleSVVector size) SV.ifilter
           ])
    , bgroup
        "Stable Sort"
        (sorts
           [ Sort "Data.List"            randomSampleList     L.sort
           , Sort "Data.Vector"          randomSampleVector   sortVec
           , Sort "Data.Vector.Unboxed"  randomSampleUVVector sortUVec
           , Sort "Data.Vector.Storable" randomSampleSVVector sortSVec
           , Sort "Data.Sequence"        randomSampleSeq      S.sort
           ])
    ]
  where
    benchSetNums = [10, 100, 1000, 10000]
    bench' :: String -> Int -> Benchmarkable -> Benchmark
    bench' title i = bench (title ++ ":" ++ show i)
    benchEnv :: NFData a => String -> (a -> Benchmarkable) -> (Int -> IO a) -> Int -> Benchmark
    benchEnv t g f i =
      env
        (f i)
        (bench' t i . g)

    conses funcs =
      [ benchEnv title ((`whnf` 1) . flip func) mkSample i
      | i <- benchSetNums
      , Conser title mkSample func <- funcs
      ]
    snocs funcs =
      [ benchEnv title ((`whnf` 1) . func) mkSample i
      | i <- benchSetNums
      , Snocer title mkSample func <- funcs
      ]
    appends funcs =
      [ benchEnv title (whnf (forcer . join func)) mkSample i
      | i <- benchSetNums
      , Append title mkSample func forcer <- funcs
      ]
    normalizations funcs =
      [ benchEnv title (nf id) mkSample len
      | len <- benchSetNums
      , Normalization title mkSample <- funcs
      ]
    indexes funcs =
      [ env
        payload
        (bench' title index . (`nf` index) . func)
      | index <- benchSetNums
      , Indexing title payload func <- funcs
      ]
    lengths funcs =
      [ benchEnv title (nf func) mkSample len
      | len <- benchSetNums
      , Length title mkSample func <- funcs
      ]
    replicators funcs =
      [ bench' title i $ nf (uncurry func) (i, 1234)
      | i <- benchSetNums
      , Replicator title func <- funcs
      ]
    mins funcs =
      [ benchEnv title (nf func) mkSample len
      | len <- benchSetNums
      , Min title mkSample func <- funcs
      ]
    maxs funcs =
      [ benchEnv title (nf func) mkSample len
      | len <- benchSetNums
      , Max title mkSample func <- funcs
      ]
    removeElems funcs =
      [ env
        payload
        (bench' title relem . nf (func (relem /=)))
      | relem <- benchSetNums
      , RemoveElement title payload func <- funcs
      ]
    removeByIndexes funcs =
      [ env
        payload
        (bench' title relem . nf (func $ const (relem /=)))
      | relem <- benchSetNums
      , RemoveByIndex title payload func <- funcs
      ]
    sorts funcs =
      [ benchEnv title (nf func) mkSample len
      | len <- benchSetNums
      , Sort title mkSample func <- funcs
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

randomSampleGen :: (NFData a, Random i) => ([i] -> a) -> Int -> IO a
randomSampleGen conv i = evaluate $ force $ conv $ take i $ randoms $ mkStdGen 0

randomSampleList :: Int -> IO [Int]
randomSampleList = randomSampleGen id

randomSampleDList :: Int -> IO (D.DList Int)
randomSampleDList = randomSampleGen D.fromList

randomSampleVector :: Int -> IO (V.Vector Int)
randomSampleVector = randomSampleGen V.fromList

randomSampleUVVector :: Int -> IO (UV.Vector Int)
randomSampleUVVector = randomSampleGen UV.fromList

randomSampleSVVector :: Int -> IO (SV.Vector Int)
randomSampleSVVector = randomSampleGen SV.fromList

randomSampleSeq :: Int -> IO (S.Seq Int)
randomSampleSeq = randomSampleGen S.fromList

randomSampleMassivUArray :: Int -> IO (M.Array M.U Int Int)
randomSampleMassivUArray = randomSampleGen (M.fromList M.Seq)

randomSampleRRB :: Int -> IO (RRB.Vector Int)
randomSampleRRB = randomSampleGen RRB.fromList

randomSampleAcc :: Int -> IO (Acc.Acc Int)
randomSampleAcc = randomSampleGen GHC.Exts.fromList


sampleGen :: (NFData a, Num i, Enum i) => ([i] -> a) -> i -> IO a
sampleGen conv i = evaluate $ force $ conv [1..i]

sampleList :: Int -> IO [Int]
sampleList = sampleGen id

sampleDList :: Int -> IO (D.DList Int)
sampleDList = sampleGen D.fromList

sampleVector :: Int -> IO (V.Vector Int)
sampleVector = sampleGen V.fromList

sampleUVVector :: Int -> IO (UV.Vector Int)
sampleUVVector = sampleGen UV.fromList

sampleSVVector :: Int -> IO (SV.Vector Int)
sampleSVVector = sampleGen SV.fromList

sampleSeq :: Int -> IO (S.Seq Int)
sampleSeq = sampleGen S.fromList

sampleMassivUArray :: Int -> IO (M.Array M.U Int Int)
sampleMassivUArray = sampleGen (M.fromList M.Seq)

sampleRRB :: Int -> IO (RRB.Vector Int)
sampleRRB = sampleGen RRB.fromList

sampleAcc :: Int -> IO (Acc.Acc Int)
sampleAcc = sampleGen GHC.Exts.fromList
