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

data Conser = forall f. NFData (f Int) => Conser String (Int -> IO (f Int)) (Int -> f Int -> f Int)
data Snocer = forall f. NFData (f Int) => Snocer String (Int -> IO (f Int)) (f Int -> Int -> f Int)
data Append = forall f. NFData (f Int) => Append String (Int -> IO (f Int)) (f Int -> f Int -> f Int) (f Int -> f Int)
data Replicator = forall f. NFData (f Int) => Replicator String (Int -> Int -> f Int)
data Indexing = forall f. NFData (f Int) => Indexing String (IO (f Int)) (f Int -> Int -> Int)
data Normalization = forall f. NFData (f Int) => Normalization String (Int -> IO (f Int))
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

    conses funcs =
      [ env
        (sample i)
        (\p -> bench (title ++ ":" ++ show i) (whnf (\e -> func e p) 1))
      | i <- benchSetNums
      , Conser title sample func <- funcs
      ]
    snocs funcs =
      [ env
        (sample i)
        (\p -> bench (title ++ ":" ++ show i) (whnf (\e -> func p e) 1))
      | i <- benchSetNums
      , Snocer title sample func <- funcs
      ]
    appends funcs =
      [ env
        (payload i)
        (\p -> bench (title ++ ":" ++ show i) $ whnf (\x -> forcer (func x x)) p)
      | i <- benchSetNums
      , Append title payload func forcer <- funcs
      ]
    normalizations funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf id p)
      | len <- benchSetNums
      , Normalization title payload <- funcs
      ]
    indexes funcs =
      [ env
        payload
        (\p -> bench (title ++ ":" ++ show index) $ nf (\x -> func p x) index)
      | index <- benchSetNums
      , Indexing title payload func <- funcs
      ]
    lengths funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- benchSetNums
      , Length title payload func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ ":" ++ show i) $ nf (\(x, y) -> func x y) (i, 1234)
      | i <- benchSetNums
      , Replicator title func <- funcs
      ]
    mins funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- benchSetNums
      , Min title payload func <- funcs
      ]
    maxs funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- benchSetNums
      , Max title payload func <- funcs
      ]
    removeElems funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $ nf (\x -> func (/= relem) x) p)
      | relem <- benchSetNums
      , RemoveElement title payload func <- funcs
      ]
    removeByIndexes funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $
           nf (\x -> func (\index _ -> index /= relem) x) p)
      | relem <- benchSetNums
      , RemoveByIndex title payload func <- funcs
      ]
    sorts funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- benchSetNums
      , Sort title payload func <- funcs
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
