{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Criterion.Main
import           Criterion.Measurement
import           Criterion.Types
import           Data.Function
import           Data.List
import qualified Data.List as L
import           Data.Ord
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           System.Directory
import           System.Environment
import           Text.CSV

data Conser = forall f. NFData (f Int) => Conser String (Int -> f Int)
data Replicator = forall f. NFData (f Int) => Replicator String (Int -> Int -> f Int)
data Indexing = forall f. NFData (f Int) => Indexing String (f Int) (f Int -> Int -> Int)

main :: IO ()
main = do
  let fp = "out.csv"
  list <- sampleList
  vector <- sampleVector
  uvector <- sampleUVVector
  seqd <- sampleSeq
  -- exists <- doesFileExist fp
  -- when exists (removeFile fp)
  -- defaultMainWith
  --   defaultConfig {csvFile = Just fp}
  --   [ bgroup
  --       "Consing"
  --       (conses
  --          [ Conser "Data.List" conslist
  --          , Conser "Data.Vector" consvector
  --          , Conser "Data.Vector.Unboxed" consuvector
  --          , Conser "Data.Sequence" consseq
  --          ])
  --   , bgroup
  --       "Replicate"
  --       (replicators
  --          [ Replicator "Data.List" L.replicate
  --          , Replicator "Data.Vector" V.replicate
  --          , Replicator "Data.Vector.Unboxed" UV.replicate
  --          , Replicator "Data.Sequence" S.replicate
  --          ])
  --   , bgroup
  --       "Indexing"
  --       (indexes
  --          [ Indexing "Data.List" list (L.!!)
  --          , Indexing "Data.Vector" vector (V.!)
  --          , Indexing "Data.Vector.Unboxed" uvector (UV.!)
  --          , Indexing "Data.Sequence" seqd (S.index)
  --          ])
  --   ]
  reportFromCsv fp
  where
    conses funcs =
      [ bench (title ++ " 0.." ++ show i) $ nf func i
      | i <- [10, 1000, 10000]
      , Conser title func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ " " ++ show i) $ nf (\(x, y) -> func x y) (i, 1234)
      | i <- [10, 1000, 10000]
      , Replicator title func <- funcs
      ]
    sampleList :: IO [Int]
    sampleList = evaluate $ force [1 .. 10000]
    sampleVector :: IO (V.Vector Int)
    sampleVector = evaluate $ force $ V.generate 10000 id
    sampleUVVector :: IO (UV.Vector Int)
    sampleUVVector = evaluate $ force $ UV.generate 10000 id
    sampleSeq :: IO (S.Seq Int)
    sampleSeq = evaluate $ force $ S.fromList [1 .. 10000]
    indexes funcs =
      [ bench (title ++ " " ++ show index) $ nf (\x -> func payload x) index
      | index <- [100, 1000, 8000]
      , Indexing title payload func <- funcs
      ]

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

reportFromCsv fp = do
  result <- parseCSVFromFile fp
  case result of
    Left e -> print e
    Right (_:rows) -> do
      !readme <- fmap force (readFile "README.md")
      let sep = "<!-- RESULTS -->"
          before = unlines (takeWhile (/= sep) (lines readme) ++ [sep ++ "\n"])
      writeFile
        "README.md"
        (before ++
         unlines
           (map
              format
              (filter
                 (not . null . filter (not . null))
                 (groupBy (on (==) (takeWhile (/= '/') . concat . take 1)) rows))))

format :: [[String]] -> String
format rows =
  ("## " ++ takeWhile (/= '/') (concat (concat (take 1 (drop 1 rows))))) ++
  "\n\n" ++ unlines ["|Name|Mean|Min|Max|Stddev|", "|---|---|---|---|---|"] ++
  unlines
    (map
       (\x ->
          case x of
            (name:mean:min:max:stddev:_) ->
              "|" ++
              intercalate
                " | "
                [ dropWhile (== '/') (dropWhile (/= '/') name)
                , float mean
                , float min
                , float max
                , float stddev
                ] ++
              "|"
            k -> error (show k))
       (filter (not . null . filter (not . null)) rows))

float x = secs (read x)
