{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.DeepSeq
import           Criterion.Measurement
import           Data.Function
import           Data.List
import           System.Environment
import           Text.CSV

main :: IO ()
main = do
  fp:_ <- getArgs
  reportFromCsv fp

reportFromCsv :: FilePath -> IO ()
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

float :: String -> String
float x = secs (read x)
