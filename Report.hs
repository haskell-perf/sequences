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
                 (not . null . filter (not . null . filter (not . null)))
                 (groupBy (on (==) (takeWhile (/= '/') . concat . take 1)) rows))))

format :: [[String]] -> String
format rows =
  ("## " ++ takeWhile (/= '/') (concat (concat (take 1 (drop 1 rows))))) ++
  "\n\n" ++
  unlines [("|Name|" ++ intercalate "|" scales ++ "|"), "|" ++ concat (replicate (1 + length scales) "---|")] ++
  unlines
    (map (\name -> "|" ++ name ++ "|" ++ intercalate "|" (values name) ++ "|") (names))
  where
    values name =
      map (\(_:mean:_) -> float mean) (filter ((== name) . rowName) rows)
    names = nub (map rowName rows)
    scales = nub (map rowScale rows)
    rowName row =
      let s =
            takeWhile
              (/= ':')
              (dropWhile (== '/') (dropWhile (/= '/') (concat (take 1 row))))
      in s
    rowScale row =
      let scale = dropWhile (== ':') (dropWhile (/= ':') (concat (take 1 row)))
      in scale


float :: String -> String
float x = secs (read x)
