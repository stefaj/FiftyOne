module Main where

import CodeGen
import Parser (whileParser)
import Text.Megaparsec

-- FIFTY ONE: A simple language for a simple microcontroller

main = do
  f <- getContents
--  putStrLn "Parse:"
  case parse whileParser "" f of
    Left err -> print err
    Right ast -> do
--      print ast
--      putStrLn ""
--      putStrLn "OUT:"
      putStrLn $ generateMain ast
