module Main where

import           Control.Monad        (when)
import           Data.Attoparsec.Text
import qualified Data.Text.IO         as TI
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)

import           HEP.Data.LHCO.Parser

main :: IO ()
main = do
  args <- getArgs
  when (length args /=1) $ do
    putStrLn "Usage: testlhcoparse filename"
    exitFailure

  let infile = head args
  putStrLn $ "-- Reading " ++ show infile ++ "."
  evstr <- TI.readFile infile
  print $ parseOnly parseEvents evstr
