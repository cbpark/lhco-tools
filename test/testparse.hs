module Main where

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)
import           System.IO                       (IOMode (..), withFile)

import           HEP.Data.LHCO

parseAndPrint :: ByteString -> IO ()
parseAndPrint str = case parse lhcoEvent str of
                     Fail r _ _         -> C.putStrLn r
                     Done unused result -> do print result
                                              parseAndPrint unused

main :: IO ()
main = do
  args <- getArgs
  when (length args /=1) $ do
    putStrLn "Usage: lhco_parse_test filename"
    exitFailure

  let infile = head args
  putStrLn $ "-- Reading " ++ show infile ++ "."
  withFile infile ReadMode $ \inh -> do evstr <- C.hGetContents inh
                                        parseAndPrint evstr

  putStrLn "-- Done parsing."
