module Main where

import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (($$), (=$))
import           Data.Conduit.Attoparsec      (conduitParser)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

import           HEP.Data.LHCO

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: lhco_conduitparse_test filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  runResourceT $
    CB.sourceFile infile $$ conduitParser lhcoEvent =$
    CL.mapM_ (liftIO . print . snd)

  putStrLn "-- Done parsing."
