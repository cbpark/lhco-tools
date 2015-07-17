module Main where

import           Control.Monad                    (when)
import           Control.Monad.Trans.State.Strict (runStateT)
import           Data.ByteString                  (ByteString)
import           Pipes
import           Pipes.Attoparsec                 (parse)
import           Pipes.ByteString                 (fromHandle)
import qualified Pipes.Prelude                    as P
import           System.Environment               (getArgs)
import           System.Exit                      (exitFailure)
import           System.IO                        (IOMode (..), withFile)

import           HEP.Data.LHCO

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: lhco_pipesparse_test filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  withFile infile ReadMode $ \hin ->
    runEffect $ getLHCOEvent (fromHandle hin) >-> P.take 3 >-> P.print
  putStrLn "-- Done parsing."

main' :: IO ()
main' = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: lhco_pipesparse_test filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  withFile infile ReadMode $ \inh -> parseAndPrint (fromHandle inh)

parseAndPrint :: Producer ByteString IO r -> IO ()
parseAndPrint input = runEffect $ do
  (result, unused) <- lift $ runStateT (parse lhcoEvent) input
  case result of
   Nothing -> liftIO $ putStrLn "-- Done parsing."
   Just x  -> case x of Left _    -> return ()
                        Right str -> liftIO $ do print str
                                                 parseAndPrint unused
