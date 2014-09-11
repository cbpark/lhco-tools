module Main where

import           Control.Monad                    (when)
import           Control.Monad.Trans.State.Strict (runStateT)
import           Data.ByteString                  (ByteString)
import           Pipes                            (Producer, lift, liftIO,
                                                   runEffect)
import           Pipes.Attoparsec                 (parse)
import           Pipes.ByteString                 (fromHandle)
import           System.Environment               (getArgs)
import           System.Exit                      (exitFailure)
import           System.IO                        (IOMode (..), withFile)

import           HEP.Data.LHCO

parseAndPrint :: Producer ByteString IO r -> IO ()
parseAndPrint input = runEffect $ do
  (result, unused) <- lift $ runStateT (parse rawLHCOEvent) input
  case result of
   Nothing -> liftIO $ putStrLn "-- Done parsing."
   Just x  -> case x of Left _    -> return ()
                        Right str -> liftIO $ do print str
                                                 parseAndPrint unused

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: lhco_pipesparse_test filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  withFile infile ReadMode $ \inh -> parseAndPrint (fromHandle inh)
