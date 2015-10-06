module HEP.Data.LHCO.PipesUtil (getLHCOEvent) where

import           Control.Monad.Trans.State.Strict
import           Data.ByteString.Char8            (ByteString)
import           Pipes
import           Pipes.Attoparsec                 (parse)

import           HEP.Data.LHCO.Parser             (lhcoEvent)
import           HEP.Data.LHCO.Type               (Event)

getLHCOEvent :: Monad m => Producer ByteString m () -> Producer Event m ()
getLHCOEvent s = do (r, s') <- lift $ runStateT (parse lhcoEvent) s
                    case r of Just (Right ev) -> yield ev >> getLHCOEvent s'
                              _               -> return ()
