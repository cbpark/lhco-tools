module HEP.Data.LHCO.Parser (rawLHCOEvent, lhcoEvent) where

import           Control.Applicative              ((<*))
import           Control.Monad                    (mzero)
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)

import           HEP.Data.LHCO.Type

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine)

header :: Parser Header
header = do skipSpace
            nev <- decimal
            skipSpace
            tw  <- decimal
            skipTillEnd
            return Header { numEve = nev, triggerWord = tw }

object :: Parser RawObject
object = do skipSpace
            counter' <- decimal
            if (counter' :: Int) == 0
              then mzero
              else do skipSpace
                      typ'   <- decimal
                      skipSpace
                      eta'   <- double
                      skipSpace
                      phi'   <- double
                      skipSpace
                      pt'    <- double
                      skipSpace
                      jmass' <- double
                      skipSpace
                      ntrk'  <- double
                      skipSpace
                      btag'  <- double
                      skipSpace
                      hadem' <- double
                      skipTillEnd
                      return RawObject { typ   = typ'
                                       , eta   = eta'
                                       , phi   = phi'
                                       , pt    = pt'
                                       , jmass = jmass'
                                       , ntrk  = ntrk'
                                       , btag  = btag'
                                       , hadem = hadem' }

rawLHCOEvent :: Parser (Header, [RawObject])
rawLHCOEvent = do comment
                  skipSpace
                  char '0'
                  hd <- header <* endOfLine
                  objs <- many1' $ object <* endOfLine
                  return (hd, objs)
  where comment = many' $ skipSpace >> char '#' >> skipTillEnd >> endOfLine

lhcoEvent :: Parser Event
lhcoEvent = do (hd, rawObjs) <- rawLHCOEvent
               let phyObjs = map makeEachObj rawObjs
               return $ makeEvent (numEve hd) phyObjs
