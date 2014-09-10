module HEP.Data.LHCO.Parser (lhcoEvent, lhcoEvents) where

import           Control.Applicative              ((<*))
import           Control.Monad                    (mzero)
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.IntMap                      (fromList)

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

object :: Parser (Int, Object)
object = do skipSpace
            counter' <- decimal
            if counter' == 0
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
                      return (counter', Object { _typ   = typ'
                                               , _eta   = eta'
                                               , _phi   = phi'
                                               , _pt    = pt'
                                               , _jmass = jmass'
                                               , _ntrk  = ntrk'
                                               , _btag  = btag'
                                               , _hadem = hadem' })

lhcoEvent :: Parser (Header, Objects)
lhcoEvent = do comment
               skipSpace
               char '0'
               hd <- header <* endOfLine
               objs <- many1' $ object <* endOfLine
               return (hd, fromList objs)
  where comment = many' $ skipSpace >> char '#' >> skipTillEnd >> endOfLine

lhcoEvents :: Parser [(Header, Objects)]
lhcoEvents =  many1' lhcoEvent
