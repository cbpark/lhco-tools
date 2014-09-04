module HEP.Data.LHCO.Parser where

import           Control.Applicative
import           Data.Attoparsec.Text

import           HEP.Data.LHCO

comment :: Parser [()]
comment = many' $ do skipSpace
                     char '#'
                     skipWhile (not . isEndOfLine) <* endOfLine

header :: Parser Header
header = do skipSpace
            char '0'
            skipSpace
            nev <- decimal
            skipSpace
            tw <- decimal
            return Header { numEve = nev, triggerWord = tw }

objectline :: Parser ObjectLine
objectline = do skipSpace
                counter' <- decimal
                skipSpace
                typ' <- decimal
                skipSpace
                eta' <- double
                skipSpace
                phi' <- double
                skipSpace
                pt' <- double
                skipSpace
                jmass' <- double
                skipSpace
                ntrk' <- double
                skipSpace
                btag' <- double
                skipSpace
                hadem' <- double
                takeTill isEndOfLine
                return (counter', Object { _typ   = typ'
                                         , _eta   = eta'
                                         , _phi   = phi'
                                         , _pt    = pt'
                                         , _jmass = jmass'
                                         , _ntrk  = ntrk'
                                         , _btag  = btag'
                                         , _hadem = hadem' })
