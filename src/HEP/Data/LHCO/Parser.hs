{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHCO.Parser (rawLHCOEvent, lhcoEvent) where

import           Control.Applicative              ((<*))
import           Control.Monad                    (mzero)
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.List                        (foldl', sortBy)

import           HEP.Vector                       (HasFourMomentum, ptCompare)

import           HEP.Data.LHCO.Type

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine)

header :: Parser Header
header = do skipSpace
            nev <- decimal
            skipSpace
            tw  <- decimal
            skipTillEnd
            return Header { lhcoNumEve = nev, lhcoTriggerWord = tw }

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
                      return RawObject { lhcoTyp   = typ'
                                       , lhcoEta   = eta'
                                       , lhcoPhi   = phi'
                                       , lhcoPt    = pt'
                                       , lhcoJmass = jmass'
                                       , lhcoNtrk  = ntrk'
                                       , lhcoBtag  = btag'
                                       , lhcoHadem = hadem' }

rawLHCOEvent :: Parser (Header, [RawObject])
rawLHCOEvent = do comment
                  skipSpace
                  char '0'
                  hd <- header <* endOfLine
                  objs <- many1' $ object <* endOfLine
                  return (hd, objs)
  where comment = many' $ skipSpace >> char '#' >> skipTillEnd >> endOfLine

lhcoEvent :: Parser Event
lhcoEvent = do (Header { .. }, rawObjs) <- rawLHCOEvent
               let phyObjs = map makeEachObj rawObjs
               return $ (sortEvent . makeEvent lhcoNumEve) phyObjs

makeEachObj :: RawObject -> EachObj
makeEachObj RawObject { .. } =
  let ntrkToCharge n = if n > 0 then CPlus else CMinus
      ntrkToProng n = if abs n < 1.1 then OneProng else ThreeProng
  in case lhcoTyp of
      0 -> EachObj $ ObjPhoton (Track (lhcoEta, lhcoPhi, lhcoPt))
      1 -> EachObj $ ObjElectron (Track (lhcoEta, lhcoPhi, lhcoPt))
                                 (ntrkToCharge lhcoNtrk)
      2 -> EachObj $ ObjMuon (Track (lhcoEta, lhcoPhi, lhcoPt))
                             (ntrkToCharge lhcoNtrk)
      3 -> EachObj $ ObjTau (Track (lhcoEta, lhcoPhi, lhcoPt))
                            (ntrkToCharge lhcoNtrk)
                            (ntrkToProng lhcoNtrk)
      4 -> if lhcoBtag > 0
           then EachObj $ ObjBjet (Track (lhcoEta, lhcoPhi, lhcoPt))
                                  lhcoJmass (round lhcoNtrk)
           else EachObj $ ObjJet (Track (lhcoEta, lhcoPhi, lhcoPt))
                                 lhcoJmass (round lhcoNtrk)
      6 -> EachObj $ ObjMet (lhcoPhi, lhcoPt)
      _ -> EachObj ObjUnknown

makeEvent :: Int -> [EachObj] -> Event
makeEvent n = foldl' addObj (Event n [] [] [] [] [] [] (ObjMet (0, 0)))
  where addObj :: Event -> EachObj -> Event
        addObj ev (EachObj p@(ObjPhoton _))    = ev { photons   = p : photons ev }
        addObj ev (EachObj p@(ObjElectron {})) = ev { electrons = p : electrons ev }
        addObj ev (EachObj p@(ObjMuon {}))     = ev { muons     = p : muons ev }
        addObj ev (EachObj p@(ObjTau {}))      = ev { taus      = p : taus ev }
        addObj ev (EachObj p@(ObjJet {}))      = ev { jets      = p : jets ev }
        addObj ev (EachObj p@(ObjBjet {}))     = ev { bjets     = p : bjets ev }
        addObj ev (EachObj p@(ObjMet {}))      = ev { met       = p }
        addObj ev (EachObj ObjUnknown)         = ev

sortEvent :: Event -> Event
sortEvent ev = ev { photons   = ptOrdering (photons ev)
                  , electrons = ptOrdering (electrons ev)
                  , muons     = ptOrdering (muons ev)
                  , taus      = ptOrdering (taus ev)
                  , jets      = ptOrdering (jets ev)
                  , bjets     = ptOrdering (bjets ev) }
  where ptOrdering :: HasFourMomentum a => [a] -> [a]
        ptOrdering = sortBy ptCompare
