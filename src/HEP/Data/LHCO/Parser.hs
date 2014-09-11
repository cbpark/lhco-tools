{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHCO.Parser (rawLHCOEvent, lhcoEvent) where

import           Control.Applicative              ((<*))
import           Control.Monad                    (mzero)
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.List                        (foldl', sortBy)

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
               return $ (sortEvent . makeEvent (numEve hd)) phyObjs

makeEachObj :: RawObject -> EachObj
makeEachObj RawObject { .. } =
  let ntrkToCharge n = if n > 0 then CPlus else CMinus
      ntrkToProng n = if abs n < 1.1 then OneProng else ThreeProng
  in case typ of 0 -> EachObj ObjPhoton { photonTrack = (eta, phi, pt) }
                 1 -> EachObj ObjElectron { electronTrack  = (eta, phi, pt)
                                          , electronCharge = ntrkToCharge ntrk }
                 2 -> EachObj ObjMuon  { muonTrack  = (eta, phi, pt)
                                       , muonCharge = ntrkToCharge ntrk }
                 3 -> EachObj ObjTau { tauTrack  = (eta, phi, pt)
                                     , tauCharge = ntrkToCharge ntrk
                                     , tauProng  = ntrkToProng ntrk }
                 4 -> if btag > 0
                      then EachObj ObjBjet { bjetTrack    = (eta, phi, pt)
                                           , bjetMass     = jmass
                                           , bjetNumTrack = round ntrk }
                      else EachObj ObjJet { jetTrack    = (eta, phi, pt)
                                          , jetMass     = jmass
                                          , jetNumTrack = round ntrk }
                 6 -> EachObj ObjMet { metTrack = (phi, pt) }
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
  where ptOrdering :: TrackObj a => [a] -> [a]
        ptOrdering = sortBy ptCompare
