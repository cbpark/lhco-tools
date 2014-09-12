{-# LANGUAGE GADTs #-}

module HEP.Data.LHCO
       (
         module HEP.Data.LHCO.Type
       , module HEP.Data.LHCO.Parser

       , numPhoton
       , numElectron
       , numMuon
       , numTau
       , numJet
       , numBjet
       , missingET
       ) where

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader (Reader, ask)

import           HEP.Data.LHCO.Parser
import           HEP.Data.LHCO.Type

numObjs :: (Event -> [PhyObj a]) -> (PhyObj a -> Bool) -> Reader Event Int
numObjs self cutf = liftM (length . filter cutf . self) ask

numPhoton :: (PhyObj Photon -> Bool) -> Reader Event Int
numPhoton = numObjs photons

numElectron :: (PhyObj Electron -> Bool) -> Reader Event Int
numElectron = numObjs electrons

numMuon :: (PhyObj Muon -> Bool) -> Reader Event Int
numMuon = numObjs muons

numTau :: (PhyObj Tau -> Bool) -> Reader Event Int
numTau = numObjs taus

numJet :: (PhyObj Jet -> Bool) -> Reader Event Int
numJet = numObjs jets

numBjet :: (PhyObj Bjet -> Bool) -> Reader Event Int
numBjet = numObjs bjets

missingET :: Reader Event Double
missingET = do e <- ask
               let ObjMet (_, a) = met e
               return a
