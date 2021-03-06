{-# LANGUAGE GADTs #-}

module HEP.Data.LHCO
       (
         module LT
       , module LP
       , module LPU
       , module HK
       , module LV
       , module V2

       , numPhoton
       , numElectron
       , numMuon
       , numTau
       , numJet
       , numBjet
       , missingET
       ) where

import           Control.Monad                       (liftM)
import           Control.Monad.Trans.Reader          (Reader, ask)

import           HEP.Kinematics                      as HK
import           HEP.Kinematics.Vector.LorentzVector as LV (setEtaPhiPtM)
import           HEP.Kinematics.Vector.TwoVector     as V2 (setPtPhi)

import           HEP.Data.LHCO.Parser                as LP
import           HEP.Data.LHCO.PipesUtil             as LPU
import           HEP.Data.LHCO.Type                  as LT

numObjs :: (Event -> [PhyObj a]) -> (PhyObj a -> Bool) -> Reader Event Int
numObjs self cutf = liftM (length . filter cutf . self) ask

numPhoton :: (PhyObj Photon -> Bool) -> Reader Event Int
numPhoton = numObjs photon

numElectron :: (PhyObj Electron -> Bool) -> Reader Event Int
numElectron = numObjs electron

numMuon :: (PhyObj Muon -> Bool) -> Reader Event Int
numMuon = numObjs muon

numTau :: (PhyObj Tau -> Bool) -> Reader Event Int
numTau = numObjs tau

numJet :: (PhyObj Jet -> Bool) -> Reader Event Int
numJet = numObjs jet

numBjet :: (PhyObj Bjet -> Bool) -> Reader Event Int
numBjet = numObjs bjet

missingET :: Reader Event TransverseMomentum
missingET = do e <- ask
               let ObjMet (phi', pt') = met e
               return (setPtPhi pt' phi')
