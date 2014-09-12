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

numObjs :: (Event -> [PhyObj a]) -> Reader Event Int
numObjs f = liftM (length . f) ask

numPhoton :: Reader Event Int
numPhoton = numObjs photons

numElectron :: Reader Event Int
numElectron = numObjs electrons

numMuon :: Reader Event Int
numMuon = numObjs muons

numTau :: Reader Event Int
numTau = numObjs taus

numJet :: Reader Event Int
numJet = numObjs jets

numBjet :: Reader Event Int
numBjet = numObjs bjets

missingET :: Reader Event Double
missingET = do e <- ask
               let ObjMet (_, a) = met e
               return a
