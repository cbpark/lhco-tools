{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module HEP.Data.LHCO.Type
       (
         Header (..)
       , RawObject (..)
       , Event (..)
       , PhyObj (..)
       , Photon
       , Electron
       , Muon
       , Tau
       , Jet
       , Bjet
       , Met
       , EachObj (..)
       , Track (..)
       , Charge (..)
       , TauProng (..)

       , HasFourMomentum (..)
       ) where

import           Data.Function            (on)

import           HEP.Vector.LorentzVector (LorentzVector, setEtaPhiPtM)
import qualified HEP.Vector.LorentzVector as LV
import           HEP.Vector.TwoVector     (phi2MPiPi)

data Header = Header { lhcoNumEve      :: !Int -- ^ event number.
                     , lhcoTriggerWord :: !Int -- ^ triggering information.
                     } deriving Show

data RawObject = RawObject { -- | type of object.
                             --
                             --     * 0 = photon
                             --
                             --     * 1 = electron
                             --
                             --     * 2 = muon
                             --
                             --     * 3 = hadronically-decaying tau
                             --
                             --     * 4 = jet
                             --
                             --     * 6 = missing transverse energy
                             lhcoTyp   :: !Int
                           , lhcoEta   :: !Double -- ^ pseudorapidity.
                           , lhcoPhi   :: !Double -- ^ azimuthal angle.
                           , lhcoPt    :: !Double -- ^ transverse momentum.
                             -- | invariant mass of the object.
                             --
                             -- For a jet, it is constructed from all energy and
                             -- momentum that are contained within it.
                           , lhcoJmass :: !Double
                             -- | number of tracks associated with the object.
                             --
                             -- In the case of a lepton, the number is multiplied
                             -- by the charge of the lepton.
                           , lhcoNtrk  :: !Double
                             -- | either 1 or 2 for a jet that has been tagged as
                             -- containing a b-quark.
                           , lhcoBtag  :: !Double
                             -- | ratio of the hadronic /vs/ electromagnetic
                             -- energy deposited in the calorimeter cells.
                           , lhcoHadem :: !Double
                           } deriving Show

data Photon
data Electron
data Muon
data Tau
data Jet
data Bjet
data Met
data Unknown

type Eta = Double
type Phi = Double
type Pt = Double
type Mass = Double
type Ntrk = Int

newtype Track = Track (Eta, Phi, Pt) deriving Eq
instance Show Track where
  show (Track (e, ph, p)) =
    "eta = " ++ show e ++ ", phi = " ++ show ph ++ ", pt = " ++ show p

data Charge = CPlus | CMinus deriving Eq
instance Show Charge where show q = case q of CPlus -> "1"
                                              _     -> "-1"

data TauProng = OneProng | ThreeProng deriving Eq
instance Show TauProng where show t = case t of OneProng -> "1"
                                                _        -> "3"

data PhyObj t where
  ObjPhoton   :: Track -> PhyObj Photon
  ObjElectron :: Track -> Charge -> PhyObj Electron
  ObjMuon     :: Track -> Charge -> PhyObj Muon
  ObjTau      :: Track -> Charge -> TauProng -> PhyObj Tau
  ObjJet      :: Track -> Mass -> Ntrk -> PhyObj Jet
  ObjBjet     :: Track -> Mass -> Ntrk -> PhyObj Bjet
  ObjMet      :: (Phi, Pt) -> PhyObj Met
  ObjUnknown  :: PhyObj Unknown

instance Show (PhyObj Photon) where
  show (ObjPhoton t) = "(" ++ show t ++ ")"

instance Show (PhyObj Electron) where
  show (ObjElectron t c) = "(" ++ show t ++ ", charge = " ++ show c ++ ")"

instance Show (PhyObj Muon) where
  show (ObjMuon t c) = "(" ++ show t ++ ", charge = " ++ show c ++ ")"

instance Show (PhyObj Tau) where
  show (ObjTau t c p) = "(" ++ show t ++ ", charge = " ++ show c ++
                        ", prong = " ++ show p ++ ")"

instance Show (PhyObj Jet) where
  show (ObjJet t m n) = "(" ++ show t ++ ", " ++ showJetMassNtrk m n ++ ")"

instance Show (PhyObj Bjet) where
  show (ObjBjet t m n) = "(" ++ show t ++ ", " ++ showJetMassNtrk m n ++ ")"

instance Show (PhyObj Met) where
  show (ObjMet (ph, p)) = "(phi = " ++ show ph ++ ", pt = " ++ show p ++ ")"

showJetMassNtrk :: Double -> Int -> String
showJetMassNtrk m n = "jmass = " ++ show m ++ ", ntrk = " ++ show n

data EachObj where
  EachObj :: PhyObj t -> EachObj

data Event = Event { neve      :: !Int
                   , photons   :: ![PhyObj Photon]
                   , electrons :: ![PhyObj Electron]
                   , muons     :: ![PhyObj Muon]
                   , taus      :: ![PhyObj Tau]
                   , jets      :: ![PhyObj Jet]
                   , bjets     :: ![PhyObj Bjet]
                   , met       :: !(PhyObj Met)
                   } deriving Show

class HasFourMomentum a where
  fourMomentum :: a -> LorentzVector Double
  pt :: a -> Double
  eta :: a -> Double
  phi :: a -> Double

  invMass :: [a] -> Double
  invMass = LV.invariantMass . momentumSum

  ptCompare :: a -> a -> Ordering
  ptCompare = flip compare `on` pt

  ptSum :: [a] -> Double
  ptSum = foldr (\p acc -> pt p + acc) 0

  momentumSum :: [a] -> LorentzVector Double
  momentumSum = LV.vectorSum . map fourMomentum

  deltaPhi :: a -> a -> Double
  deltaPhi p p' = phi2MPiPi $ phi p - phi p'

  deltaEta :: a -> a -> Double
  deltaEta = (-) `on` eta

  deltaR :: a -> a -> Double
  deltaR p p' = sqrt $ deta * deta + dphi * dphi
    where deta = deltaEta p p'
          dphi = deltaPhi p p'

  cosTheta :: a -> a -> Double
  cosTheta p p' = cos $ (LV.deltaTheta `on` fourMomentum) p p'

instance HasFourMomentum (PhyObj Photon) where
  fourMomentum (ObjPhoton (Track (e, ph, p))) = setEtaPhiPtM e ph p 0
  pt (ObjPhoton (Track (_, _, p))) = p
  eta (ObjPhoton (Track (e, _, _))) = e
  phi (ObjPhoton (Track (_, ph, _))) = ph

instance HasFourMomentum (PhyObj Electron) where
  fourMomentum (ObjElectron (Track (e, ph, p)) _) = setEtaPhiPtM e ph p 0
  pt (ObjElectron (Track (_, _, p)) _) = p
  eta (ObjElectron (Track (e, _, _)) _) = e
  phi (ObjElectron (Track (_, ph, _)) _) = ph

instance HasFourMomentum (PhyObj Muon) where
  fourMomentum (ObjMuon (Track (e, ph, p)) _) = setEtaPhiPtM e ph p 0
  pt (ObjMuon (Track (_, _, p)) _) = p
  eta (ObjMuon (Track (e, _, _)) _) = e
  phi (ObjMuon (Track (_, ph, _)) _) = ph

instance HasFourMomentum (PhyObj Tau) where
  fourMomentum (ObjTau (Track (e, ph, p)) _ _) = setEtaPhiPtM e ph p 0
  pt (ObjTau (Track (_, _, p)) _ _) = p
  eta (ObjTau (Track (e, _, _)) _ _) = e
  phi (ObjTau (Track (_, ph, _)) _ _) = ph

instance HasFourMomentum (PhyObj Jet) where
  fourMomentum (ObjJet (Track (e, ph, p)) m _) = setEtaPhiPtM e ph p m
  pt (ObjJet (Track (_, _, p)) _ _) = p
  eta (ObjJet (Track (e, _, _)) _ _) = e
  phi (ObjJet (Track (_, ph, _)) _ _) = ph

instance HasFourMomentum (PhyObj Bjet) where
  fourMomentum (ObjBjet (Track (e, ph, p)) m _) = setEtaPhiPtM e ph p m
  pt (ObjBjet (Track (_, _, p)) _ _) = p
  eta (ObjBjet (Track (e, _, _)) _ _) = e
  phi (ObjBjet (Track (_, ph, _)) _ _) = ph
