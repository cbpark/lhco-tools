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

       , Trackable (..)
       ) where

import           Data.Function            (on)

import           HEP.Vector.LorentzVector as LV

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

newtype Track = Track { getTrack :: (Eta, Phi, Pt) } deriving Eq
instance Show Track where
  show t = let (a, b, c) = getTrack t
           in "eta = " ++ show a ++ ", phi = " ++ show b ++ ", pt = " ++ show c

data Charge = CPlus | CMinus deriving Eq
instance Show Charge where show q = case q of CPlus -> "1"
                                              _     -> "-1"

data TauProng = OneProng | ThreeProng deriving Eq
instance Show TauProng where show t = case t of OneProng -> "1"
                                                _        -> "3"

data PhyObj t where
  ObjPhoton   :: { photonTrack :: !Track }     -> PhyObj Photon
  ObjElectron :: { electronTrack  :: !Track
                 , electronCharge :: !Charge } -> PhyObj Electron
  ObjMuon     :: { muonTrack  :: !Track
                 , muonCharge :: !Charge }     -> PhyObj Muon
  ObjTau      :: { tauTrack  :: !Track
                 , tauCharge :: !Charge
                 , tauProng  :: !TauProng }    -> PhyObj Tau
  ObjJet      :: { jetTrack    :: !Track
                 , jetMass     :: !Double
                 , jetNumTrack :: !Int }       -> PhyObj Jet
  ObjBjet     :: { bjetTrack    :: !Track
                 , bjetMass     :: !Double
                 , bjetNumTrack :: !Int }      -> PhyObj Bjet
  ObjMet      :: { metTrack :: !(Phi, Pt) }    -> PhyObj Met
  ObjUnknown  ::                                  PhyObj Unknown

instance Show (PhyObj Photon) where
  show p = "(" ++ show (photonTrack p) ++ ")"

instance Show (PhyObj Electron) where
  show p = "(" ++ show (electronTrack p) ++
           ", charge = " ++ show (electronCharge p) ++ ")"

instance Show (PhyObj Muon) where
  show p = "(" ++ show (muonTrack p) ++
           ", charge = " ++ show (muonCharge p) ++ ")"

instance Show (PhyObj Tau) where
  show p = "(" ++ show (tauTrack p) ++
           ", charge = " ++ show (tauCharge p) ++
           ", prong = " ++ show (tauProng p) ++ ")"

instance Show (PhyObj Jet) where
  show p = "(" ++ show (jetTrack p) ++ ", " ++
           showJetMassNtrk (jetMass p) (jetNumTrack p) ++ ")"

instance Show (PhyObj Bjet) where
  show p = "(" ++ show (bjetTrack p) ++ ", " ++
           showJetMassNtrk (bjetMass p) (bjetNumTrack p) ++ ")"

instance Show (PhyObj Met) where
  show p = let (x, y) = metTrack p
           in "(phi = " ++ show x ++ ", pt = " ++ show y ++ ")"

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

class Trackable a where
  fourMomentum :: a -> LorentzVector Double
  ptMag :: a -> Double

  ptCompare :: a -> a -> Ordering
  ptCompare = flip compare `on` ptMag

  momentumSum :: [a] -> LorentzVector Double
  momentumSum = vectorSum . map fourMomentum

  ptSum :: [a] -> Double
  ptSum = foldr (\p acc -> ptMag p + acc) 0

  invMass :: [a] -> Double
  invMass = LV.invariantMass . momentumSum

  rapidity :: a -> Double
  rapidity = LV.eta . fourMomentum

  deltaR :: a -> a -> Double
  deltaR = LV.deltaR `on` fourMomentum

  deltaPhi :: a -> a -> Double
  deltaPhi = LV.deltaPhi `on` fourMomentum

  cosTheta :: a -> a -> Double
  cosTheta p p' = cos $ (deltaTheta `on` fourMomentum) p p'

instance Trackable (PhyObj Photon) where
  ptMag p = let (_, _, pt') = (getTrack . photonTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . photonTrack) p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Electron) where
  ptMag p = let (_, _, pt') = (getTrack . electronTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . electronTrack) p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Muon) where
  ptMag p = let (_, _, pt') = (getTrack . muonTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . muonTrack) p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Tau) where
  ptMag p = let (_, _, pt') = (getTrack . tauTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . tauTrack) p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Jet) where
  ptMag p = let (_, _, pt') = (getTrack . jetTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . jetTrack) p
                   in setEtaPhiPtM eta' phi' pt' (jetMass p)

instance Trackable (PhyObj Bjet) where
  ptMag p = let (_, _, pt') = (getTrack . bjetTrack) p in pt'
  fourMomentum p = let (eta', phi', pt') = (getTrack . bjetTrack) p
                   in setEtaPhiPtM eta' phi' pt' (bjetMass p)
