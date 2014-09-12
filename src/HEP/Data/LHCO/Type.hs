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
       , EachObj (..)
       , Charge (..)
       , TauProng (..)

       , Trackable (..)
       ) where

import           Data.Function            (on)

import           HEP.Vector.LorentzVector

data Header = Header { numEve      :: !Int -- ^ event number.
                     , triggerWord :: !Int -- ^ triggering information.
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
                             typ   :: !Int
                             -- | pseudorapidity.
                           , eta   :: !Double
                             -- | azimuthal angle.
                           , phi   :: !Double
                             -- | transverse momentum.
                           , pt    :: !Double
                             -- | invariant mass of the object.
                             --
                             -- For a jet, it is constructed from all energy and
                             -- momentum that are contained within it.
                           , jmass :: !Double
                             -- | number of tracks associated with the object.
                             --
                             -- In the case of a lepton, the number is multiplied
                             -- by the charge of the lepton.
                           , ntrk  :: !Double
                             -- | either 1 or 2 for a jet that has been tagged as
                             -- containing a b-quark.
                           , btag  :: !Double
                             -- | ratio of the hadronic /vs/ electromagnetic
                             -- energy deposited in the calorimeter cells.
                           , hadem :: !Double
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
type Track = (Eta, Phi, Pt)
data Charge = CPlus | CMinus deriving Eq
data TauProng = OneProng | ThreeProng deriving Eq

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
  ObjUnknown  ::                                 PhyObj Unknown

instance Show (PhyObj Photon) where
  show p = let (x, y, z) = photonTrack p in showTrack x y z ++ ")"

instance Show (PhyObj Electron) where
  show p = let (x, y, z) = electronTrack p
           in showTrack x y z ++ ", " ++ showCharge (electronCharge p) ++ ")"

instance Show (PhyObj Muon) where
  show p = let (x, y, z) = muonTrack p
           in showTrack x y z ++ ", " ++ showCharge (muonCharge p)

instance Show (PhyObj Tau) where
  show p = let (x, y, z) = tauTrack p
           in showTrack x y z ++ ", " ++ showCharge (tauCharge p) ++
              ", prong = " ++ case (tauProng p) of OneProng -> show (1::Int)
                                                   _        -> show (3::Int)
              ++ ")"

instance Show (PhyObj Jet) where
  show p = let (x, y, z) = jetTrack p
           in showTrack x y z ++ ", " ++
              showJetMassNtrk (jetMass p) (jetNumTrack p) ++ ")"

instance Show (PhyObj Bjet) where
  show p = let (x, y, z) = bjetTrack p
           in showTrack x y z ++ ", " ++
              showJetMassNtrk (bjetMass p) (bjetNumTrack p) ++ ")"

instance Show (PhyObj Met) where
  show p = let (x, y) = metTrack p
           in "(phi = " ++ show x ++ ", pt = " ++ show y ++ ")"

showTrack :: Double -> Double -> Double -> String
showTrack a b c =
  "(eta = " ++ show a ++ ", phi = " ++ show b ++ ", pt = " ++ show c

showCharge :: Charge -> String
showCharge c = "charge = " ++ case c of CPlus -> show (1::Int)
                                        _     -> show (-1::Int)

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
  ptCompare = (flip compare) `on` ptMag

  momentumSum :: [a]  -> LorentzVector Double
  momentumSum = vectorSum . map fourMomentum

  ptSum :: [a] -> Double
  ptSum = foldr (\p acc -> ptMag p + acc) 0

  invMass :: [a] -> Double
  invMass = invariantMass . momentumSum

  rapidity :: a -> Double
  rapidity = HEP.Vector.LorentzVector.eta . fourMomentum

  dR :: a -> a -> Double
  dR = deltaR `on` fourMomentum

  cosTheta :: a -> a -> Double
  cosTheta p p' = cos $ (deltaTheta `on` fourMomentum) p p'

instance Trackable (PhyObj Photon) where
  ptMag p = let (_, _, pt') = photonTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = photonTrack p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Electron) where
  ptMag p = let (_, _, pt') = electronTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = electronTrack p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Muon) where
  ptMag p = let (_, _, pt') = muonTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = muonTrack p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Tau) where
  ptMag p = let (_, _, pt') = tauTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = tauTrack p
                   in setEtaPhiPtM eta' phi' pt' 0

instance Trackable (PhyObj Jet) where
  ptMag p = let (_, _, pt') = jetTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = jetTrack p
                   in setEtaPhiPtM eta' phi' pt' (jetMass p)

instance Trackable (PhyObj Bjet) where
  ptMag p = let (_, _, pt') = bjetTrack p in pt'
  fourMomentum p = let (eta', phi', pt') = bjetTrack p
                   in setEtaPhiPtM eta' phi' pt' (bjetMass p)
