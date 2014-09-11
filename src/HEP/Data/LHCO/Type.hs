{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHCO.Type where

import           Data.IntMap (IntMap)

data Header = Header { numEve      :: Int -- ^ event number.
                     , triggerWord :: Int -- ^ triggering information.
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
                             typ   :: Int
                             -- | pseudorapidity.
                           , eta   :: Double
                             -- | azimuthal angle.
                           , phi   :: Double
                             -- | transverse momentum.
                           , pt    :: Double
                             -- | invariant mass of the object.
                             --
                             -- For a jet, it is constructed from all energy and
                             -- momentum that are contained within it.
                           , jmass :: Double
                             -- | number of tracks associated with the object.
                             --
                             -- In the case of a lepton, the number is multiplied
                             -- by the charge of the lepton.
                           , ntrk  :: Double
                             -- | either 1 or 2 for a jet that has been tagged as
                             -- containing a b-quark.
                           , btag  :: Double
                             -- | ratio of the hadronic /vs/ electromagnetic
                             -- energy deposited in the calorimeter cells.
                           , hadem :: Double
                           } deriving (Eq, Show)

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
data Charge = CPlus | CMinus
data TauProng = OneProng | ThreeProng

data PhyObj t where
  ObjPhoton   :: { photonTrack :: Track }     -> PhyObj Photon
  ObjElectron :: { electronTrack  :: Track
                 , electronCharge :: Charge } -> PhyObj Electron
  ObjMuon     :: { muonTrack  :: Track
                 , muonCharge :: Charge }     -> PhyObj Muon
  ObjTau      :: { tauTrack  :: Track
                 , tauCharge :: Charge
                 , tauProng  :: TauProng }    -> PhyObj Tau
  ObjJet      :: { jetTrack    :: Track
                 , jetMass     :: Double
                 , jetNumTrack :: Int }       -> PhyObj Jet
  ObjBjet     :: { bjetTrack    :: Track
                 , bjetMass     :: Double
                 , bjetNumTrack :: Int }      -> PhyObj Bjet
  ObjMet      :: { metTrack :: (Phi, Pt) }    -> PhyObj Met
  ObjUnknown  ::                                 PhyObj Unknown

data EachObj where
  EachObj :: PhyObj t -> EachObj

makeEachObj :: RawObject -> EachObj
makeEachObj RawObject { .. } =
  case typ of 0 -> EachObj $ ObjPhoton { photonTrack = (eta, phi, pt) }
              1 -> EachObj $ ObjElectron { electronTrack  = (eta, phi, pt)
                                         , electronCharge = ntrkToCharge ntrk }
              2 -> EachObj $ ObjMuon  { muonTrack  = (eta, phi, pt)
                                      , muonCharge = ntrkToCharge ntrk }
              3 -> EachObj $ ObjTau { tauTrack  = (eta, phi, pt)
                                    , tauCharge = ntrkToCharge ntrk
                                    , tauProng  = ntrkToProng ntrk }
              4 -> EachObj $ ObjJet { jetTrack    = (eta, phi, pt)
                                    , jetMass     = jmass
                                    , jetNumTrack = round ntrk }
              5 -> EachObj $ ObjBjet { bjetTrack    = (eta, phi, pt)
                                     , bjetMass     = jmass
                                     , bjetNumTrack = round ntrk }
              6 -> EachObj $ ObjMet { metTrack = (phi, pt) }
              _ -> EachObj $ ObjUnknown

ntrkToCharge :: Double -> Charge
ntrkToCharge n = if n > 0 then CPlus else CMinus

ntrkToProng :: Double -> TauProng
ntrkToProng n = if abs n < 1.1 then OneProng else ThreeProng

data Event = Event { eventNum  :: Int
                   , photons   :: IntMap (PhyObj Photon)
                   , electrons :: IntMap (PhyObj Electron)
                   , muons     :: IntMap (PhyObj Muon)
                   , taus      :: IntMap (PhyObj Tau)
                   , jets      :: IntMap (PhyObj Jet)
                   , bjets     :: IntMap (PhyObj Bjet)
                   , met       :: PhyObj Met }
