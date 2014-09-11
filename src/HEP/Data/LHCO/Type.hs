{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.LHCO.Type where

import           Data.List (foldl')

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
data Charge = CPlus | CMinus deriving Eq
data TauProng = OneProng | ThreeProng deriving Eq

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

instance Show (PhyObj Photon) where
  show o = let (x, y, z) = photonTrack o in showTrack x y z ++ ")"

instance Show (PhyObj Electron) where
  show o = let (x, y, z) = electronTrack o
           in showTrack x y z ++ ", " ++ showCharge (electronCharge o) ++ ")"

instance Show (PhyObj Muon) where
  show o = let (x, y, z) = muonTrack o
           in showTrack x y z ++ ", " ++ showCharge (muonCharge o)

instance Show (PhyObj Tau) where
  show o = let (x, y, z) = tauTrack o
           in showTrack x y z ++ ", " ++ showCharge (tauCharge o) ++
              ", prong = " ++ case (tauProng o) of OneProng -> show (1::Int)
                                                   _        -> show (3::Int)
              ++ ")"

instance Show (PhyObj Jet) where
  show o = let (x, y, z) = jetTrack o
           in showTrack x y z ++ ", " ++
              showJetMassNtrk (jetMass o) (jetNumTrack o) ++ ")"

instance Show (PhyObj Bjet) where
  show o = let (x, y, z) = bjetTrack o
           in showTrack x y z ++ ", " ++
              showJetMassNtrk (bjetMass o) (bjetNumTrack o) ++ ")"

instance Show (PhyObj Met) where
  show o = let (x, y) = metTrack o
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

makeEachObj :: RawObject -> EachObj
makeEachObj RawObject { .. } =
  let ntrkToCharge n = if n > 0 then CPlus else CMinus
      ntrkToProng n = if abs n < 1.1 then OneProng else ThreeProng
  in case typ of
      0 -> EachObj $ ObjPhoton { photonTrack = (eta, phi, pt) }
      1 -> EachObj $ ObjElectron { electronTrack  = (eta, phi, pt)
                                 , electronCharge = ntrkToCharge ntrk }
      2 -> EachObj $ ObjMuon  { muonTrack  = (eta, phi, pt)
                              , muonCharge = ntrkToCharge ntrk }
      3 -> EachObj $ ObjTau { tauTrack  = (eta, phi, pt)
                            , tauCharge = ntrkToCharge ntrk
                            , tauProng  = ntrkToProng ntrk }
      4 -> if btag > 0.0
           then EachObj $ ObjBjet { bjetTrack    = (eta, phi, pt)
                                  , bjetMass     = jmass
                                  , bjetNumTrack = round ntrk }
           else EachObj $ ObjJet { jetTrack    = (eta, phi, pt)
                                 , jetMass     = jmass
                                 , jetNumTrack = round ntrk }
      6 -> EachObj $ ObjMet { metTrack = (phi, pt) }
      _ -> EachObj $ ObjUnknown

type ObjType = String

data Event = Event { eventNum  :: Int
                   , photons   :: [PhyObj Photon]
                   , electrons :: [PhyObj Electron]
                   , muons     :: [PhyObj Muon]
                   , taus      :: [PhyObj Tau]
                   , jets      :: [PhyObj Jet]
                   , bjets     :: [PhyObj Bjet]
                   , met       :: PhyObj Met
                   } deriving Show

makeEvent :: Int -> [EachObj] -> Event
makeEvent n = foldl' addObj (Event n [] [] [] [] [] [] (ObjMet (0, 0)))

addObj :: Event -> EachObj -> Event
addObj ev (EachObj p@(ObjPhoton _))     = ev { photons   = p : (photons ev) }
addObj ev (EachObj p@(ObjElectron _ _)) = ev { electrons = p : (electrons ev) }
addObj ev (EachObj p@(ObjMuon _ _))     = ev { muons     = p : (muons ev) }
addObj ev (EachObj p@(ObjTau _ _ _))    = ev { taus      = p : (taus ev) }
addObj ev (EachObj p@(ObjJet _ _ _))    = ev { jets      = p : (jets ev) }
addObj ev (EachObj p@(ObjBjet _ _ _))   = ev { bjets     = p : (bjets ev) }
addObj ev (EachObj p@(ObjMet _))        = ev { met       = p }
addObj ev  (EachObj ObjUnknown)         = ev
