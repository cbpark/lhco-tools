{-# LANGUAGE TemplateHaskell #-}

module HEP.Data.LHCO where

import           Control.Lens
import           Data.IntMap  (IntMap)

data Header = Header { numEve      :: Int -- ^ event number.
                     , triggerWord :: Int -- ^ triggering information.
                     } deriving Show

data Object = Object { -- | type of object.
                       --
                       --     * 0 = photon
                       --     * 1 = electron
                       --     * 2 = muon
                       --     * 3 = hadronically-decaying tau
                       --     * 4 = jet
                       --     * 6 = missing transverse energy
                       _typ   :: Int
                       -- | pseudorapidity.
                     , _eta   :: Double
                       -- | azimuthal angle.
                     , _phi   :: Double
                       -- | transverse momentum.
                     , _pt    :: Double
                       -- | invariant mass of the object.
                       --
                       -- For a jet, it is constructed from all energy and
                       -- momentum that are contained within it.
                     , _jmass :: Double
                       -- | number of tracks associated with the object.
                       --
                       -- In the case of a lepton, the number is multiplied by
                       -- the charge of the lepton.
                     , _ntrk  :: Double
                       -- | either 1 or 2 for a jet that has been tagged as
                       -- containing a b-quark.
                     , _btag  :: Double
                       -- | ratio of the hadronic /vs/ electromagnetic energy
                       -- deposited in the calorimeter cells.
                     , _hadem :: Double
                     } deriving (Eq, Show)

makeLenses ''Object

type Objects = IntMap Object
type Event = (Header, Objects)
