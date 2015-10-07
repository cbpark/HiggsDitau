{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Compression.GZip     (decompress)
import           Control.Monad              (forever)
import           Control.Monad.Trans.Reader (runReader)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate, sortBy)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Environment         (getArgs)

import           HEP.Data.LHCO

main :: IO ()
main = do
  let header = "# " ++ intercalate ", "
               ["mTtrue", "mVisible", "mEffective", "mT2", "mTHiggsBound"]
  putStrLn header

  infile <- fmap head getArgs
  evStr <- fmap decompress (L.readFile infile)
  runEffect $ (eventFromBS . L.toStrict) evStr >-> basicSelection
    >-> P.map takeLeptonMet
    >-> P.print

basicSelection :: Monad m => Pipe Event Event m ()
basicSelection = forever $ do
  Event { .. } <- await
  let evFiltered = Event { nev      = nev
                         , photon   = filter basicSelection' photon
                         , electron = filter basicSelection' electron
                         , muon     = filter basicSelection' muon
                         , tau      = filter basicSelection' tau
                         , jet      = filter basicSelection' jet
                         , bjet     = filter basicSelection' bjet
                         , met      = met }
  yield evFiltered

basicSelection' :: PhyObj a -> Bool
basicSelection' (ObjPhoton _)                          = False
basicSelection' (ObjElectron (Track (eta', _, pt')) _) = abs eta' < 2.5 && pt' > 20.0
basicSelection' (ObjMuon (Track (eta', _, pt')) _ _)   = abs eta' < 2.5 && pt' > 20.0
basicSelection' (ObjTau (Track (eta', _, pt')) _ _ _)  = abs eta' < 2.5 && pt' > 20.0
basicSelection' (ObjJet (Track (eta', _, pt')) _ _)    = abs eta' < 2.5 && pt' > 15.0
basicSelection' (ObjBjet (Track (eta', _, pt')) _ _ _) = abs eta' < 2.5 && pt' > 15.0
basicSelection' (ObjMet _)                             = True
basicSelection' ObjUnknown                             = False

data Objects = Objects { missing     :: TransverseMomentum
                       , leptonicObj :: [FourMomentum]
                       } deriving Show

takeLeptonMet :: Event -> Objects
takeLeptonMet ev@Event {..} =
  let electrons = map fourMomentum electron
      muons     = map fourMomentum muon
      taus      = map fourMomentum tau
      leptons = sortBy ptCompare (electrons ++ muons ++ taus)
      miss = runReader missingET ev
  in Objects miss leptons
