{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Compression.GZip      (decompress)
import           Control.Monad               (forever)
import qualified Data.ByteString.Lazy.Char8  as L
import           Data.List                   (intercalate, partition)
import           Pipes
import qualified Pipes.Prelude               as P
import           System.Environment          (getArgs)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil     as U
import           HEP.Kinematics.Variable     (mTLowerBound)
import           HEP.Kinematics.Variable.MT2 (mT2SymmMinuit2)

main :: IO ()
main = do
  let header = "# " ++ intercalate ", "
               [ "mTtrue", "mVisible", "mEffective", "mT2", "mTHiggsBound"
               , "deltaR" ]
  putStrLn header

  infile <- fmap head getArgs
  evStr <- fmap decompress (L.readFile infile)
  runEffect $ (U.eventEntryFromBS . L.toStrict) evStr
       >-> U.finalStates >-> basicSelection
       >-> U.groupByMother >-> P.map (mconcat . map part) >-> variables
       >-> P.print

isNeutrino :: Particle -> Bool
isNeutrino = (`elem` neutrinos) . idOf

basicSelection :: Monad m => Pipe [Particle] [Particle] m ()
basicSelection = P.map $ filter ((||) <$> isNeutrino <*> condThres)
  where condThres = (&&) <$> (>0.5) . pt <*> (<2.5) . abs . eta

part :: [Particle] -> KinematicObjects
part ps =
  let (invis, vis) = partition isNeutrino ps
      visSum = momentumSum vis
      visSelected | pt visSum > 20.0 && (abs . eta) visSum < 2.5 = [visSum]
                  | otherwise                                    = []
  in KinematicObjects ((transverseVector . momentumSum) invis) visSelected

data KinematicObjects = KinematicObjects { missing :: TransverseMomentum
                                         , visible :: [FourMomentum]
                                         } deriving Show

instance Monoid KinematicObjects where
  mempty = KinematicObjects zero []
  KinematicObjects miss1 vis1 `mappend` KinematicObjects miss2 vis2 =
    KinematicObjects (miss1 `mappend` miss2) (vis1 `mappend` vis2)

newtype Result = Result { getResult :: [(String, Double)] }

instance Show Result where
  show = intercalate "," . map (show . snd) . getResult

variables :: MonadIO m => Pipe KinematicObjects Result m ()
variables = forever $ do
  KinematicObjects { .. } <- await
  if pt missing > 20.0 && length visible == 2
  then do let mTtrue = transverseMassCluster visible missing
              mVisible = invariantMass visible
              mEffective = invariantMass (fourMomentum missing : visible)
              (visA:(visB:_)) = visible
              mT2 = mT2func visA visB missing 0
              dR = deltaR visA visB
          mTHiggsBound <- liftIO $ mTLowerBound visA visB missing mTau
          yield $ Result [ ("mTtrue",       mTtrue       )
                         , ("mVisible",     mVisible     )
                         , ("mEffective",   mEffective   )
                         , ("mT2",          mT2          )
                         , ("mTHiggsBound", mTHiggsBound )
                         , ("deltaR",       dR           ) ]
  else yield $ Result [ ("mTtrue",       -1)
                      , ("mVisible",     -1)
                      , ("mEffective",   -1)
                      , ("mT2",          -1)
                      , ("mTHiggsBound", -1)
                      , ("deltaR",       -1) ]

mT2func :: FourMomentum -> FourMomentum -> TransverseMomentum -> Double -> Double
mT2func visA visB ptmiss mInv = case mT2SymmMinuit2 visA visB ptmiss mInv of
                                  Right (val, _, _) -> val
                                  _                 -> -10

mTau :: Double
mTau = 1.77682
