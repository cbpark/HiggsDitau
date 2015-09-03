{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List                   (partition)
import           Pipes
import           Pipes.ByteString            (fromHandle)
import qualified Pipes.Prelude               as P
import           System.Environment          (getArgs)
import           System.IO                   (IOMode (..), withFile)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil     as U
import           HEP.Kinematics.Variable     (mTBound)
import           HEP.Kinematics.Variable.MT2 (mT2AsymmBisect)

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  withFile infile ReadMode $ \hin ->
    runEffect $ eventEntry (fromHandle hin)
    >-> U.finalStates >-> U.groupByMother
    >-> P.map (variables . mconcat . map part)
    >-> P.print

part :: [Particle] -> KinematicObjects
part ps =
  let (invis, vis) = partition ((`elem` neutrinos) . idOf) ps
  in KinematicObjects ((transverseVector . momentumSum) invis) [momentumSum vis]

data KinematicObjects = KinematicObjects { missing :: TransverseMomentum
                                         , visible :: [FourMomentum]
                                         } deriving Show

instance Monoid KinematicObjects where
  mempty = KinematicObjects zeroV2 []
  (KinematicObjects miss1 vis1) `mappend` (KinematicObjects miss2 vis2) =
    KinematicObjects (miss1 `mappend` miss2) (vis1 `mappend` vis2)

type PartonLevelResult = [(String, Double)]

variables :: KinematicObjects -> PartonLevelResult
variables KinematicObjects { .. } =
  let mTtrue = transverseMassCluster visible missing
      mVisible = invariantMass visible
      mEffective = invariantMass (fourMomentum missing : visible)
      twoVisibles = if length visible /= 2 then [] else visible
      (mT2, mTHiggsBound)
        | null twoVisibles = (-10, -10)
        | otherwise        = let (visA:(visB:_)) = twoVisibles
                             in  ( mT2AsymmBisect visA visB missing 0 0
                                 , mTBound        visA visB missing mTau )
  in [ ("mTtrue",   mTtrue)
     , ("mVisible", mVisible)
     , ("mEffective", mEffective)
     , ("mT2", mT2)
     , ("mTHiggsBound", mTHiggsBound)
     ]

mTau :: Double
mTau = 1.77682
