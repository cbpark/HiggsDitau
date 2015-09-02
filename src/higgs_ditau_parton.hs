{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List               (partition)
import           Pipes
import           Pipes.ByteString        (fromHandle)
import qualified Pipes.Prelude           as P
import           System.Environment      (getArgs)
import           System.IO               (IOMode (..), withFile)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil as U

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  withFile infile ReadMode $ \hin ->
    runEffect $ eventEntry (fromHandle hin)
    >-> U.finalStates >-> U.groupByMother
    >-> P.map (variables . mconcat . map part)
    -- >-> P.take 3
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
  in [ ("mTtrue",   mTtrue)
     , ("mVisible", mVisible)
     , ("mEffective", mEffective)
     ]
