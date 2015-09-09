{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List                   (intercalate, partition)
import           Pipes
import qualified Pipes.Prelude               as P
import           System.Environment          (getArgs)
import           System.IO                   (IOMode (..), withFile)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil     as U
import           HEP.Kinematics.Variable     (mTBound)
import           HEP.Kinematics.Variable.MT2 (mT2SymmMinuit2)

main :: IO ()
main = do
  let header = "# " ++ intercalate ", "
               ["mTtrue", "mVisible", "mEffective", "mT2", "mTHiggsBound"]
  putStrLn header

  infile <- head <$> getArgs
  withFile infile ReadMode $ \hin ->
    runEffect $ U.eventEntry hin >-> U.finalStates >-> U.groupByMother
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

newtype Result = Result { getResult :: [(String, Double)] }

instance Show Result where
  show = intercalate "," . map (show . snd) . getResult

variables :: KinematicObjects -> Result
variables KinematicObjects { .. } =
  let mTtrue = transverseMassCluster visible missing
      mVisible = invariantMass visible
      mEffective = invariantMass (fourMomentum missing : visible)
      twoVisibles = if length visible /= 2 then [] else visible
      (mT2, mTHiggsBound)
        | null twoVisibles = (-1, -1)
        | otherwise        = let (visA:(visB:_)) = twoVisibles
                             in ( mT2func visA visB missing 0
                                , mTBound visA visB missing mTau )
  in Result [ ("mTtrue",       mTtrue       )
            , ("mVisible",     mVisible     )
            , ("mEffective",   mEffective   )
            , ("mT2",          mT2          )
            , ("mTHiggsBound", mTHiggsBound )
            ]

mT2func :: FourMomentum -> FourMomentum -> TransverseMomentum -> Double -> Double
mT2func visA visB ptmiss mInv = case mT2SymmMinuit2 visA visB ptmiss mInv of
                                  Right (val, _, _) -> val
                                  _                 -> -10

mTau :: Double
mTau = 1.77682
