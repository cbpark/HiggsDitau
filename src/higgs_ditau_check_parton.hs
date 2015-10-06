module Main where

import           Codec.Compression.GZip  (decompress)
import qualified Data.ByteString.Lazy    as L
import           Data.List               (intercalate)
import           Pipes
import qualified Pipes.Prelude           as P
import           System.Environment      (getArgs)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil as U

main :: IO ()
main = do
  let header = "# " ++ intercalate ", " ["mAll", "mTau1", "mTau2"]
  putStrLn header

  infile <- fmap head getArgs
  evStr <- fmap decompress (L.readFile infile)
  runEffect $ (U.eventEntryFromBS . L.toStrict) evStr
    >-> U.finalStates >-> U.groupByMother >-> P.map invMasses
    >-> P.print

newtype InvariantMasses = InvariantMasses { getMasses :: (Double, Double, Double) }

instance Show InvariantMasses where
  show ms = let (m1, m2, m3) = getMasses ms
            in show m1 ++ "," ++ show m2 ++ "," ++ show m3

invMasses :: [[Particle]] -> InvariantMasses
invMasses ps
  | length ps /= 2 = InvariantMasses (0, 0, 0)
  | otherwise      = let (ps1:(ps2:_)) = ps
                         (vis1, vis2) = (momentumSum ps1, momentumSum ps2)
                         (invmass1, invmass2) = ( invariantMass [vis1]
                                                , invariantMass [vis2] )
                         invmassAll = invariantMass [vis1 `mappend` vis2]
                     in InvariantMasses (invmass1, invmass2, invmassAll)
