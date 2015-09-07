module Main where

import           Control.Monad.Trans.State.Strict
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.ByteString.Char8            (ByteString)
import           Data.List                        (intercalate, transpose)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.ByteString                 (fromHandle)
import qualified Pipes.Prelude                    as P
import           System.Environment               (getArgs)
import           System.IO

import           HEP.Analysis.Histogram1D

main :: IO ()
main = do
  let header = "# " ++ intercalate ", "
               ["bins", "mTtrue", "mVisible", "mEffective", "mTHiggsBound"]
  putStrLn header

  infile <- head <$> getArgs
  s <- withFile infile ReadMode getHists
  mapM_ putStrLn (showHists s)

data Variables = Variables { mTtrue       :: Double
                           , mVisible     :: Double
                           , mEffective   :: Double
                           , mTHiggsBound :: Double }

data Hists = Hists { mTtrueHist       :: Hist1D Double
                   , mVisibleHist     :: Hist1D Double
                   , mEffectiveHist   :: Hist1D Double
                   , mTHiggsBoundHist :: Hist1D Double }

instance Monoid Hists where
  mempty = Hists emptyHist emptyHist emptyHist emptyHist
  h1 `mappend` h2 = Hists (mTtrueHist h1 `mappend` mTtrueHist h2)
                          (mVisibleHist h1 `mappend` mVisibleHist h2)
                          (mEffectiveHist h1 `mappend` mEffectiveHist h2)
                          (mTHiggsBoundHist h1 `mappend` mTHiggsBoundHist h2)

showHists :: Hists -> [String]
showHists hs = let bs = bins (mTtrueHist hs)
                   mTtrue' = contents (mTtrueHist hs)
                   mVisible' = contents (mVisibleHist hs)
                   mEffective' = contents (mEffectiveHist hs)
                   mTHiggsBound' = contents (mTHiggsBoundHist hs)
               in map (intercalate ", " . map show) $
                  transpose [bs, mTtrue', mVisible', mEffective', mTHiggsBound']

getHists :: MonadIO m => Handle -> m Hists
getHists hin = P.fold mappend mempty id hists
  where hists = (getVariables . fromHandle) hin >-> P.map (varsToHists 10 90 100)

varsToHists :: Int -> Double -> Double -> Variables -> Hists
varsToHists nbin lo hi vs = Hists (varsToHists' [mTtrue vs])
                                  (varsToHists' [mVisible vs])
                                  (varsToHists' [mEffective vs])
                                  (varsToHists' [mTHiggsBound vs])
  where varsToHists' = histogram nbin lo hi

getVariables :: Monad m => Producer ByteString m () -> Producer Variables m ()
getVariables s = do (r, s') <- lift $ runStateT (PA.parse vars) s
                    case r of Just (Right ev) -> yield ev >> getVariables s'
                              _               -> return ()

vars :: Parser Variables
vars = do skipSpace
          many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
          mTtrue'       <- double
          char ',' >> skipSpace
          mVisible'     <- double
          char ',' >> skipSpace
          mEffective'   <- double
          char ',' >> skipSpace
          _             <- double
          char ',' >> skipSpace
          mTHiggsBound' <- double <* skipSpace
          return Variables { mTtrue       = mTtrue'
                           , mVisible     = mVisible'
                           , mEffective   = mEffective'
                           , mTHiggsBound = mTHiggsBound' }
