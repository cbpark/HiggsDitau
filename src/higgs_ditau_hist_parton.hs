{-# LANGUAGE RecordWildCards #-}

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
  s <- withFile infile ReadMode (getHists 10 90 100)
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
  h1 `mappend` h2 = Hists (      mTtrueHist h1 `mappend`       mTtrueHist h2)
                          (    mVisibleHist h1 `mappend`     mVisibleHist h2)
                          (  mEffectiveHist h1 `mappend`   mEffectiveHist h2)
                          (mTHiggsBoundHist h1 `mappend` mTHiggsBoundHist h2)

getHists :: MonadIO m => Int -> Double -> Double -> Handle -> m Hists
getHists nbin lo hi hin = P.fold mappend mempty id hists
  where hists = (getVariables . fromHandle) hin >-> P.map varsToHists
        varsToHists Variables { .. } =
          let varsToHists' = histogram1 nbin lo hi
          in Hists (varsToHists' mTtrue      )
                   (varsToHists' mVisible    )
                   (varsToHists' mEffective  )
                   (varsToHists' mTHiggsBound)

getVariables :: Monad m => Producer ByteString m () -> Producer Variables m ()
getVariables s = do (r, s') <- lift $ runStateT (PA.parse vars) s
                    case r of Just (Right ev) -> yield ev >> getVariables s'
                              _               -> return ()

vars :: Parser Variables
vars = do skipSpace
          _ <- many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
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

showHists :: Hists -> [String]
showHists hs =
  let (bs, mTtrue', mVisible', mEffective', mTHiggsBound') =
        (,,,,) <$> bins . mTtrueHist
        <*> contents . mTtrueHist     <*> contents . mVisibleHist
        <*> contents . mEffectiveHist <*> contents . mTHiggsBoundHist $ hs
  in map (intercalate ", " . map show) $
     transpose [bs, mTtrue', mVisible', mEffective', mTHiggsBound']
