module Main where

import           Data.List           (intercalate)
import           Options.Applicative
import           System.IO           (IOMode (..), withFile)

import           Analysis.Parton     (getHists, showHists)

main :: IO ()
main = do
  putStrLn $ "# " ++ intercalate ", "
    ["bins", "mTtrue", "mVisible", "mEffective", "mTHiggsBound"]

  args <- execParser opts
  histograms args
    where opts = info (helper <*> cmdoptions)
                 (fullDesc
                  <> progDesc "Obtain histograms"
                  <> header "higgs_ditau_hist_parton")

histograms :: Args -> IO ()
histograms (Args infile n l u) =
  withFile infile ReadMode (getHists (read n :: Int)
                                     (read l :: Double)
                                     (read u :: Double))
  >>= mapM_ putStrLn . showHists

data Args = Args { input :: String
                 , nbin  :: String
                 , lower :: String
                 , upper :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$>
             argument str (metavar "INPUT"
                           <> help "Input data file (ex: input.dat)")
             <*> strOption (long "nbin"
                            <> short 'n'
                            <> metavar "nbin"
                            <> help "Number of bins")
             <*> strOption (long "lower"
                            <> short 'l'
                            <> metavar "lower"
                            <> help "Lower bound")
             <*> strOption (long "upper"
                            <> short 'u'
                            <> metavar "upper"
                            <> help "Upper bound")
