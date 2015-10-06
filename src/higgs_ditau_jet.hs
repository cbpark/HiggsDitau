module Main where

import           Codec.Compression.GZip     (decompress)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Environment         (getArgs)

import           HEP.Data.LHCO              (eventFromBS)

main :: IO ()
main = do
  let header = "# " ++ intercalate ", "
               ["mTtrue", "mVisible", "mEffective", "mT2", "mTHiggsBound"]
  putStrLn header

  infile <- fmap head getArgs
  evStr <- fmap decompress (L.readFile infile)
  runEffect $ (eventFromBS . L.toStrict) evStr >-> P.print
