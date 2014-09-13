{-# LANGUAGE
  DataKinds
  , KindSignatures
  , GADTs
  , TypeOperators
  #-}
module Sound.Scissors where

import GHC.TypeLits
import Data.Proxy
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import Control.Monad
import System.Process (callProcess)

data Side = Begin | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio'
  = Silence Integer Integer
  | File String
  | Pad Side Double Audio'
  | Trim Side Double Audio'
  | Cutoff Side Double Audio'
  | Concatenate [(Double, Audio')]
  | Merge [(Double, Audio')]
  | Mix [(Double, Audio')]
  | Resample Audio' Integer
  deriving (Eq, Ord, Show, Read)

newtype Audio (freq :: Nat) (chans :: Nat)
  = Audio { rawAudio :: Audio' }
  deriving (Eq, Ord, Show, Read)

concatenate :: (KnownNat f, KnownNat c) => [Audio f c] -> Audio f c
concatenate []    = silence
concatenate [aud] = aud
concatenate auds  = Audio $ Concatenate [ (1, rawAudio aud) | aud <- auds ]

merge :: Audio f c1 -> Audio f c2 -> Audio f (c1 + c2)
merge (Audio x) (Audio y) = Audio $ Merge [(1, x), (1, y)]

mix :: (KnownNat f, KnownNat c) => [Audio f c] -> Audio f c
mix []    = silence
mix [aud] = aud
mix auds  = Audio $ Mix [ (1, rawAudio aud) | aud <- auds ]

silence :: (KnownNat f, KnownNat c) => Audio f c
silence = let
  res = Audio $ Silence (frequency res) (channels res)
  in res

file :: String -> Audio f c
file = Audio . File

resample :: (KnownNat f2) => Audio f1 c -> Audio f2 c
resample (Audio x) = let
  res = Audio $ Resample x $ frequency res
  in res

pad, trim, cutoff :: Side -> Double -> Audio f c -> Audio f c
pad    side len = Audio . Pad    side len . rawAudio
trim   side len = Audio . Trim   side len . rawAudio
cutoff side len = Audio . Cutoff side len . rawAudio

frequency :: (KnownNat f) => Audio f c -> Integer
frequency = let
  makeProxy :: Audio f c -> Proxy f
  makeProxy _ = Proxy
  in natVal . makeProxy

channels :: (KnownNat c) => Audio f c -> Integer
channels = let
  makeProxy :: Audio f c -> Proxy c
  makeProxy _ = Proxy
  in natVal . makeProxy

myfile :: Audio 44100 2
myfile = file "stereo.mp3"

freaky :: Audio 48000 2
freaky = file "freaky.wav"

runSox :: [String] -> IO ()
runSox = callProcess "sox"

runAudio :: FilePath -> Audio' -> IO FilePath
runAudio tempdir = go where
  new :: IO FilePath
  new = do
    (fp, h) <- openTempFile tempdir "scissors.wav"
    hClose h
    return fp
  go x = case x of
    Silence freq chans -> do
      output <- new
      runSox
        [ "-n"
        , output
        , "trim", "0", "0"
        , "channels", show chans
        , "rate", show freq
        ]
      return output
    File fp -> return fp
    Concatenate auds -> combine "concatenate" auds
    Merge auds -> combine "merge" auds
    Mix auds -> combine "mix" auds
    Resample aud freq -> do
      input <- go aud
      output <- new
      runSox [input, output, "rate", show freq]
      return output
    Pad    Begin len aud -> transform aud ["pad", show len]
    Pad    End   len aud -> transform aud ["pad", "0", show len]
    Trim   Begin len aud -> transform aud ["trim", show len]
    Trim   End   len aud -> transform aud ["trim", "0", show $ negate len]
    Cutoff Begin len aud -> transform aud ["trim", "0", show len]
    Cutoff End   len aud -> transform aud ["trim", show $ negate len]
  transform :: Audio' -> [String] -> IO FilePath
  transform aud args = do
    input <- go aud
    output <- new
    runSox $ input : output : args
    return output
  combine :: String -> [(Double, Audio')] -> IO FilePath
  combine method auds = if null $ drop 1 auds
    then error "runAudio: can't combine less than 2 audio files"
    else do
      inputs <- forM auds $ \(v, aud) -> do
        f <- go aud
        return (v, f)
      output <- new
      runSox
        $ ["--combine", method]
        ++ (inputs >>= \(vel, f) -> ["-v", show vel, show f])
        ++ [output]
      return output
