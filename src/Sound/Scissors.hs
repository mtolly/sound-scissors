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

data Side = Front | Back
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Time
  = Seconds Double
  | Samples Double
  deriving (Eq, Ord, Show, Read)

-- | A raw audio expression.
data Rawdio
  = Silence     Integer Integer
  | File        FilePath
  | Pad         Side Time Rawdio
  | Trim        Side Time Rawdio
  | Cutoff      Side Time Rawdio
  | Concatenate [(Double, Rawdio)]
  | Merge       [(Double, Rawdio)]
  | Mix         [(Double, Rawdio)]
  | Resample    Rawdio Integer
  deriving (Eq, Ord, Show, Read)

-- | An audio expression, typed by sample frequency and number of channels.
newtype Audio (freq :: Nat) (chans :: Nat)
  = Audio { toRawdio :: Rawdio }
  deriving (Eq, Ord, Show, Read)

unsafeFromRawdio :: Rawdio -> Audio c f
unsafeFromRawdio = Audio

-- | Glues audio files together sequentially.
concatenate :: (KnownNat f, KnownNat c) => [Audio f c] -> Audio f c
concatenate []    = silence
concatenate [aud] = aud
concatenate auds  = Audio $ Concatenate [ (1, toRawdio aud) | aud <- auds ]

-- | Stacks the channels of two audio files together.
merge :: Audio f c1 -> Audio f c2 -> Audio f (c1 + c2)
merge (Audio x) (Audio y) = Audio $ Merge [(1, x), (1, y)]

-- | Mixes audio files together channel-wise.
mix :: (KnownNat f, KnownNat c) => [Audio f c] -> Audio f c
mix []    = silence
mix [aud] = aud
mix auds  = Audio $ Mix [ (1, toRawdio aud) | aud <- auds ]

-- | An empty audio file, of zero length.
silence :: (KnownNat f, KnownNat c) => Audio f c
silence = let res = Audio $ Silence (frequency res) (channels res) in res

-- | Assigns an already-known sample rate and channel count to an existing file.
unsafeFile :: FilePath -> Audio f c
unsafeFile = Audio . File

-- | Resamples an audio file from one frequency to another.
resample :: (KnownNat f2) => Audio f1 c -> Audio f2 c
resample (Audio x) = let res = Audio $ Resample x $ frequency res in res

-- | Pads one end of the audio with silence.
pad :: Side -> Time -> Audio f c -> Audio f c
pad side len = Audio . Pad    side len . toRawdio

-- | Trims a chunk of time off one end of the audio.
trim :: Side -> Time -> Audio f c -> Audio f c
trim side len = Audio . Trim   side len . toRawdio

-- | Cuts off everything after a length of time (counted from either end).
cutoff :: Side -> Time -> Audio f c -> Audio f c
cutoff side len = Audio . Cutoff side len . toRawdio

-- | Gets the sample rate stored in the audio file's type.
frequency :: (KnownNat f) => Audio f c -> Integer
frequency = natVal . (undefined :: Audio f c -> Proxy f)

-- | Gets the number of channels stored in the audio file's type.
channels :: (KnownNat c) => Audio f c -> Integer
channels = natVal . (undefined :: Audio f c -> Proxy c)

-- | Runs SoX with the given list of arguments. An exception will be raised
-- if SoX returns a non-zero error code.
runSox :: [String] -> IO ()
runSox = callProcess "sox"

runRawdio
  :: FilePath -- ^ A directory for temporary files.
  -> Rawdio -- ^ Audio expression to evaluate.
  -> IO FilePath
runRawdio tempdir = go where

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
    Pad    Front len aud -> transform aud ["pad", showTime len]
    Pad    Back  len aud -> transform aud ["pad", "0", showTime len]
    Trim   Front len aud -> transform aud ["trim", showTime len]
    Trim   Back  len aud -> transform aud ["trim", "0", '-' : showTime len]
    Cutoff Front len aud -> transform aud ["trim", "0", showTime len]
    Cutoff Back  len aud -> transform aud ["trim", '-' : showTime len]

  showTime :: Time -> String
  showTime (Seconds d) = show d
  showTime (Samples d) = show d ++ "s"

  transform :: Rawdio -> [String] -> IO FilePath
  transform aud args = do
    input <- go aud
    output <- new
    runSox $ input : output : args
    return output

  combine :: String -> [(Double, Rawdio)] -> IO FilePath
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
