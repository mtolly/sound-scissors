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
import Data.Maybe
import System.Process (callProcess, readProcess)

-- | An audio expression, typed by sample rate and number of channels.
newtype Audio (rate :: Nat) (chans :: Nat)
  = Audio { toRawdio :: Rawdio }
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

data Side = Front | Back
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Time
  = Seconds Double
  | Samples Double
  deriving (Eq, Ord, Show, Read)

unsafeFromRawdio :: Rawdio -> Audio c f
unsafeFromRawdio = Audio

-- | Glues audio files together sequentially.
concatenate :: (KnownNat r, KnownNat c) => [Audio r c] -> Audio r c
concatenate []    = silence
concatenate [aud] = aud
concatenate auds  = Audio $ Concatenate [ (1, toRawdio aud) | aud <- auds ]

-- | Stacks the channels of two audio files together.
merge :: Audio r c1 -> Audio r c2 -> Audio r (c1 + c2)
merge (Audio x) (Audio y) = Audio $ Merge [(1, x), (1, y)]

-- | Mixes audio files together channel-wise.
mix :: (KnownNat r, KnownNat c) => [Audio r c] -> Audio r c
mix []    = silence
mix [aud] = aud
mix auds  = Audio $ Mix [ (1, toRawdio aud) | aud <- auds ]

-- | An empty audio file, of zero length.
silence :: (KnownNat r, KnownNat c) => Audio r c
silence = let res = Audio $ Silence (sampleRate res) (channels res) in res

-- | Assigns an already-known sample rate and channel count to an existing file.
unsafeFile :: FilePath -> Audio r c
unsafeFile = Audio . File

-- | Resamples an audio file from one rate to another.
resample :: (KnownNat f2) => Audio f1 c -> Audio f2 c
resample (Audio x) = let res = Audio $ Resample x $ sampleRate res in res

-- | Pads one end of the audio with silence.
pad :: Side -> Time -> Audio r c -> Audio r c
pad side len = Audio . Pad    side len . toRawdio

-- | Trims a chunk of time off one end of the audio.
trim :: Side -> Time -> Audio r c -> Audio r c
trim side len = Audio . Trim   side len . toRawdio

-- | Cuts off everything after a length of time (counted from either end).
cutoff :: Side -> Time -> Audio r c -> Audio r c
cutoff side len = Audio . Cutoff side len . toRawdio

-- | Gets the sample rate stored in the audio file's type.
sampleRate :: (KnownNat r) => Audio r c -> Integer
sampleRate = natVal . (undefined :: Audio r c -> Proxy r)

-- | Gets the number of channels stored in the audio file's type.
channels :: (KnownNat c) => Audio r c -> Integer
channels = natVal . (undefined :: Audio r c -> Proxy c)

-- | Runs SoX with the given list of arguments. An exception will be raised
-- if SoX returns a non-zero error code.
runSox :: [String] -> IO ()
runSox = callProcess "sox"

getRate :: FilePath -> IO Integer
getRate f = fmap read $ readProcess "soxi" ["-r", f] ""

getChannels :: FilePath -> IO Integer
getChannels f = fmap read $ readProcess "soxi" ["-c", f] ""

file :: (KnownNat r, KnownNat c) => FilePath -> IO (Maybe (Audio r c))
file fp = do
  r <- getRate fp
  c <- getChannels  fp
  let res = guard (r == sampleRate aud && c == channels aud)
        >> Just (unsafeFile fp)
      aud = fromJust res
  return res

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
    Silence rate chans -> do
      output <- new
      runSox
        [ "-n"
        , output
        , "trim", "0", "0"
        , "rate", show rate
        , "channels", show chans
        ]
      return output
    File fp -> return fp
    Concatenate auds -> combine "concatenate" auds
    Merge auds -> combine "merge" auds
    Mix auds -> combine "mix" auds
    Resample aud rate -> do
      input <- go aud
      output <- new
      runSox [input, output, "rate", show rate]
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
