{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE TypeOperators  #-}
{- |
A functional, type-safe interface to slicing up audio files with SoX.
-}
module Sound.Scissors

( Audio
, Rawdio(..)
, Side(..)
, Time(..)
, toRawdio
, fromRawdio
, fromRawdio'
, unsafeFromRawdio

-- * Basic audio primitives
, silence
, resample
, file
, file'
, unsafeFile

-- * Combining multiple audio files
, concatenate
, merge
, mix
, concatenate'
, merge'
, mix'

-- * Editing a single audio file
, pad
, trim
, cutoff
, vol

-- * Querying sample rate and channel count
, sampleRate
, channels
, getSampleRate
, getChannels

-- * Writing audio to a file
, runAudio
, runRawdio
, runRawdioIn

) where

import           Control.Arrow    (second)
import           Control.Monad    (forM)
import           Data.Char        (toLower)
import           Data.Proxy       (Proxy (..))
import qualified Data.Traversable as T
import           GHC.TypeLits     (KnownNat, Nat, natVal, (+)())
import           System.Directory (copyFile)
import           System.FilePath  (takeExtension)
import           System.IO        (hClose)
import           System.IO.Temp   (openTempFile, withSystemTempDirectory)
import           System.Process   (callProcess, readProcess)

-- | An audio expression, typed by sample rate and number of channels.
newtype Audio (r :: Nat) (c :: Nat) = Audio
  { toRawdio :: Rawdio
  -- ^ Removes the extra type information, leaving a raw audio expression.
  } deriving (Eq, Ord, Show, Read)

-- | A raw audio expression.
data Rawdio
  = Silence     Integer Integer -- ^ Sample rate & channel count.
  | File        FilePath
  | Pad         Side Time Rawdio
  | Trim        Side Time Rawdio
  | Cutoff      Side Time Rawdio
  | Concatenate [(Double, Rawdio)]
  | Merge       [(Double, Rawdio)]
  | Mix         [(Double, Rawdio)]
  | Resample    Rawdio Integer
  deriving (Eq, Ord, Show, Read)

-- | Which end of the audio to apply certain operations to.
data Side = Front | Back
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A length of time to cut or add.
data Time
  = Seconds Double
  | Samples Double
  deriving (Eq, Ord, Show, Read)

-- | Assigns an already-known sample rate and channel count to an expression.
unsafeFromRawdio :: Rawdio -> Audio r c
unsafeFromRawdio = Audio

-- | Glues audio files together sequentially. Can also adjust volumes.
concatenate' :: (KnownNat r, KnownNat c) => [(Double, Audio r c)] -> Audio r c
concatenate' []    = silence
concatenate' vauds = Audio $ Concatenate $ map (second toRawdio) vauds

-- | Glues audio files together sequentially.
concatenate :: (KnownNat r, KnownNat c) => [Audio r c] -> Audio r c
concatenate = concatenate' . map (1,)

-- | Stacks the channels of two audio files together. Can also adjust volumes.
merge' :: (Double, Audio r c1) -> (Double, Audio r c2) -> Audio r (c1 + c2)
merge' (a, Audio x) (b, Audio y) = Audio $ Merge [(a, x), (b, y)]

-- | Stacks the channels of two audio files together.
merge :: Audio r c1 -> Audio r c2 -> Audio r (c1 + c2)
merge x y = merge' (1, x) (1, y)

-- | Mixes audio files together channel-wise. Can also adjust volumes.
mix' :: (KnownNat r, KnownNat c) => [(Double, Audio r c)] -> Audio r c
mix' []    = silence
mix' vauds = Audio $ Mix $ map (second toRawdio) vauds

-- | Mixes audio files together channel-wise.
mix :: (KnownNat r, KnownNat c) => [Audio r c] -> Audio r c
mix = mix' . map (1,)

-- | Multiplies the volume of the audio by a scalar.
vol :: Double -> Audio r c -> Audio r c
vol v (Audio x) = Audio $ Concatenate [(v, x)]

-- | An empty audio file, of zero length.
silence :: (KnownNat r, KnownNat c) => Audio r c
silence = let res = Audio $ Silence (sampleRate res) (channels res) in res

-- | Assigns an already-known sample rate and channel count to an existing file.
unsafeFile :: FilePath -> Audio r c
unsafeFile = Audio . File

-- | Resamples an audio file from one rate to another.
resample :: (KnownNat r2) => Audio r1 c -> Audio r2 c
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

-- | Gets the sample rate stored in the audio expression's type.
sampleRate :: (KnownNat r) => Audio r c -> Integer
sampleRate = natVal . (undefined :: Audio r c -> Proxy r)

-- | Gets the number of channels stored in the audio expression's type.
channels :: (KnownNat c) => Audio r c -> Integer
channels = natVal . (undefined :: Audio r c -> Proxy c)

-- | Runs SoX with the given list of arguments. An exception will be raised
-- if SoX returns a non-zero error code.
sox :: [String] -> IO ()
sox = callProcess "sox"

-- | Calculates the sample rate of a raw audio expression.
-- Any files referenced will be queried with @soxi@.
getSampleRate :: Rawdio -> IO Integer
getSampleRate raw = case raw of
  Silence r _ -> return r
  File f -> fmap read $ readProcess "soxi" ["-r", f] ""
  Pad    _ _ raw' -> getSampleRate raw'
  Trim   _ _ raw' -> getSampleRate raw'
  Cutoff _ _ raw' -> getSampleRate raw'
  Concatenate pairs -> getSampleRate $ snd $ head pairs
  Merge       pairs -> getSampleRate $ snd $ head pairs
  Mix         pairs -> getSampleRate $ snd $ head pairs
  Resample _ r -> return r

-- | Calculates the channel count of a raw audio expression.
-- Any files referenced will be queried with @soxi@.
getChannels :: Rawdio -> IO Integer
getChannels raw = case raw of
  Silence _ c -> return c
  File f -> fmap read $ readProcess "soxi" ["-c", f] ""
  Pad    _ _ raw' -> getChannels raw'
  Trim   _ _ raw' -> getChannels raw'
  Cutoff _ _ raw' -> getChannels raw'
  Concatenate pairs -> getChannels $ snd $ head pairs
  Merge       pairs -> fmap sum $ mapM (getChannels . snd) pairs
  Mix         pairs -> getChannels $ snd $ head pairs
  Resample _ r -> return r

-- | Checks that the sample rate and channel count of the expression
-- match the type it is being cast to. If not, returns 'Nothing'.
fromRawdio :: (KnownNat r, KnownNat c) => Rawdio -> IO (Either String (Audio r c))
fromRawdio raw = do
  r <- getSampleRate raw
  c <- getChannels raw
  let res = if r == sampleRate aud && c == channels aud
        then Right $ unsafeFromRawdio raw
        else Left $ unwords
          [ "Expected sample rate"
          , show $ sampleRate aud
          , "and channel count"
          , show $ channels aud
          , "but got"
          , show r
          , "and"
          , show c
          ]
      aud = (undefined :: Either a b -> b) res
  return res

-- | Checks that the sample rate and channel count of the file
-- match the type it is being cast to. If not, returns 'Nothing'.
file :: (KnownNat r, KnownNat c) => FilePath -> IO (Either String (Audio r c))
file = fromRawdio . File

-- | Like 'fromRawdio', but raises an error if the rate or count do not match.
fromRawdio' :: (KnownNat r, KnownNat c) => Rawdio -> IO (Audio r c)
fromRawdio' raw = fromRawdio raw >>=
  either (\e -> error $ "fromRawdio': " ++ e) return

-- | Like 'file', but raises an error if the rate or count do not match.
file' :: (KnownNat r, KnownNat c) => FilePath -> IO (Audio r c)
file' = fromRawdio' . File

-- | Writes a typed audio expression to a file.
runAudio
  :: Audio r c -- ^ Audio expression to evaluate.
  -> FilePath -- ^ Output WAV file to generate.
  -> IO ()
runAudio = runRawdio . toRawdio

-- | Writes a raw audio expression to a file.
runRawdio
  :: Rawdio -- ^ Audio expression to evaluate.
  -> FilePath -- ^ Output WAV file to generate.
  -> IO ()
runRawdio raw out = withSystemTempDirectory "scissors" $ \tempdir -> do
  f <- runRawdioIn tempdir raw
  copyFile f out

-- | Writes a raw audio expression to a new file in a temporary directory.
runRawdioIn
  :: FilePath -- ^ A directory for temporary files.
  -> Rawdio -- ^ Audio expression to evaluate.
  -> IO FilePath -- ^ The output file, which will be in the temporary directory.
runRawdioIn tempdir = go where

  new :: IO FilePath
  new = do
    (fp, h) <- openTempFile tempdir "scissors.wav"
    hClose h
    return fp

  go x = case x of
    Silence rate chans -> do
      output <- new
      sox
        [ "-n"
        , output
        , "trim", "0", "0"
        , "rate", show rate
        , "channels", show chans
        ]
      return output
    File input -> do
      output <- new
      case map toLower $ takeExtension input of
        ".mp3" -> callProcess "lame" ["--decode", input, output]
        _      -> sox [input, output]
      return output
    Concatenate auds -> combine "concatenate" auds
    Merge auds -> combine "merge" auds
    Mix auds -> combine "mix" auds
    Resample aud rate -> do
      input <- go aud
      output <- new
      sox [input, output, "rate", show rate]
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
    sox $ input : output : args
    return output

  combine :: String -> [(Double, Rawdio)] -> IO FilePath
  combine _ [] = error "runAudio: can't combine 0 audio files"
  combine _ [(v, aud)] = do
    input <- go aud
    output <- new
    sox ["-v", show v, input, output]
    return output
  combine method vauds = do
    inputs <- forM vauds $ T.mapM go
    output <- new
    sox
      $ ["--combine", method]
      ++ (inputs >>= \(v, f) -> ["-v", show v, f])
      ++ [output]
    return output
