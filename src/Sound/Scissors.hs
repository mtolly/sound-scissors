{-# LANGUAGE
  DataKinds
  , KindSignatures
  , GADTs
  , TypeOperators
  , TupleSections
  #-}
module Sound.Scissors where

import GHC.TypeLits
import Data.Proxy
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import Control.Monad

data Audio'
  = Silence Integer Integer
  | File String
  | Concatenate [(Double, Audio')]
  | Merge [(Double, Audio')]
  | Mix [(Double, Audio')]
  | Resample Audio' Integer
  deriving (Eq, Ord, Show, Read)

newtype Audio (freq :: Nat) (chans :: Nat)
  = Audio { rawAudio :: Audio' }
  deriving (Eq, Ord, Show, Read)

concatenate :: [Audio f c] -> Audio f c
concatenate = Audio . Concatenate . map ((1,) . rawAudio)

merge :: Audio f c1 -> Audio f c2 -> Audio f (c1 + c2)
merge (Audio x) (Audio y) = Audio $ Merge [(1, x), (1, y)]

mix :: [Audio f c] -> Audio f c
mix = Audio . Mix . map ((1,) . rawAudio)

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
runSox = undefined

tempFile :: FilePath -> String -> IO FilePath
tempFile dir pat = do
  (fp, h) <- openTempFile dir pat
  hClose h
  return fp

runAudio :: FilePath -> Audio' -> IO FilePath
runAudio tempdir = go where
  go x = case x of
    Silence freq chans -> do
      temp <- tempFile tempdir "scissors.wav"
      runSox
        [ "-n"
        , temp
        , "trim", "0", "0"
        , "channels", show chans
        , "rate", show freq
        ]
      return temp
    File fp -> return fp
    Concatenate auds -> if null auds
      then error "runAudio: can't concatenate 0 audio files"
      else do
        inputs <- forM auds $ \(v, aud) -> do
          f <- go aud
          return (v, f)
        temp <- tempFile tempdir "scissors.wav"
        runSox $ (inputs >>= \(vel, f) -> ["-v", show vel, show f]) ++ [temp]
        return temp
    Merge auds -> undefined
    Mix auds -> if null auds
      then error "runAudio: can't concatenate 0 audio files"
      else do
        inputs <- forM auds $ \(v, aud) -> do
          f <- go aud
          return (v, f)
        temp <- tempFile tempdir "scissors.wav"
        runSox
          $ ["--combine", "mix"]
          ++ (inputs >>= \(vel, f) -> ["-v", show vel, show f])
          ++ [temp]
        return temp
    Resample aud freq -> undefined
