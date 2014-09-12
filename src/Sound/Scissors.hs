{-# LANGUAGE
  DataKinds
  , KindSignatures
  , GADTs
  , TypeOperators
  #-}
module Sound.Scissors where

import GHC.TypeLits
import Data.Proxy

data Audio'
  = Silence Integer Integer
  | File String
  | Concatenate [Audio']
  | Merge [Audio']
  | Mix [Audio']
  | Resample Audio' Integer
  deriving (Eq, Ord, Show, Read)

newtype Audio (freq :: Nat) (chans :: Nat)
  = Audio { rawAudio :: Audio' }
  deriving (Eq, Ord, Show, Read)

concatenate :: [Audio f c] -> Audio f c
concatenate = Audio . Concatenate . map rawAudio

merge :: Audio f c1 -> Audio f c2 -> Audio f (c1 + c2)
merge (Audio x) (Audio y) = Audio $ Merge [x, y]

mix :: [Audio f c] -> Audio f c
mix = Audio . Mix . map rawAudio

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
